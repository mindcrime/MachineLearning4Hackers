# ch1_ufo4_hist_seasonal.R
# 
# Author: prhodes
###############################################################################


library('ggplot2')
library( 'plyr' )
library( 'reshape' )
library(scales)

ufo <- read.delim( "/home/prhodes/development/books/MachineLearning4Hackers/ML_for_Hackers/01-Introduction/data/ufo/ufo_awesome.tsv", sep="\t", stringsAsFactors=FALSE, header=FALSE, na.strings="" )

names(ufo) <- c( "DateOccurred", "DateReported", "Location", "ShortDescription", "Duration", "LongDescription")

good.rows <- ifelse ( nchar(ufo$DateOccurred) != 8  | nchar(ufo$DateReported) != 8, FALSE, TRUE )

# writeLines( toString( length( which(!good.rows) ) ) )

ufo <- ufo[good.rows,];

ufo$DateOccurred <- as.Date(ufo$DateOccurred, format="%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported, format="%Y%m%d")

get.location <- function(l) {
	split.location <- tryCatch(strsplit(l, ",")[[1]], error=function(e) return( c(NA, NA)))
	clean.location <- gsub( "^ ", "", split.location)
	if( length(clean.location) > 2 ) {
		return( c(NA,NA))
	} else
	{
		return( clean.location)
	}
	
}

city.state <- lapply( ufo$Location, get.location)
# head( city.state )


location.matrix <- do.call( rbind, city.state)
ufo <- transform( ufo, USCity=location.matrix[,1], USState=tolower(location.matrix[,2]), stringsAsFactors=FALSE)

# head(ufo)

us.states = c( "ak", "al", "ar", "az", "ca", "co", "ct", "de", "fl", "ga", "hi", "ia", "id", "il", 
		"in", "ks", "ky", "la", "ma", "md", "me", "mi", "mn", "mo", "ms", "mt", "nc", "nd", "ne", "nh", "nj",
		"nm", "nv", "ny", "oh", "ok", "or", "pa", "ri", "sc", "sd", "tn", "tx", "ut", "va", "vt", "wa", 
		"wi", "wv", "wy")

ufo$USState <- us.states[ match( ufo$USState, us.states)]
ufo$USCity[is.na(ufo$USState)] <- NA

ufo.us <- subset( ufo, !is.na( USState))

summary( ufo.us$DateOccurred)

ufo.us <- subset( ufo.us, DateOccurred>= as.Date("1990-01-01"))


ufo.us$YearMonth <- strftime( ufo.us$DateOccurred, format="%Y-%m")

sightings.count <- ddply( ufo.us, .(USState, YearMonth), nrow)
# head( sightings.count)

dates.range <- seq.Date(from=as.Date(min(ufo.us$DateOccurred)), to=as.Date(max(ufo.us$DateOccurred)), by="month")
dates.strings <- strftime( dates.range, "%Y-%m")

states.dates <- lapply(us.states, function(s) cbind(s, dates.strings))
states.dates <- data.frame( do.call( rbind, states.dates), stringsAsFactors=FALSE)
# head( states.dates)

all.sightings <- merge( states.dates, sightings.count, by.x=c("s", "dates.strings"), by.y=c("USState", "YearMonth"), all=TRUE)
# head(all.sightings)


names( all.sightings ) <- c("State", "YearMonth", "Sightings")
all.sightings$Sightings[ is.na(all.sightings$Sightings) ] <- 0
all.sightings$YearMonth <- as.Date(rep(date.range, length(us.states)))
all.sightings$State <- as.factor( toupper( all.sightings$State ) )

head( all.sightings )

state.plot <- ggplot( all.sightings, aes(x=YearMonth,y=Sightings))+
		geom_line(aes(color="darkblue")) +
		facet_wrap(~State,nrow=10,ncol=5) +
		theme_bw() +
		scale_color_manual( values=c( "darkblue"="darkblue"), guide="none") +
		scale_x_date(breaks = "5 years", labels = date_format('%Y')) +
		xlab( "Time") + ylab("Number of Sightings") +
		ggtitle("Number of UFO sightings by Month-Year and U.S. State (1990-2010)")

print( state.plot )



#quick.hist = ggplot( ufo.us, aes(x=DateOccurred))+geom_histogram()+
#		scale_x_date(breaks = "10 years")
# ggsave(plot=quick.hist, filename="/home/prhodes/Desktop/ufo_plot.png", height=6, width=15)
# print( quick.hist )


