# ch1_ufo3.R
# 
# Author: prhodes
###############################################################################


library('ggplot2')

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
# head( ufo.us )

summary( ufo.us$DateOccurred)

