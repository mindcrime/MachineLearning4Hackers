# TODO: Add comment
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
ufo$DateReported <- as.Date(ufo$Reported, format="%Y%m%d")

head(ufo)

