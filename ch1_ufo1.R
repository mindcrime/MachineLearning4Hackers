# TODO: ch1_ufo1.R
# 
# Author: prhodes
###############################################################################


library('ggplot2')

ufo <- read.delim( "/home/prhodes/development/books/MachineLearning4Hackers/ML_for_Hackers/01-Introduction/data/ufo/ufo_awesome.tsv", sep="\t", stringsAsFactors=FALSE, header=FALSE, na.strings="" )

names(ufo) <- c( "DateOccurred", "DateReported", "Location", "ShortDescription", "Duration", "LongDescription")

ufo$DateOccurred<- as.Date(ufo$DateOccurred, format="%Y%m%d")

head(ufo)

# writeLines( "\n\n\n\n")
# tail(ufo)

