## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

library(ggplot2)
library(plyr)
balto <- dcast(melt(NEI,measure.vars="Emissions"), year + type ~ variable, sum, 
               subset=.(fips=="24510"))

png("plot3.png")
g<-ggplot(balto,aes(year,Emissions))
g + geom_line() + geom_point() + facet_grid(type~.)
dev.off()

#POINT increased, others decreased