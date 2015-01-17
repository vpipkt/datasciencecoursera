## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

library(reshape2)
library(plyr)
library(ggplot2)
mv <- dcast(melt(NEI,measure.vars="Emissions"), year + fips ~ variable, sum, 
               subset=.(fips%in%c("24510","06037") & type=="ON-ROAD"))
mv$County<- factor(mv$fips,labels=c("Los Angeles","Baltimore"))
    
png("plot6.png")

ggplot(mv, aes(year,Emissions)) + 
    facet_grid(.~County) + 
    geom_line() + geom_point()

dev.off()

