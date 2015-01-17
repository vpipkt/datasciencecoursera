## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

library(reshape2)
library(plyr)
library(ggplot2)
balto <- dcast(melt(NEI,measure.vars="Emissions"), year  ~ variable, sum, 
               subset=.(fips=="24510" & type=="ON-ROAD"))

png("plot5.png")
p<- ggplot(balto, aes(year,Emissions))
p + geom_line() + geom_point() + ylab("Baltimore On Road Emissions")
dev.off()
