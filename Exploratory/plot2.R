## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

library(reshape2)
library(plyr)
balto <- dcast(melt(NEI,measure.vars="Emissions"), year ~ variable, sum, 
               subset=.(fips=="24510"))

png("plot2.png")
plot(balto$year, balto$Emissions, type="o",
     xlab="Year",ylab="Baltimore PM2.5 Emissions")
dev.off()
