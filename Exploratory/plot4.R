## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

library(reshape2)
library(ggplot2)
library(plyr)

coal.comb<- grep("Comb.*[C|c]oal",SCC$Short.Name)
coal.SCC<-SCC$SCC[coal.comb]
coal <- dcast(melt(NEI,measure.vars="Emissions"), year  ~ variable, sum, 
               subset=.(SCC %in% coal.SCC))

png("plot4.png")
qplot(year,Emissions,data=coal, geom=c("point","line")) + ylab("Coal combustion emissions, tons")
dev.off()
