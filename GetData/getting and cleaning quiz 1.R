#quiz 1 getting and cleaning

url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

getwd()
setwd("C:/Users/Jason/Documents/datasciencecourse")
download.file(url,"acs.csv")

acs<- read.csv("acs.csv")

names(acs)
head(acs)
acs.idaho <- subset(acs,ST=="16" & VAL==24)

nrow(acs.idaho)  #53


#Needed to do this in 32-bit R.
library(xlsx)
#Read rows 18-23 and columns 7-15 into R and assign the result to a variable called:
dat<- read.xlsx("gas.xlsx",1,startRow=18,endRow=23)[7:15]
dat
sum(dat$Zip*dat$Ext,na.rm=T) 
#36534720


xml.url<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
download.file(xml.url,"balto.xml")

#How many restaurants have zipcode 21231?
library(XML)
balto<- xmlTreeParse("balto.xml",useInternalNodes=T)
summary(balto)
balto.root<-xmlRoot(balto)
names(balto.root)

sum(xpathSApply(balto.root,"//zipcode",xmlValue)=="21231")
#127



library(data.table)

acs2<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(acs2,"acs2.csv")
DT<- fread("acs2.csv",sep=",")

system.time(DT[,mean(pwgtp15),by=SEX])
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
nrow(DT)
