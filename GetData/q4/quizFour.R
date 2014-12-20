## QUIZ FOUR 

setwd("GetData/q4")

# ================== Queastion 1 ===========
#Apply strsplit() to split all the names of the data frame on the characters "wgtp". What is the value of the 123 element of the resulting list?

acs.url<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
acs.file<-"acs.csv"
download.file(acs.url,acs.file)
acs<-read.csv(acs.file)
?strsplit
strsplit(names(acs),"wgtp")[[123]]


# ---------------------Q uestion 2-------------

#familiar troublesome gdp datasets
gdp.url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
gdp.file<-"gdp.csv"
download.file(gdp.url,gdp.file)
gdp<-read.csv(gdp.file,skip=5,header=F,colClasses="character")
gdp$V3 <- gdp$V7 <- gdp$V8 <- gdp$V9 <- gdp$V10 <- NULL
names(gdp) <- c("shortcode","rank","name","gdpUSDMM","footnote")
gdp$rank<- as.numeric(gdp$rank)
#NOTE THE CLAUSE IN THE QUESTION: FOR THE 190 RANKED COUNTRIES
gdp<-subset(gdp,rank<=190)
head(gdp,10)
plot(gdp$rank)

#average of the gdp
gdp$gdpUSDMM <- gsub(",","",gdp$gdpUSDMM)
gdp$gdpUSDMM

mean(as.numeric(gdp$gdpUSDMM))

# ---------- QUESTION THREEE ----------

countryNames<- gdp$name
grep("United$",countryNames)
countryNames[grep("*United",countryNames)]
countryNames[grep("^United",countryNames)]
countryNames[grep("^United",countryNames)]

# -----  QUenstion FOUR -------------
## get the education data
ed.url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
ed.file<-"ed.csv"
download.file(ed.url,ed.file)
ed<- read.csv(ed.file, colClasses="character")
head(ed)
tail(ed)

q4 <- merge(gdp,ed,by.y="CountryCode",by.x="shortcode",all=F)
head(q4$Special.Notes,10)
#find "fiscal" or "fy" and "June" or "6?"
q4$Special.Notes[grep("[Ff]iscal",q4$Special.Notes)]
q4$Special.Notes[grep("end: [Jj]un",q4$Special.Notes)]


# ================== Question the fifth ===== 

#but don't plead it

download.package('quantmod')
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn) 

#How many values were collected in 2012? How many values were collected on Mondays in 2012

class(sampleTimes)

sampleTimes.lt<-as.POSIXlt(sampleTimes)


sampleTimes[sampleTimes.lt$year==112]

list(y2012 = sum(sampleTimes.lt$year==112),
 mondays = sum(sampleTimes.lt$wday==1 & sampleTimes.lt$year==112))
#0 is sunday
