setwd("GetData")

# ===============QUESTION 1 ===================
acs.url<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
acs.filename <- "acs.csv"
download.file(acs.url, acs.filename)

acs<- read.csv(acs.filename)
summary(acs)

which(acs$ACR == 3 & acs$AGS == 6)[1:3]


# =================QUESTION 2 ===========================

library(jpeg)
jeff <- readJPEG("getdata-jeff.jpg",native=T) 
summary(jeff)
quantile(as.matrix(jeff), probs=c(.3,.8))

# ===============QUESTION 3 ============================

gdp.url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
gdp.file<-"gdp.csv"
download.file(gdp.url,gdp.file)
gdp<-read.csv(gdp.file,skip=5,header=F)
summary(gdp)

#shortcode, rank, <> ,name, $$MM, footnote, <><><>

gdp$V3 <- gdp$V7 <- gdp$V8 <- gdp$V9 <- gdp$V10 <- NULL
names(gdp) <- c("shortcode","rank","name","gdpUSDMM","footnote")
head(gdp,10)
summary(gdp)

#gdp field itself is an absolute wreck.
#just need the rank to sort on
gdp$rank<- as.numeric(as.character(gdp$rank))
head(gdp,10)
plot(gdp$rank)

#NOTE THE CLAUSE IN THE QUESTION: FOR THE 190 RANKED COUNTRIES
gdp<-subset(gdp,rank<=190)
nrow(gdp)

## get the education data
ed.url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
ed.file<-"ed.csv"
download.file(ed.url,ed.file)
ed<- read.csv(ed.file)
head(ed)
tail(ed)

q3 <- merge(gdp,ed,by.y="CountryCode",by.x="shortcode",all=F)

list(matches = nrow(q3),
     thirteenth = q3[order(q3$rank,decreasing=T),"Table.Name"][1:13])


# ============= QUESTION 4 =================
#What is the average GDP ranking for the "High income: OECD" and "High income: nonOECD" group?

table(q3$rank,q3$Income.Group)
aggregate(rank ~ Income.Group, q3, mean )


# ========== QUESTION 5 ============================

q3$rank.quart <- cut(q3$rank,5)
plot(q3$rank.quart)
table(q3$rank.quart,q3$Income.Group)
