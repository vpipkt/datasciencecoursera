#quiz 2

library(httr)
library(jsonlite)

# request looks like
#GET /repos/:owner/:repo
#GET https://api.github.com/users/jtleek/repos/datasharing

#element created_at 


repos <- GET("https://api.github.com/users/jtleek/repos")
repos.j <- fromJSON("https://api.github.com/users/jtleek/repos")
repos.j$created_at[repos.j$name=="datasharing"]



#QUESTION 4.
l <- c(10,20,30,100)
url<-"http://biostat.jhsph.edu/~jleek/contact.html"
q4.con<- url(url)
q4<- readLines(q4.con,n=max(l))
nchar(q4[l]) #45 31 7 25
q4[l]
close(q4.con)

#question 5
q5.url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
q5.con<-url(q5.url)
readLines(q5.con,n=10)
close(q5.con)
#skip 3

download.file(q5.url,"q5.for")
q5.df <- read.fwf("q5.for",widths=c(10,9,4,9,4,9,4,9,4),
                      header=F,sep="\t",skip=4)
summary(q5.df)
sum(q5.df[[4]])
sum(q5.df$V4)
