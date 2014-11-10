d <- readPollutionFiles("specdata")

summary(d)

sapply(1:4,function(x) sum(complete.cases(subset(d,ID==x))))

comp <- complete("specdata",c(4:7,99:102))



source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript1.R")
submit()


threshold=1000
monitors <- subset(comp, nobs > threshold)$id
monitors

cor(d$sulfate,d$nitrate,use="complete.obs")

sapply(monitors, function(x){
    dd<-subset(d,ID==x)
    return(cor(dd$sulfate,dd$nitrate,use="complete.obs"))
}
                    )



cr<- corr("specdata",150)
summary(cr)
