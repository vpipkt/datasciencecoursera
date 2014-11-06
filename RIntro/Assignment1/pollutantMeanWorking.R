## pollutant mean working stuff

#Test area

pollutantmean("specdata","s",1:10)
#4.064
pollutantmean("specdata","nitrate",70:72)
#1.706
pollutantmean("specdata/","nitrate",23)
#1.281



#work area. delete this before submitting
drr <- "C:/users/jbrown/source/repos/datasciencecoursera/rintro/assignment1/specdata/"

substr(drr, nchar(drr),nchar(drr)) == "/"



filenames<-paste(drr,sprintf("%03d",1:4),".csv",sep="")
filenames
read.csv(filenames)
thing<-data.frame(Date=structure(numeric(0),class="Date"),sulfate=numeric(0),nitrate=numeric(0),ID=numeric(0))
four <- lapply(filenames, function(x) thing<-rbind(thing,read.csv(x)) )
nrow(four)
summary(four)
nrow(four[[4]])
#this doesn't work quite well, it gives a list of data frames.

#easiest way to do this is just loop over id argument passed in
thing<-data.frame(Date=structure(numeric(0),class="Date"),sulfate=numeric(0),nitrate=numeric(0),ID=numeric(0))
for (filenum in 1:4){
  path<- paste(drr,sprintf("%03d",filenum),".csv",sep="")
  thing<-rbind(thing,
               read.csv( path,colClasses=c("Date",rep("numeric",3))))
  
}

summary(thing)
table(thing$ID)

columnIWant <- "s"
mean(thing[[columnIWant,exact=F]],na.rm=T)

thing[["s"]]
