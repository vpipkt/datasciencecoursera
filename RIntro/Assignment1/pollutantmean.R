#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector of monitor ID numbers, 'pollutantmean' reads those monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. A prototype of the function is as follows

readPollutionFiles <- function(directory, id=1:332){
  # Get the files read into a single data frame.
  # first check if directory ends with / 
  if(substr(directory, nchar(directory),nchar(directory))!="/"){
    directory <- paste(directory,"/",sep="")
  }
  data <-data.frame(Date=structure(numeric(0),class="Date"),sulfate=numeric(0),nitrate=numeric(0),ID=numeric(0))
  
  for (filenum in id){
    path<- paste(directory,sprintf("%03d",filenum),".csv",sep="")
    data<-rbind(data,
                read.csv( path,colClasses=c("Date",rep("numeric",3))))
  }
  return(data)
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
  
  
  
        data <- readPollutionFiles(directory,id)
        
        return(mean(data[[pollutant,exact=F]],na.rm=T))
  
}

