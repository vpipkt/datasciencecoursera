#for convenience, return the desired data frame row from a split 
#assumes NAs already cleaned out
rankOne <- function(df, num="best"){
    
    ranking <- order(df[3],df[2]) #rank by mortality then name
        
    ## Return hospital name in the data frame with the given rank
    ## 30-day death rate
    
    returnRow <- NA
    
    if (num == "best")
        returnRow <- 1
    else if(num == "worst" )
        returnRow <- length(ranking)
    else
        returnRow <- as.numeric(num)
    
    #return the hospital name, name the vector with the state
    rv <- df[ranking[returnRow],2]
    # names(rv) <- df[ranking[returnRow],1]
    
    return(rv)
}


rankall <- function(outcome, num = "best", filename="outcome-of-care-measures.csv") {
    
    
    ## Read outcome data
    outcome.df <- read.csv(filename,colClasses="character")

    ## Check that outcome is valid
    if(! outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop("invalid outcome")
        #Must be one of 'heart attack', 'heart failure', or 'pneumonia'.")
        return(NULL)    
    }
    
       
    #subset outcome data of interest
    if(outcome=="heart attack"){
        outcome.df <- outcome.df[,c(7,2,11)]
    }else if(outcome=="heart failure"){
        outcome.df <- outcome.df[,c(7,2,17)]
    } else if(outcome=="pneumonia"){
        outcome.df<- outcome.df[,c(7,2,23)]
    }
    
    
    
    #convert measure to numeric
    outcome.df[,3] <- as.numeric(outcome.df[,3])
    
    #remove NAs
    outcome.df<- outcome.df[!is.na(outcome.df[,3]),] 
    
    outcome.split<-split(outcome.df,outcome.df$State)
    
    #now lapply on the list of data frames... something
    hosp <- sapply(outcome.split,rankOne,num)
    
    return(data.frame(hospital=hosp,state=names(hosp)))
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
}
