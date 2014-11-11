outcome<- read.csv("Rintro/assignment3/outcome-of-care-measures.csv",
                   colClasses="character")
head(outcome)
names(outcome)
nrow(outcome)

hist(as.numeric(outcome[,11]))


best("Virginia")
best("VA","ebola")
best("VA","heart failure")


best("TX", "heart attack")
best("MD", "pneumonia")




outcome.df <- read.csv("outcome-of-care-measures.csv",colClasses="character")
## Check that state and outcome are valid

if(! state %in% unique(outcome.df$State)){
    stop("invalid state")
    return(NULL)
}

if(! outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
    #Must be one of 'heart attack', 'heart failure', or 'pneumonia'.")
    return(NULL)    
}

#subset outcome data of interest
outcome.df<- subset(outcome.df, State=="state"NC"")
if(outcome=="heart attack"){
    outcome.df <- outcome.df[,c(2,11)]
}else if(outcome=="heart failure"){
    outcome.df <- outcome.df[,c(2,17)]
} else if(outcome=="pneumonia"){
    outcome.df<- outcome.df[,c(2,23)]
}



#convert measure to numeric
outcome.df[,2] <- as.numeric(outcome.df[,2])

nrow(outcome.df)
outcome.df[is.na(outcome.df[,2]),] <- NULL

#sort
ranking <- order(outcome.df[2],outcome.df[1])
#clean NAs out of ranking
ranking <- ranking[seq_len(sum(!is.na(outcome.df[ranking,2])))]

## Return hospital name in that state with the given rank
## 30-day death rate

returnRow <- NA

if (num == "best")
    returnRow <- 1
else if(num == "worst" )
    returnRow <- length(ranking)
else
    returnRow <- as.numeric(num)

return(outcome.df[ranking[returnRow],])
