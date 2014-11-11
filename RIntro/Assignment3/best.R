

best <- function(state, outcome, filename="outcome-of-care-measures.csv") {
    ## Read outcome data
    outcome.df <- read.csv(filename,colClasses="character")
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
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    outcome.df<- subset(outcome.df, State==state)
    if(outcome=="heart attack"){
        outcome.df <- outcome.df[,c(2,11)]
    }else if(outcome=="heart failure"){
        outcome.df <- outcome.df[,c(2,17)]
    } else if(outcome=="pneumonia"){
        outcome.df<- outcome.df[,c(2,23)]
    }
    
    #convert measure to numeric
    outcome.df[,2] <- as.numeric(outcome.df[,2])
    
    #sort
    best.row <- order(outcome.df[2],outcome.df[1])[1]
    
    #return the best one
    return(outcome.df[best.row,1])
}