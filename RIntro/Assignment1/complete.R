#Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the id of the file and the second column is the number of complete cases. A prototype of this function follows

#question: is the id to return numeric type or char?

complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

	data <- readPollutionFiles(directory,id)
	
	#compute the complete cases per id
    return(data.frame(id=id,
                      nobs = sapply(id, 
                                    function(x) sum(complete.cases(subset(d,ID==x))))))
}
