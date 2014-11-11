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
