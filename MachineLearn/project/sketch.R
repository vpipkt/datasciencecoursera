trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
trainDir <- "data/pml-training.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testDir <- "data/pml-testing.csv"

download.file(trainUrl, trainDir)
download.file(testUrl, testDir)

#need to set "#DIV/O!" as a NA
training <- read.csv(trainDir, na.strings = c("NA","#DIV/0!"))
names(training)

#first field looks like a row name
summary(training$X - 1:nrow(training))

#not sure what these are
summary(training$new_window)
summary(training$num_window)

summary(training[,20:40])
classes(training)
str(training)

modRF <- train(classe ~ ., data = training, method="rf")
