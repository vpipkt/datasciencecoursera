setwd('~/source/repos/datasciencecoursera/machinelearn/project')

trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
trainDir <- "data/pml-training.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
#testDir <- "data/pml-testing.csv"

download.file(trainUrl, trainDir)
#download.file(testUrl, testDir)

#need to set "#DIV/O!" as a NA
training <- read.csv(trainDir, na.strings = c("NA","#DIV/0!"))
names(training)

#first field looks like a row name
summary(training$X - 1:nrow(training))
training$X <- NULL
    
#not sure what these are
summary(training$new_window)
summary(training$num_window)

summary(training[,20:40])
str(training)

library(caret)
modRF <- train(classe ~ ., data = training, method="rf")
# fails due to missing values, every row has >0 missing value


mean(complete.cases(training))
plot(rowMeans(is.na(training)))

plot(
    colMeans(is.na(training))
)
names(missCols)[missCols]

#eliminate any variable with more than 90% missing data, first creating a new column denoting missingness
training$missingness <- rowMeans(is.na(training[, 
                                colMeans(is.na(training)) > 0.9]))
training <- training[, colMeans(is.na(training)) <= 0.9]


library(doParallel); 
#run model in parallel
cl <- makeCluster(detectCores())
registerDoParallel(cl)

#try random forest again (note this will have seed control issues http://stackoverflow.com/questions/13403427/fully-reproducible-parallel-models-using-caret )
tc <- trainControl(method = 'cv', number = 10)
modRF <- train(classe ~ ., data = training, method="rf", trControl = tc)
modRF
#save(modRF, file = 'data/modRF.Rdata')
varImp(modRF)

#why are several timestamps so important? Possible that the 
plot(jitter(as.numeric(classe)) ~ jitter(raw_timestamp_part_1, amount = 1e2), training)
table( training$raw_timestamp_part_1, training$classe)

#other timestamps (or levels of them) were important too...
plot(classe ~ cvtd_timestamp , data = training)
# there is a definite repetitive structure here which has been exploited in training.