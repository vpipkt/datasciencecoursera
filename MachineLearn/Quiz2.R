#practical machine learn quiz 2
#Question 1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]


#Question 2

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

plot(training$CompressiveStrength , col=cut(training$FlyAsh,5))


plot(FlyAsh ~ CompressiveStrength, training)

plot(training$CompressiveStrength , col=cut(training[,1],4))
plot(training$CompressiveStrength , col=cut(training[,2],4))
plot(training$CompressiveStrength , col=cut(training[,3],4))
plot(training$CompressiveStrength , col=cut(training[,4],4))
plot(training$CompressiveStrength , col=cut(training[,5],4))
plot(training$CompressiveStrength , col=cut(training[,6],4))
plot(training$CompressiveStrength , col=cut(training[,7],4))
plot(training$CompressiveStrength , col=cut(training[,8],4))


# QUESTION 3

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]


hist(training$Superplasticizer)
sum(training$Superplasticizer<=0)
nrow(training)
hist(training$Superplasticizer+1)


#Question 4 looks familiar
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

ILS <- grep("^IL.",names(training))
count(ILS)

q4 <- preProcess(training[,ILS],method="pca",thresh=0.8)
q4


### QUESTION 5 
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
#Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis. Build two predictive models, one using the predictors as they are and one using PCA with principal components explaining 80% of the variance in the predictors. Use method="glm" in the train function. What is the accuracy of each method in the test set? Which is more accurate?

library(e1071)
ILS <- grep("^IL.",names(training))
trainq5 <- training[, c(1,ILS)]
testq5 <- testing[, c(1,ILS)]

pcaProc <- preProcess(trainq5[,-1], method = "pca", thresh = 0.8)
trainPca <- predict(pcaProc, trainq5[,-1])
modelPca <- train(trainq5$diagnosis ~ ., method="glm", data = trainPca)
testPca <- predict(pcaProc, testq5[,-1])
confusionMatrix(testq5$diagnosis, predict(modelPca, testPca))


#now non-pCA version
model <- train(trainq5$diagnosis ~ ., method="glm", data = trainq5[,-1])
confusionMatrix(testq5$diagnosis, predict(model, testq5[,-1]))
