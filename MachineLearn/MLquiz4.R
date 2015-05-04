#Machine Learning
#Quiz 4

#Question 1
library(caret)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

q1rf <- train(y ~ ., data = vowel.train, method = 'rf')
set.seed(33833)
q1gbm <- train(y ~ ., data = vowel.train, method = 'gbm', verbose = FALSE)

q1rf.p <- predict(q1rf, vowel.test)
q1gbm.p <- predict(q1gbm, vowel.test)

#confusionMatrix(vowel.test$y, q1rf.p)  #6061
#confusionMatrix(vowel.test$y, q1gbm.p) #5303
sum(vowel.test$y == q1rf.p) / nrow(vowel.test)
sum(vowel.test$y == q1gbm.p) / nrow(vowel.test)

sum(q1rf.p == q1gbm.p) / nrow(vowel.test)
# What is the accuracy among the test set samples where the two methods agree?
agree <- q1rf.p == q1gbm.p
sum(q1rf.p[agree] == vowel.test[agree,'y']) / sum(agree)


## Question 2
library(caret)
#library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
q2rf <- train(diagnosis ~ ., training, method = 'rf')
q2gbm <- train(diagnosis ~ ., training, method = 'gbm', verbose = FALSE)
q2lda <- train(diagnosis ~ ., training, method = 'lda')

#predict on test set
q2predrf  <- predict(q2rf,  testing)
q2predgbm <- predict(q2gbm, testing)
q2predlda <- predict(q2lda, testing)

#combine predictors
q2pred <- data.frame(q2predrf, q2predgbm, q2predlda, diagnosis = testing$diagnosis)
q2ComineModel <- train(diagnosis ~ ., q2pred, method = 'rf')
q2CombPred <- predict(q2ComineModel, q2pred)
summary(q2CombPred)
mean(q2CombPred == testing$diagnosis)

#Accuracy of individual predictions on the test set
mean(q2predrf  == testing$diagnosis)
mean(q2predgbm == testing$diagnosis)
mean(q2predlda == testing$diagnosis)

plot(q2C)

#### Question 3 ####
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

library(elasticnet)
?plot.enet

set.seed(233)
q3 <- train(CompressiveStrength ~ ., training, method = 'lasso')
plot(q3$finalModel, use.color=T)
q3$finalModel



#### Question 4 - blog ####

library(lubridate)  # For year() function below
download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv','blog.csv')
dat      <- read.csv("blog.csv")
training <- dat[year(dat$date) < 2012,]
testing  <- dat[(year(dat$date)) > 2011,]
tstrain  <- ts(training$visitsTumblr)

library(forecast)
?bats
q4model <- bats(tstrain)
plot(forecast(tstrain))

q4p <- forecast(q4model, h = nrow(testing), level = 95)
mean(q4p$upper > testing$visitsTumblr & testing$visitsTumblr > q4p$lower)

plot(q4p$upper, ylim = c(0,max(q4p$upper,testing$visitsTumblr)), type = 'l', col = 'red')
lines(testing$visitsTumblr)
lines(q4p$lower, col = 'red')

summary(q4model)




#### Question 5 ####
#Load the concrete data with the commands:
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
#Set the seed to 325 and fit a support vector machine using the e1071 package to predict Compressive Strength using the default settings. Predict on the testing set. What is the RMSE?
set.seed(325)
library(e1071)
q5model <- svm(CompressiveStrength ~ ., training)
q5pred  <- predict(q5model, testing)
sqrt( sum((q5pred - testing$CompressiveStrength)^2) / length(q5pred))
#yes!