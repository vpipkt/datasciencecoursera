#Quiz3 in Practical Machine Learning

# Question 1

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

q1Train <- subset(segmentationOriginal, Case == 'Train')

nrow(q1Train)

set.seed(125)
q1Mod <- train(Class ~ ., data=q1Train, method = "rpart")
print(q1Mod$finalModel)


#### Question 3 ####

library(pgmm)

library(tree)
data(olive)
olive <- olive[,-1]
q3tree <- tree(Area ~ ., data=olive)
predict(q3tree, newdata = as.data.frame(t(colMeans(olive))))

#Note area is numeric / integer
str(olive)
summary(olive)

#misc attempts. note rpart slightly different result but same issues
library(rpart)
data(olive)
olive <- olive[,-1]

q3Model <- train(Area ~ ., data = olive, method = "rpart")
print(q3Model$finalModel)
predict(newdata = as.data.frame(t(colMeans(olive))), q3Model)

#how to really do this. area 9
olive2 <- olive
olive2$AreaF <- factor(olive2$Area)
summary(olive2)
olive2$Area <- NULL
q3Model2 <- train(AreaF ~ ., data = olive2, method="rpart" )
predict(newdata = as.data.frame(t(colMeans(olive))), q3Model2)


#### Question 4 ####

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
trainz = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[trainz,]
testSA = SAheart[-trainz,]

help(SAheart)

set.seed(13234)

q4Mod <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,
               data = trainSA, method = "glm", family = "binomial"  )
q4Mod$finalModel

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

q4TestPredict <- predict(q4Mod, testSA)
missClass(testSA$chd, q4TestPredict)

q4TrainPred <- predict(q4Mod, trainSA)
missClass(trainSA$chd, q4TrainPred)



#### Quetion 5 ####

library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

help(vowel.train)
str(vowel.train)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
table(vowel.test$y)

set.seed(33833)
q5rf <- train(y ~ ., data = vowel.train, method = "rf")

?varImp
varImp(q5rf)


varImp(train(y ~ ., data=rbind(vowel.train,vowel.test), method="rf"))
