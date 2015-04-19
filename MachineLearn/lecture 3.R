data(iris)
names(iris)

library(ggplot2)
library(caret)

# Classification and regression trees 

#in the *iris* data, predict the species

inTrain <- createDataPartition(y=iris$Species, p = 0.7, list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)

qplot(Petal.Length, Sepal.Width, colour=Species, data=training)
featurePlot(x=training, y=training$Species, plot = "pairs")
modfit <- train(Species ~ ., method="rpart", data=training)
print(modfit$finalModel)


plot(modfit$finalModel, uniform=TRUE)
text(modfit$finalModel, use.n=T, all=T)

confusionMatrix(testing$Species, predict(modfit, testing))
#0.911 accuracy, 0.8667 kappa

# Now try it with random forests
modfit.forest <- train(Species ~ ., data = training, method="rf", prox = TRUE)
modfit.forest
#already better accuracy than the solo tree

#example of 2nd tree squint at that a while
getTree(modfit.forest$finalModel, k=2)

confusionMatrix(testing$Species, predict(modfit.forest, testing))
#0.9333 accuracy, 0.9 kappa

#boostin with trees
gbm <- train(Species ~ ., data = training, method = "gbm", verbose = F)
print(gbm)
predict(gbm, training)
confusionMatrix(testing$Species, predict(gbm, testing))


#naive bayes
nb <- train(Species ~ ., data=training, method="nb")
table(predict(nb, training), training$Species)
confusionMatrix(testing$Species, predict(nb, testing))
