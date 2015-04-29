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
