##Quiz 3

?influence.measures

#Question 1
data(mtcars)
help(mtcars)

q1 <- lm(mpg ~ as.factor(cyl) + wt, mtcars)
summary(q1)

plot(mpg~as.factor(cyl),mtcars)
abline(q1)


#question 2
 # the adjusted model is q1

q2 <- lm(mpg ~ as.factor(cyl), mtcars)
summary(q2)


#question 3
 # also use q1 model fit
q3 <- lm(mpg ~ as.factor(cyl) * wt, mtcars)
summary(q3)

anova(q1,q3)


#question 4
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
summary(q1)

#question 5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
q5 <- lm(y ~ x)
hatvalues(q5)
influence.measures(q5)
par(mfrow =c(2,2))
plot(q5)
plot(y~x)

#q6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
influence.measures(lm(y~x))
