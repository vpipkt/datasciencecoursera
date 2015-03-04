#Quiz 2


#q1

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
#Give a P-value for the two sided hypothesis test of whether β1 from a linear regression model is 0 or not.

q1 <- lm(y~x)
summary(q1)

#q2
#sd(q1$residuals)
sqrt(sum(q1$residuals^2) / (length(x)-2)  )


#q3
data(mtcars)
q3<-lm(mpg~wt,mtcars)
#not to exceed this
predict(q3,newdata=data.frame(wt=mean(mtcars$wt)))

predict(q3,newdata=data.frame(wt=mean(mtcars$wt)),
        interval="confidence")

#q5 quite easy 
predict(q3, newdata=data.frame(wt=3.000)) #look at options...
predict(q3, newdata=data.frame(wt=3.000),interval="p",level=0.95)

#q6
sumCoef <- summary(q3)$coefficients
(sumCoef[2,1] + c(-1, 1) * qt(.975, df = q3$df) * sumCoef[2, 2]) * 2 

#q7
coef(q3)
coef(lm(mpg~I(wt/100),mtcars))

#q9
summary(q3)

sum(q3$residuals^2) /
    sum((mean(mtcars$mpg) - mtcars$mpg)^2)
