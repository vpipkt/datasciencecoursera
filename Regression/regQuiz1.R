#regression quiz 1

#q1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
weighted.mean(x,c)

sum(x*w)/sum(w)

#q2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
summary(lm(y~x+0))

plot(y~x,xlim=c(0,max(x)))
abline(lm(y~x+0),add=TRUE)

#q3
data(mtcars)
summary(mtcars)
q3<- lm(mpg~wt,mtcars)
summary(q3)
plot(mpg~wt,mtcars)
abline(q3)

#q4 # beta1 =  cov * sd(y) / sd(x) --> 0.5 * 1 / .5 = 1

#q5 - should the question not be E[normalized score on quiz 2]
.4*1.5

#q6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
z <- (x - mean(x)) / sd(x)
z[1]

#q7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
summary(lm(y~x))

#q8
0

#q9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

#qTen -- var/var
