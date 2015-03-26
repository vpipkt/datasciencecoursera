

#regression quiz 4

##### question 1 ########

#Consider the space shuttle data ?shuttle in the MASS library. Consider modeling the use of the autolander as the outcome (variable name use). Fit a logistic regression model with autolander (variable auto) use (labeled as "auto" 1) versus not (0) as predicted by wind sign (variable wind). Give the estimated odds ratio for autolander use comparing head winds, labeled as "head" in the variable headwind (numerator) to tail winds (denominator).

library(MASS)
?shuttle
data(shuttle)

str(shuttle)

#this is exp(b1)
summary(shuttle$wind)

q1<- glm(use ~ wind, shuttle, family="binomial")
summary(q1)
 
exp(q1$coefficients["windtail"])

##########  QUESTION 2 ######
# Consider the previous problem. Give the estimated odds ratio for autolander use comparing head winds (numerator) to tail winds (denominator) adjusting for wind strength from the variable magn.

q2 <- glm(use ~ wind + magn, shuttle, family = "binomial")
summary(q2)
exp(q2$coefficients[2])


####### QUESTION THREE  ####
shuttle$notuse <-  2-as.numeric(shuttle$use)
summary(shuttle)
levels(shuttle$use)
str(shuttle)
cbind(shuttle$use, shuttle$notuse)
summary(glm(notuse ~ wind , shuttle, family="binomial"))
q1$coefficients

############## Question FOUR ##

#Consider the insect spray data InsectSprays. Fit a Poisson model using spray as a factor level. Report the estimated relative rate comapring spray A (numerator) to spray B (denominator).
data(InsectSprays)
summary(InsectSprays)
q4 <- glm(count ~ spray, InsectSprays, family="poisson", offset=log(count +1))
summary(q4)
exp(-q4$coefficients[2])


######q 5 ####
q5 <- glm(count ~ spray + offset(1), InsectSprays, family="poisson")
q5.2 <- glm(count ~ spray, InsectSprays, family="poisson", offset=log(10))

summary(q5)
summary(q5.2)
q5$coefficients / q5.2$coefficients

#no clue said divide 10

######### Question 6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
plot(y~x)
q6df <- data.frame(x=x, y=y)
q6<- lm(y~x, subset(q6df, x>=0))
summary(q6)
abline(q6,col="green")





15 * (5/7  + 5/6+ 2 )
