#quiz 3 


##Q1

#In a population of interest, a sample of 9 men yielded a sample average brain volume of 1,100cc and a standard deviation of 30cc. What is a 95% Student's T confidence interval for the mean brain volume in this new population?


1100 +      qt(0.05/2,9-1) * 30 / sqrt(9)
1100 -      qt(0.05/2,9-1) * 30 / sqrt(9)


#q2
d = -2.0
n=9
- d * sqrt(n) / qt(0.05/2,n-1)

#q3 paird individual performance will dominate

#q4     The average MWT for the new system was 3 hours with a variance of 0.60 while the average MWT for the old system was 5 hours with a variance of 0.68. Consider the 95% confidence interval estimate for the differences of the mean MWT associated with the new system. Assume a constant variance. What is the interval? Subtract in this order (New System - Old System).

dbar <- 3 - 5
snew <- 0.60
sold <- 0.68
n <- 10
seold<- sold/n
senew <-snew/n
nu <- (seold^2 +senew^2)^2 /( seold^4/(n-1) + senew^4/(n-1))
nu<-floor(nu)
nu<- n + n - 2
sp= ((n-1)*sold^2 + (n-1)*snew^2) / (n+n-2)
dbar + qt(0.05/2, nu) * sqrt(sp) * sqrt(1/n + 1/n)
dbar - qt(0.05/2, nu) * sqrt(sp*(1/n + 1/n))

dbar - qt(0.05/2, nu) * sqrt(snew^2/n + sold^2/n)


#well maybe it is under normality assumption
dbar + qnorm(0.05/2) * sqrt(snew^2/n + sold^2/n)
dbar - qnorm(0.05/2) * sqrt(snew^2/n + sold^2/n)

#inconclusive here... hmm -2.75

#q5.

#q6 To further test the hospital triage system, administrators selected 200 nights and randomly assigned a new triage system to be used on 100 nights and a standard system on the remaining 100 nights. They calculated the nightly median waiting time (MWT) to see a physician. The average MWT for the new system was 4 hours with a standard deviation of 0.5 hours while the average MWT for the old system was 6 hours with a standard deviation of 2 hours. Consider the hypothesis of a decrease in the mean MWT associated with the new treatment. What does the 95% independent group confidence interval with unequal variances suggest vis a vis this hypothesis? (Because there's so many observations per group, just use the Z quantile instead of the T.)
dbar <- 6 - 4
snew <- 0.50
sold <- 2.0
n <- 100

#well maybe it is under normality assumption
dbar + qnorm(0.05/2) * sqrt(snew^2/n + sold^2/n)
dbar - qnorm(0.05/2) * sqrt(snew^2/n + sold^2/n)


#q7
#

dbar <- -3 - 1
snew <- 1.5
sold <- 1.8
n <- 9

nu<- n + n - 2
sp= ((n-1)*sold^2 + (n-1)*snew^2) / (n+n-2)
dbar + qt(0.05/2, nu) * sqrt(sp) * sqrt(1/n + 1/n)
dbar - qt(0.05/2, nu) * sqrt(sp*(1/n + 1/n))
#-5.364