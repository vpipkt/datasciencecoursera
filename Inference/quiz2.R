##quiz2

#q2
#mean of 80 (mm Hg) and a standard deviation of 10. About what is the probability that a random 35-44 year old has a DBP less than 70?
pnorm(70, mean=80, sd=10)


#q3
qnorm(0.95, mean=1100, sd=75)


#q4
qnorm(0.95, mean=1100, sd=75/sqrt(100))

#q5
dbinom(4,5, 0.5) + dbinom(5,5, 0.5)

#q6 sd / sqrt(n) = 1


#q8
ppois(10, lambda =  3 * 5)

