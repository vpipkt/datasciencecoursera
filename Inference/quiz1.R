#inference Q1


#q1
#P(M U F) = P(M) + P(F) - P(M n F)
0.17 - .12 + .06

#q2
#no controversy
quantile(runif(100000),.75)

#q3
#if p is heads: (1-p)Y - pX = 0 
#Y/X= p / (1-p)

#q4 no controversy

#q5
x <- 1:4
p <- x/sum(x)
temp <- rbind(x, p)
rownames(temp) <- c("X", "Prob")
temp

sum(x*p)

#q6
sens <- 0.75 #P(+ | D) true positives
spec <- 0.52 # P(- | D')
pd   <- 0.30 #P(D)
#find P(D | +)
# consider the set of answers available and the mechanics of updating your prior...