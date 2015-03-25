# Quiz 4

Subject    Baseline	Week 2
1	140	132
2	138	135
3	150	151
4	148	146
5	135	130

q1 <- data.frame(
    bp = c(140,138,150,148,135,132,135,151,146,130),
    t  = c(rep("baseline",5),rep("wk2",5)))


library(reshape2)
dcast( melt(q1), t ~ ., mean)

t.test(bp ~ t, q1, paired=TRUE, alternative="two")$p.value

##########3 q2
#A sample of 9 men yielded a sample average brain volume of 1,100cc and a standard deviation of 30cc. What is the complete set of values of μ0 that a test of H0:μ=μ0 would fail to reject the null hypothesis in a two sided 5% Students t-test?

1100 + c(-1,1) * qt(.975, df = 9-1) * 30 / sqrt(9)


#################   Q   3   ########
#Researchers conducted a blind taste test of Coke versus Pepsi. Each of four people was asked which of two blinded drinks given in random order that they preferred. The data was such that 3 of the 4 people chose Coke. Assuming that this sample is representative, report a P-value for a test of the hypothesis that Coke is preferred to Pepsi using a one sided exact test.

#take the "is preferred" to mean p = 0.5 (at least) 
dbinom(3, 4, 0.500) + dbinom(4, 4, 0.500)

dbinom(0, 4, 0.500) + dbinom(1, 4, 0.500)+dbinom(2, 4, 0.500)+dbinom(3, 4, 0.500)+dbinom(4, 4, 0.500)

pbinom(2, size=4, p=0.5, lower.tail=F)

###################  Q 4 ##

#Infection rates at a hospital above 1 infection per 100 person days at risk are believed to be too high and are used as a benchmark. A hospital that had previously been above the benchmark recently had 10 infections over the last 1,787 person days at risk. About what is the one sided P-value for the relevant test of whether the hospital is *below* the standard?

pbinom(10, size = 1787, p = 1/100, lower.tail = T)


############ q  #    F I V E    ####
# Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. Subjects’ body mass indices (BMIs) were measured at a baseline and again after having received the treatment or placebo for four weeks. The average difference from follow-up to the baseline (followup - baseline) was −3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. The corresponding standard deviations of the differences was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI appear to differ between the treated and placebo groups? Assuming normality of the underlying data and a common population variance, give a pvalue for a two sided t test.
s.q5 <-  sqrt((8*1.5^2 + 8*1.8^2)/(16))
t.q5 <- (-3-1) / (s.q5 * sqrt(1/9+1/9))
df   <- 9+9-2
pt(t.q5, df)
#qt(5.13e-5, df)

################ QUESTION   SIX     ###

# as the alpha goes up, the interval gets wider. so... it is 

hw90 <- (1123 - 1077)/2
q6.mean <- 1077 + hw90
se.q6 <- hw90 / qt(.95, df=9-1)

#this is the 95% CI (two sided)
q6.mean + c(-1,1) * se.q6 * qt(.975,df=9-1)


############# Question SEVEN    $#########

# Researchers would like to conduct a study of 100 healthy adults to detect a four year mean brain volume loss of .01 mm3. Assume that the standard deviation of four year volume loss in this population is .04 mm3. About what would be the power of the study for a 5% one sided test versus a null hypothesis of no volume loss?

#power is P(reject null | Ha)

P(x - 0.0 / s*rootN > t | mu=mu_a)
?power.t.test
q7 <- power.t.test(n=100, delta = 0.01, sd = 0.04, sig.level = 0.05, alt="one",type="one.sample")
print(q7)
q7$power

############ qiestopm   8 EIGHT 8 ############

#Researchers would like to conduct a study of n healthy adults to detect a four year mean brain volume loss of .01 mm3. Assume that the standard deviation of four year volume loss in this population is .04 mm3. About what would be the value of n needded for 90% power of type one error rate of 5% one sided test versus a null hypothesis of no volume loss?

q8 <- power.t.test(delta = 0.01, sd = 0.04, sig.level = 0.05,
                   power = 0.90, alt="one",type="one.sample")
print(q8)
q8$n


#### QUESTION NINE ########### 9 #########  9   #####

q9.alpha <- 1 - c(0.8,0.85,0.9,0.95,0.98,0.99,0.999)
q9.pwr <- sapply(q9.alpha, function(sig){
    power.t.test(n=100, delta = 0.01, sd = 0.04, sig.level = sig, alt="one",type="one.sample")$power
})
plot(q9.pwr ~ q9.alpha)

#no free lunch
