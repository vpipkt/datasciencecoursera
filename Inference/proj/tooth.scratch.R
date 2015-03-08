data(ToothGrowth)
help(ToothGrowth)
summary(ToothGrowth)

plot(len ~ supp, ToothGrowth)

table(ToothGrowth[,2:3])

coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")

library(ggplot2)
p <- ggplot(ToothGrowth, aes(x=dose, y=len, group=supp))
p + geom_point(aes(color=supp)) 

p+stat_smooth(method=lm)
p+ facet_grid(.~supp) + geom_point() + geom_smooth(method=lm)

p <- ggplot(ToothGrowth, aes(x = supp, y = len, group = dose))
p + facet_grid(. ~ dose) + geom_violin(aes(group = supp)) +  geom_point(aes(col = supp)) 



t.test(subset(ToothGrowth, supp=="VC", len), subset(ToothGrowth, supp=="OJ", len),
       alternative = "two.sided", paired = FALSE, var.equal=TRUE)

t.test(subset(ToothGrowth, dose==1, len), subset(ToothGrowth, dose>.5, len),
       alternative = "two.sided", paired = FALSE, var.equal=TRUE)

t.test(subset(ToothGrowth, supp=="VC" & dose==2, len), 
       subset(ToothGrowth, supp=="OJ" & dose==2, len),
       alternative = "two.sided", paired = FALSE, var.equal=TRUE)



#confirm dose matters
t.test(subset(ToothGrowth, dose<=0.5, len), subset(ToothGrowth, dose>.5, len),
       alternative = "two.sided", paired = FALSE, var.equal=TRUE)

#confirm supplement matters (at higher doses)
t.test(subset(ToothGrowth, supp=="VC" & dose>0.5, len), 
       subset(ToothGrowth, supp=="OJ" & dose>0.5, len),
       alternative = "two.sided", paired = FALSE, var.equal=TRUE)

#if OJ iz best is 1mg more economical dose?
t.test(subset(ToothGrowth, supp=="OJ" & dose==2, len), 
       subset(ToothGrowth, supp=="OJ" & dose==1, len),
       alternative = "two.sided", paired = FALSE, var.equal=TRUE)



toothGlm <-glm(len ~ supp * dose, data=ToothGrowth)
aov <- anova(toothGlm)
summary(toothGlm)
aov
