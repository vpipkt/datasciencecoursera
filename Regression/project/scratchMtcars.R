#“Is an automatic or manual transmission better for MPG?
#Quantify the MPG difference between automatic and manual transmissions?

#library calls

data(mtcars)
help(mtcars)
f#variable am is Transmission
#0 = auto; 1 = manual


mtcars$tran <- factor(mtcars$am, levels = c(1,0), labels = c("manual", "auto"))

am <- lm(mpg ~ tran, mtcars)
summary(am)

#auto trans loses 7.2 mpg

library(reshape2)
library(plyr)
dcast(melt(mtcars),tran~variable,mean,subset=.(variable=="mpg"))

t.test(subset(mtcars,tran=="manual",mpg),
       subset(mtcars,tran=="auto",mpg))


#what other variables matter?
#can try PCA to figure out which variables are orthogonal / variable. this doesn't tell us if they're  correlated to mpg

#want to look at qsec as performance measure
# weight ; gears; displacement

qsec <- lm(mpg ~ tran + qsec, data = mtcars)
summary(qsec)

#performance matters makes trans much more significant and actually the abs(coef) is larger

wt <- lm(mpg ~ tran + wt, data = mtcars)
summary(wt)
anova(am,wt)
#weight makes tran not matter
wt0 <- lm(mpg ~ wt, mtcars)
summary(wt0)
#wt0 is the better quality model overall

gear <- lm(mpg ~ tran + gear, mtcars)
summary(gear)
#gear doesn't really improve the model
anova(am,gear)

#this is sort of interesting
summary(lm(mpg ~ tran * gear,mtcars))



plot(disp~cyl,mtcars)
#lets just use disp
disp <- lm(mpg ~ tran + disp, mtcars)
summary(disp)
anova(am, disp)
#disp conclude similary to weight

plot(disp ~ wt, mtcars)
#fairly strong collinearity here

pca <- princomp(mtcars[,1:11])
pca$loadings

plot(disp~hp,mtcars)

disp.hp <- lm(mpg ~ tran + disp + hp, mtcars)
summary(disp.hp)
anova(am,disp,disp.hp)
#interesting here AUTO is significant in the model but disp not so much

hp <- lm(mpg ~ tran + hp, mtcars)
summary(hp)
#auto matters
anova(am,hp,disp.hp)
#so disp contains a lot of info in the dataset but HP is more useful with this model

hp.gear <- lm(mpg ~ tran + hp + gear:tran,mtcars)
summary(hp.gear)
#now the elephant is wiggling his trunk

hp.carb <- lm(mpg ~ tran + hp + carb, mtcars)
summary(hp.carb)
#carb not significant. 

#so hp is the best model with TRAN
#either wt or disp without
summary(wt0)
summary(disp)
hp$rsquared
