#“Is an automatic or manual transmission better for MPG?
#Quantify the MPG difference between automatic and manual transmissions?

#library calls

data(mtcars)
help(mtcars)
#variable am is Transmission
#0 = auto; 1 = manual

mtcars$tran <- factor(mtcars$am, levels = c(1,0), labels = c("manual", "auto"))

##### EXPLORATORY
library(ggplot2)
p <- ggplot(mtcars, aes(y = mpg, color = tran))
p + geom_point(aes(x = wt))

pca <- princomp(mtcars[,1:11])
pca$loadings

pca$scores[,1]

plot(pca$scores[,1:2], col=mtcars$tran)
p + geom_point(aes(x = -pca$scores[,1]), size = 3) + 
    geom_text(aes(label = row.names(mtcars), x = -pca$scores[,1]), size=3, angle = 35, hjust = 0, position="dodge") +
    labs(title = "MPG versus 1st Principal Component", x = "First Principal Component (displacement + horsepower)") + ylim(10, 37) + xlim(-175,300)
#pca 1 is 0.9 * disp + 0.44 * hp; small imports to right; fleetwood left

plot(disp~cyl,mtcars, col = am+2)

d <- ggplot(mtcars, aes(x = disp, size = mpg, y = wt, col=tran))
d <- ggplot(mtcars, aes(x = disp, size = mpg, y = qsec, col=tran))
d + geom_point()


########################## MODELS
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

#performance matters makes trans much more significant and actually the abs(coef) is larger; The model fit better;
anova(am,qsec)

wt <- lm(mpg ~ tran + wt, data = mtcars)
summary(wt)
anova(am,wt)
#weight makes tran not matter
wt0 <- lm(mpg ~ wt, mtcars)
summary(wt0)
#wt0 is the better quality model overall; but no diagnostics yet
par(mfrow=c(2,2))
plot(wt0)

gear <- lm(mpg ~ tran + gear, mtcars)
summary(gear)
#gear doesn't  improve the model
anova(am,gear)

#this is sort of interesting; notice the  sign of gear. sign of interaction!
summary(lm(mpg ~ tran * gear,mtcars))


#lets just use disp
disp <- lm(mpg ~ tran + disp, mtcars)
summary(disp)
anova(am, disp)
#disp conclude similary to weight: now trans not matter

plot(disp ~ wt, mtcars)
#fairly strong collinearity here


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
#either wt or disp, without Tran
summary(wt0)
summary(disp0)
summary(hp)

hp.wt <- lm(mpg ~ tran + hp * wt, mtcars)
summary(hp.wt)

# BEST model yet of course here is this:
hp.wt0 <- lm(mpg ~ hp*wt, mtcars)
summary(hp.wt0)
plot(hp.wt0)


model2 <- ggplot(mtcars, aes(y=mpg, x=wt))
model2 + geom_point(aes(col = tran))


plot(mpg~hp * wt, mtcars)

#
hp2 <- lm(mpg ~ tran * hp, mtcars)
summary(hp2)

summary(hp)



###### confidence diagnostic plot
plot(mpg~hp, mtcars,col=tran)
abline(hp$coefficients[1], hp$coefficients[3])
abline(hp$coefficients[1] + hp$coefficients[2], hp$coefficients[3],col=3)
predict
?predict

news<-data.frame(hp=rep(seq(50,325,25),2),tran=rep(c("auto","manual"), each=12))
pred <- predict(hp,news,interval="confidence")
lines(x=news$hp, y=pred[,2], lty=2)
lines(x=news$hp, y=pred[,3], lty=2,col=3)
 

## after ggplot
p <- ggplot(mtcars, aes(x = hp, y=mpg, group = tran), . ~ tran) + geom_point()
p + geom_smooth(aes(group = tran, color=tran), method="lm" ,formula= y~x+group)

ggg<- p+ stat_smooth(method="lm")
#so direct call to geom+smooth appears to have wider bounds; particularly on the MT line
?geom_smooth

#instad geom_ribbon
pred.df<- as.data.frame(cbind(news, pred))
p2<- ggplot(mtcars, aes(x = hp, y=mpg))
p2 + geom_point(aes(col=tran))


#################SWAGGY PLOT$$$$$$

pred2 <- predict(hp, interval="confidence")
predframe <- with(mtcars,
                  data.frame(hp,tran, mpg=pred2[,"fit"], lwr=pred2[,"lwr"], upr=pred2[,"upr"]))
coeffs <- data.frame(tran=factor(c("auto", "manual")), a = c(sum(hp$coefficients[1:2]),hp$coefficients[1]), b = rep(hp$coefficients[3],2))
p2<- ggplot(mtcars, aes(x = hp, y=mpg))
p2 + geom_ribbon(data=predframe, aes(ymin=lwr, ymax=upr, group=tran), fill = "grey70", alpha=.5) +
    geom_abline(data = coeffs,aes(intercept = a, slope = b, color = tran)) + geom_point(aes(col=tran), size=3)



############3 and confidence interval
co <- summary(hp)$coefficients
co["tranauto","Estimate"] + c(-1, 1) * qt(.975, df = hp$df) * co["tranauto","Std. Error"]

