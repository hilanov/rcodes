setwd("C:/Users/T.Hirano/Documents/R/Git/rcodes")
# a<-read.csv("clipboard")
# write.csv(a,"a.csv")
a<-read.csv("a.csv")
head(a)

plot(prescription~month,data=a,type="b")

(m <- lm(prescription ~ observation + intervention1 + time1, data=subset(a, intervention2==0)))
summary(m)

abline(m,col="blue")

opar<-par(mfrow=c(2,2))
plot(m)
par(opar)

library(car)
durbinWatsonTest(m, max.lag=4)

