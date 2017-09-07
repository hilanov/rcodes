#
# Zero-inflated Poisson regression
# Example from:
# https://stats.idre.ucla.edu/r/dae/zip/
#


require(ggplot2)
require(pscl)
require(boot)

zinb <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
zinb <- within(zinb, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
})

summary(zinb)

## histogram with x axis in log10 scale
ggplot(zinb, aes(count)) + geom_histogram() + scale_x_log10()

summary(m1 <- zeroinfl(count ~ child + camper | persons, data = zinb))

mnull <- update(m1, . ~ 1)

pchisq(2 * (logLik(m1) - logLik(mnull)), df = 3, lower.tail = FALSE)

summary(p1 <- glm(count ~ child + camper, family = poisson, data = zinb))

vuong(p1, m1)

dput(coef(m1, "count"))

dput(coef(m1, "zero"))

f <- function(data, i) {
  require(pscl)
  m <- zeroinfl(count ~ child + camper | persons, data = data[i, ],
                start = list(count = c(1.598, -1.0428, 0.834), zero = c(1.297, -0.564)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

set.seed(10)
res <- boot(zinb, f, R = 1200, parallel = "snow", ncpus = 4)

## print results
res

## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaLL = bca[5]))
}))

## add row names
row.names(parms) <- names(coef(m1))
## print results
parms

## compare with normal based approximation
confint(m1)

## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaLL = bca[5]))
}))

## add row names
row.names(expparms) <- names(coef(m1))
## print results
expparms

newdata1 <- expand.grid(0:3, factor(0:1), 1:4)
colnames(newdata1) <- c("child", "camper", "persons")
newdata1 <- subset(newdata1, subset=(child<=persons))
newdata1$phat <- predict(m1, newdata1)

ggplot(newdata1, aes(x = child, y = phat, colour = factor(persons))) +
  geom_point() +
  geom_line() +
  facet_wrap(~camper) +
  labs(x = "Number of Children", y = "Predicted Fish Caught")


#
# Example from http://kusanagi.hatenablog.jp/entry/2017/02/23/194835
#

#使うパッケージ
library(fitdistrplus)
library(gamlss)

#ある実測のデータ
d<-c(0,5,6,0,1,8,0,2,0,1,4,2,0,3,8,9,1,1,0,10,1,2,0,2,11,3,0,8,5,11,3,8,8,8,14,7,8,6,5,1,8,7,2,0,6,8,3,3,0,6,2,11,2,0,0,0,0,0,0)

#可視化
hist(d)

#0の割合
length(d[d==0])/length(d)

#まずはポアソンを普通にデータにフィット
fit.pois<-fitdist(d,"pois")
summary(fit.pois)

#次にゼロ過剰ポアソン分布をデータにフィット
fit.ZIP<-fitdist(d,"ZIP",start=1)
summary(fit.ZIP)

#２つのモデルを比較してみる
gofstat(list(fit.pois,fit.ZIP),fitnames=c("Poisson","ZIP"))

#これではσを出してくれない。gamlssを使う
fit.ZIP2<-gamlss(d~1,family=ZIP)
fit.ZIP2

plot(fit.pois)
plot(fit.ZIP)
plot(fit.ZIP2)




