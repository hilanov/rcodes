setwd("C:/Users/T.Hirano/Documents/【PJ】/Eisai_ADAS")

# packageの読み込み

library(nlme)
library(MASS)
library(ggplot2)
library(magrittr)
library(dplyr)
library(reshape2)
library(minpack.lm)


# データの読み込み
adef<-read.csv(file="C:/Users/T.Hirano/Documents/【PJ】/Eisai_ADAS/ADEF.csv")

# 同時点で記録のあるオブザベーションを削除
adef2 <- dplyr::distinct(adef, SUBJID, month, .keep_all=T)

# monthとCHGがNAであるオブザベーションを削除
adef3<-adef2[is.na(adef2$month)!=T & is.na(adef2$CHG)!=T,]

# 必要なデータだけ残す
adef3<-subset(adef3,select=c("SUBJID","month","CHG"))

# プロット
ggplot(adef3, aes(x=month,y=CHG,group=SUBJID,colour=SUBJID))+
  geom_line()+guides(colour=F)+geom_hline(yintercept=0)+
  scale_y_continuous(limits = c(-30, 35))


###############################################
### Non-linear Fitting by a kinetics model
###############################################

# Time
t<-seq(0,36,.5)


## model based on a list of parameters 
LJnew <- function(k, k1, xx) 70*(1-exp(-k*xx))-70/(k-k1)*(exp(-k1*xx)-exp(-k*xx))

## residual function 
residFun <- function(k, k1, observed, xx) observed - LJnew(k,k1,xx)


# 全てに対してフィッティング

## values over which to simulate data 
x <- adef3$month
y <- adef3$CHG
t<-sort(unique(x))


## starting values for parameters  
# parStart <- list(k=.1, k1=5) 
kstart<-.1
k1start<-5

(adef3.grp <- groupedData(CHG ~ month | SUBJID,
   data=adef3))


## perform fit 
# nls.out <- nls.lm(par=list(k=kstart, k1=k1start), fn = residFun, observed = y,
#                   xx = x, control = nls.lm.control(nprint=1))

adef3.mean<-tapply(adef3.grp$CHG,adef3.grp$month,mean)
adef3.mean<-data.frame(month=as.numeric(names(adef3.mean)),CHG=adef3.mean)
row.names(adef3.mean)<-NULL
plot(adef3.mean,type="b",col="blue")
abline(h=0,v=0)

(nls.out <- nls(CHG~LJnew(k, k1, month), data=adef3.mean, start=list(k=.1, k1=5)))



ADAS.nlme <- nlme(
  CHG ~ LJnew(k, k1, month),
  data=adef3.grp,
  fixed = k + k1 ~ 1,
  random = k + k1 ~ 1,
  start=coef(as.list(nls.out)))

summary(ADAS.nlme)

xvals <-  with(adef3.grp,seq(min(month),max(month),length.out=100))
nresamp <- 1000
## pick new parameter values by sampling from multivariate normal distribution based on fit
pars.picked <- mvrnorm(nresamp, mu = fixef(ADAS.nlme), Sigma = vcov(ADAS.nlme))

## predicted values: useful below
pframe <- with(adef3,data.frame(month=xvals))
pframe$CHG <- predict(ADAS.nlme,newdata=pframe,level=0)

## utility function
get_CI <- function(y,pref="") {
  r1 <- t(apply(y,1,quantile,c(0.025,0.975)))
  setNames(as.data.frame(r1),paste0(pref,c("lwr","upr")))
}

set.seed(101)

yvals <- apply(pars.picked,1,
               function(x) { LJnew(x[1], x[2], xvals) }
)


c1 <- get_CI(yvals)

pframe <- data.frame(pframe,c1)

ggplot(adef3,aes(month,CHG))+
  geom_point()+
  geom_line(data=pframe,col="red")+
  geom_ribbon(data=pframe,aes(ymin=lwr,ymax=upr),colour=NA,alpha=0.3,
              fill="blue")



