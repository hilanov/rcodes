setwd("C:/Users/T.Hirano/Documents/【PJ】/Eisai_ADAS")

# packageの読み込み

library(nlme)
library(MASS)
library(ggplot2)
library(magrittr)
library(dplyr)
library(reshape2)
library(minpack.lm)
library(lattice)


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

# 時間
# t<-seq(0,36,.5)


## モデル式 
MDLFUNK <- function(k, k1, xx) 70*(1-exp(-k*xx))-70/(k-k1)*(exp(-k1*xx)-exp(-k*xx))

## residual function 
# residFun <- function(k, k1, observed, xx) observed - MDLFUNK(k,k1,xx)


## values over which to simulate data 
# x <- adef3$month
# y <- adef3$CHG
# t<-sort(unique(x))
t<-sort(unique(adef3$month))


## パラメータ初期値  
kstart<-.1
k1start<-5

## SUBJIDの上5桁で群を作成
adef3$GRP<-as.numeric(substr(adef3$SUBJID,1,5))

(adef3.grp <- groupedData(CHG ~ month | SUBJID,
   data=adef3))

## nls用に平均値のデータを作成
adef3.mean<-tapply(adef3.grp$CHG,adef3.grp$month,mean)
adef3.mean<-data.frame(month=as.numeric(names(adef3.mean)),CHG=adef3.mean)
row.names(adef3.mean)<-NULL

## 平均値のデータをプロット
plot(adef3.mean,type="b",col="blue")
abline(h=0,v=0)

## nlsによるフィッティング（平均値）
(nls.out <- nls(CHG~MDLFUNK(k, k1, month), data=adef3.mean, start=list(k=.1, k1=5)))


## NLME
ADAS.nlme <- nlme(
  CHG ~ MDLFUNK(k, k1, month),
  data=adef3.grp,
  fixed = k + k1 ~ 1,
  random = k + k1 ~ 1,
  start=coef(as.list(nls.out)))

summary(ADAS.nlme)


## フィッティングカーブの信頼区間を描画
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
               function(x) { MDLFUNK(x[1], x[2], xvals) }
)

c1 <- get_CI(yvals)

pframe <- data.frame(pframe,c1)

ggplot(adef3,aes(month,CHG))+
  geom_point()+
  geom_line(data=pframe,col="red")+
  geom_ribbon(data=pframe,aes(ymin=lwr,ymax=upr),colour=NA,alpha=0.3,
              fill="blue")



## 観測値 vs 予測値のプロット
plot(ADAS.nlme,CHG~fitted(.),abline=c(0,1))


## 個別データに対するフィッティングのプロット
ADAS.pred<-augPred(ADAS.nlme,level=0:1)
ADAS.pred$GRP<-as.numeric(substr(ADAS.pred$.groups,1,5))

NMBR<-unique(ADAS.pred$GRP)

#なぜかプロットされない
for(i in 1:length(NMBR)){
  print(NMBR[i])
  plot(ADAS.pred[ADAS.pred$GRP==NMBR[i],])
}

#やむなく書き下し
plot(ADAS.pred[ADAS.pred$GRP==NMBR[1],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[2],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[3],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[4],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[5],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[6],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[7],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[8],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[9],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[10],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[11],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[12],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[13],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[14],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[15],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[16],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[17],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[18],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[19],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[20],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[21],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[22],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[23],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[24],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[25],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[26],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[27],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[28],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[29],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[30],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[31],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[32],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[33],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[34],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[35],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[36],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[37],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[38],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[39],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[40],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[41],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[42],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[43],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[44],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[45],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[46],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[47],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[48],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[49],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[50],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[51],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[52],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[53],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[54],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[55],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[56],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[57],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[58],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[59],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[60],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[61],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[62],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[63],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[64],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[65],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[66],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[67],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[68],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[69],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[70],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[71],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[72],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[73],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[74],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[75],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[76],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[77],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[78],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[79],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[80],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[81],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[82],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[83],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[84],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[85],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[86],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[87],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[88],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[89],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[90],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[91],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[92],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[93],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[94],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[95],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[96],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[97],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[98],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[99],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[100],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[101],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[102],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[103],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[104],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[105],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[106],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[107],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[108],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[109],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[110],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[111],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[112],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[113],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[114],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[115],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[116],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[117],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[118],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[119],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[120],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[121],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[122],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[123],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[124],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[125],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[126],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[127],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[128],])
plot(ADAS.pred[ADAS.pred$GRP==NMBR[129],])


