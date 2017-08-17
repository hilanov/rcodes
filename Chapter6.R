# Chapter 6

# Example of indomethacin PK

t<-seq(0,2,.01)
phi<-c(3,1,.2)
ht<-phi[1]/(1+exp(-(t-phi[2])/phi[3]))
plot(ht~t,type="l",ylim=c(-1,4))
c<-c(-2.2911,16.591,-44.411,56.822,-31.514,6.3028)
ht2<-c[1]+c[2]*t+c[3]*t^2+c[4]*t^3+c[5]*t^4+c[6]*t^5
par(new=T)
plot(ht2~t,type="l",lty=2,ylim=c(-1,4),col="red",ann=F,ylab="",xlab="")

require(nlme)
plot(Indometh)
fm1Indom.nls<-nls(conc~SSbiexp(time,A1,lrc1,A2,lrc2),data=Indometh)
summary(fm1Indom.nls)
plot(fm1Indom.nls,Subject~resid(.),abline=0)
str(fm1Indom.nls)

fm1Indom.lis<-nlsList(conc~SSbiexp(time,A1,lrc1,A2,lrc2),data=Indometh)
fm1Indom.lis
plot(fm1Indom.lis,Subject~resid(.),abline=0)
plot(intervals(fm1Indom.lis))

fm1Indom.nlme<-nlme(fm1Indom.lis,random=pdDiag(A1+lrc1+A2+lrc2~1))
fm1Indom.nlme

fm2Indom.nlme<-update(fm1Indom.nlme,random=pdDiag(A1+lrc1+A2~1))
anova(fm1Indom.nlme,fm2Indom.nlme)
fm3Indom.nlme<-update(fm2Indom.nlme,random=A1+lrc1+A2~1)
fm3Indom.nlme

fm4Indom.nlme<-update(fm3Indom.nlme,random=pdBlocked(list(A1+lrc1~1,A2~1)))
anova(fm3Indom.nlme,fm4Indom.nlme)
anova(fm2Indom.nlme,fm4Indom.nlme)
plot(fm4Indom.nlme,id=.05,adj=-1)
qqnorm(fm4Indom.nlme)
plot(augPred(fm4Indom.nlme,level=0:1))
summary(fm4Indom.nlme)

# Example of soybeans

head(Soybean)
plot(Soybean,outer=~Year*Variety,key=F)

fm1Soy.lis<-nlsList(weight~SSlogis(Time,Asym,xmid,scal),data=Soybean)
fm1Soy.lis
plot(intervals(fm1Soy.lis))
fm1Soy.nlme<-nlme(fm1Soy.lis)
fm1Soy.nlme
plot(fm1Soy.nlme)

fm2Soy.nlme<-update(fm1Soy.nlme,weights=varPower())
anova(fm1Soy.nlme,fm2Soy.nlme)
plot(fm2Soy.nlme)

plot(ranef(fm2Soy.nlme,augFrame = T),form=~Year*Variety,layout=c(3,1))

soyFix<-fixef(fm2Soy.nlme)
options(contrasts=c("contr.treatment","contr.poly"))
fm3Soy.nlme
anova(fm3Soy.nlme)

fm4Soy.nlme<-nlme(weight~SSlogis(Time,Asym,xmid,scal),data=Soybean,
                  fixed=list(Asym~Year*Variety,xmid~Year+Variety,scal~Year),
                  random=Asym~1,
                  start=c(17,0,0,0,0,0,52,0,0,0,7.5,0,0),
                  weights=varPower(.95),control=list(verbose=T))
summary(fm4Soy.nlme)
plot(fm4Soy.nlme)
plot(augPred(fm4Soy.nlme,level=0:1))

# Example of phenobarbital PK

head(Phenobarb)
fm1Pheno.nlme<-nlme(conc~phenoModel(Subject,time,dose,lCl,lV),
                    data=Phenobarb,fixed=lCl+lV~1,
                    random=pdDiag(lCl+lV~1),start=c(-5,0),
                    na.action=NULL,naPattern=~!is.na(conc))
fm1Pheno.nlme

fm1Pheno.ranef<-ranef(fm1Pheno.nlme,augFrame = T)
plot(fm1Pheno.ranef,form=lCl~Wt+ApgarInd)
plot(fm1Pheno.ranef,form=lV~Wt+ApgarInd)

options(contrasts=c("contr.treatment","contr.poly"))
fm2Pheno.nlme<-update(fm1Pheno.nlme,fixed=list(lCl~Wt,lV~Wt+ApgarInd),
                     start=c(-5.0935,0,.34259,0,0),
                     control=list(pnlsTol=.01,msTol=.01,tolerance=.01,
                                  pnlsMaxIter=100,msMaxIter=500,maxIter=5000))
summary(fm2Pheno.nlme)

fm3Pheno.nlme<-update(fm2Pheno.nlme,fixed=lCl+lV~Wt,start=fixef(fm2Pheno.nlme)[-5])
fm3Pheno.nlme
summary(fm3Pheno.nlme)
plot(fm3Pheno.nlme,conc~fitted(.),abline=c(0,1))


