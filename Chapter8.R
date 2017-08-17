# Chapter8

require(nlme)
require(lattice)

# Example of Orange tree

plot(Orange,outer=~1)
head(Orange)

logist<-function(x,Asym,xmid,scal) Asym/(1+exp(-(x-xmid)/scal))
fm1Oran.nls<-nls(circumference~logist(age,Asym,xmid,scal),
                data=Orange,start=c(Asym=170,xmid=700,scal=500))
summary(fm1Oran.nls)

  # alternative method
logist<-deriv(~Asym/(1+exp(-(x-xmid)/scal)),
              c("Asym","xmid","scal"),function(x,Asym,xmid,scal){}) 
Asym<-180; xmid<-700; scal<-300
logist(Orange$age[1:7],Asym,xmid,scal)
fm1Oran.nls<-nls(circumference~logist(age,Asym,xmid,scal),
                 data=Orange,start=c(Asym=170,xmid=700,scal=500))
summary(fm1Oran.nls)

plot(fm1Oran.nls)
plot(fm1Oran.nls,Tree~resid(.),abline=0)

Orange.sortAvg<-sortedXyData("age","circumference",Orange)
Orange.sortAvg
NLSstClosestX(Orange.sortAvg,130)

logistInit<-function(mCall,LHS,data){
  xy<-sortedXyData(mCall[["x"]],LHS,data)
  if(nrow(xy)<3){
    stop("Too few distinct input values to fit a logistic")
  }
  Asym<-max(abs(xy[,"y"]))
  if(Asym!=max(xy[,"y"])) Asym<-Asym
  xmid<-NLSstClosestX(xy,.5*Asym)
  scal<-NLSstClosestX(xy,.75*Asym)-xmid
  value<-c(Asym,xmid,scal)
  names(value)<-mCall[c("Asym","xmid","scal")]
  value
}

logist<-selfStart(logist,initial=logistInit)
class(logist)

  # alternative method
logist<-selfStart(~Asym/(1+exp(-(x-xmid)/scal)),
                  initial=logistInit,parameters=c("Asym","xmid","scal"))

getInitial(circumference~logist(age,Asym,xmid,scal),Orange)

nls(circumference~logist(age,Asym,xmid,scal),Orange)


fm1Oran.lis<-nlsList(circumference~SSlogis(age,Asym,xmid,scal)|Tree,data=Orange)
  # when applied to a groupedData object
fm1Oran.lis<-nlsList(SSlogis,Orange)

fm1Oran.lis
summary(fm1Oran.lis)
plot(intervals(fm1Oran.lis),layout=c(3,1))
plot(fm1Oran.lis,Tree~resid(.),abline=0)

fm1Oran.nlme<-nlme(circumference~SSlogis(age,Asym,xmid,scal),
                   data=Orange,
                   fixed=Asym+xmid+scal~1,
                   start=fixef(fm1Oran.lis))
fm1Oran.nlme<-nlme(fm1Oran.lis) # More simple formula

fm1Oran.nlme
summary(fm1Oran.nlme)
pairs(fm1Oran.lis,id=.1)
summary(fm1Oran.nls)
pairs(fm1Oran.nlme)

fm2Oran.nlme<-update(fm1Oran.nlme,random=Asym~1)
anova(fm1Oran.nlme,fm2Oran.nlme)
plot(fm1Oran.nlme)
plot(fm2Oran.nlme)
plot(augPred(fm2Oran.nlme,level=0:1),layout=c(5,1))
qqnorm(fm2Oran.nlme,abline=c(0,1))

# Example of theophylline

head(Theoph)
fm1Theo.lis<-nlsList(conc~SSfol(Dose,Time,lKe,lKa,lCl),data=Theoph)
fm1Theo.lis
plot(intervals(fm1Theo.lis))
pairs(fm1Theo.lis,id=.1)

fm1Theo.nlme<-nlme(fm1Theo.lis)
fm1Theo.nlme
intervals(fm1Theo.nlme,which="var-cov")

(fm2Theo.nlme<-update(fm1Theo.nlme,random=pdDiag(list(lKe~1,lKa~1,lCl~1))))
(fm2Theo.nlme<-update(fm1Theo.nlme,random=pdDiag(lKe+lKa+lCl~1)))

(fm3Theo.nlme<-update(fm2Theo.nlme,random=pdDiag(lKa+lCl~1)))

anova(fm1Theo.nlme,fm3Theo.nlme,fm2Theo.nlme)
plot(fm3Theo.nlme)
qqnorm(fm3Theo.nlme,~ranef(.))
summary(fm3Theo.nlme)

  # Error in R
fm4Theo.nlme<-update(fm3Theo.nlme, weights=varConstPower(power=.5))


# Example of CO2 uptake

head(CO2)
plot(CO2,outer=~Type*Treatment,layout=c(4,1),key=F)
fm1CO2.lis<-nlsList(SSasympOff,CO2)
fm1CO2.lis
plot(intervals(fm1CO2.lis))
fm1CO2.nlme<-nlme(fm1CO2.lis)
fm1CO2.nlme

fm2CO2.nlme<-update(fm1CO2.nlme,random=Asym+lrc~1)
fm2CO2.nlme
plot(fm2CO2.nlme,id=.05,cex=.8,adj=-.5)
anova(fm1CO2.nlme,fm2CO2.nlme)

fm2CO2.nlmeRE<-ranef(fm2CO2.nlme,augFrame = T)
fm2CO2.nlmeRE
class(fm2CO2.nlmeRE)
plot(fm2CO2.nlmeRE,form=~Type*Treatment)

options(contrasts=c("contr.helmert","contr.poly"))
contrasts(CO2$Type)
contrasts(CO2$Treatment)

fm3CO2.nlme<-update(fm2CO2.nlme,fixed=list(Asym~Type*Treatment,lrc+c0~1),
                    start=c(32.412,0,0,0,-4.5603,49.344))
summary(fm3CO2.nlme)
anova(fm3CO2.nlme,Terms=2:4)
fm3CO2.nlmeRE<-ranef(fm3CO2.nlme,aug=T)
plot(fm3CO2.nlmeRE,form=~Type*Treatment)

fm3CO2.fix<-fixef(fm3CO2.nlme)
fm4CO2.nlme<-update(fm3CO2.nlme,fixed=list(Asym+lrc~Type*Treatment,c0~1),
                    start=c(fm3CO2.fix[1:5],0,0,0,fm3CO2.fix[6]))
summary(fm4CO2.nlme)

fm5CO2.nlme<-update(fm4CO2.nlme,random=Asym~1)
anova(fm4CO2.nlme,fm5CO2.nlme)

CO2x<-CO2
CO2x$type<-2*(as.integer(CO2x$Type)-1.5)
CO2x$treatment<-2*(as.integer(CO2x$Treatment)-1.5)
fm1CO2x.nls<-nls(uptake~SSasympOff(conc,Asym.Intercept+
                                     Asym.Type*type+Asym.Treatment*treatment+
                                     Asym.TypeTreatment*type*treatment,lrc.Intercept+
                                     lrc.Type*type+lrc.Treatment*treatment+
                                     lrc.TypeTreatment*type*treatment,c0),data=CO2x,
                 start=c(Asym.Intercept=32.371,Asym.Type=-8.0086,
                         Asym.Treatment=-4.2001,Asym.TypeTreatment=-2.7253,
                         lrc.Intercept=-4.5267,lrc.Type=.13112,
                         lrc.Treatment=.093928,lrc.TypeTreatment=.17941,
                         c0=50.126))
anova(fm5CO2.nlme,fm1CO2x.nls)

plot(augPred(fm5CO2.nlme,level=0:1),layout=c(6,2))


# Example of quinidine

fm1Quin.nlme<-nlme(conc~quinModel(Subject,time,conc,dose,interval,lV,lKa,lCl),
                   data=Quinidine,fixed=lV+lKa+lCl~1,
                   random=pdDiag(lV+lCl~1),groups=~Subject,
                   start=list(fixed=c(5,-.3,2)),
                   na.action=NULL,naPattern=~!is.na(conc))
fm1Quin.nlme

fm1Quin.nlmeRE<-ranef(fm1Quin.nlme,aug=T)
fm1Quin.nlmeRE[1:3,]

plot(fm1Quin.nlmeRE,form=lCl~Age+Smoke+Ethanol+Weight+Race+Height+glyco+
       Creatinine+Heart,control=list(cex.axis=.7),layout=c(3,3))
plot(fm1Quin.nlmeRE,form=lV~Age+Smoke+Ethanol+Weight+Race+Height+glyco+
       Creatinine+Heart,control=list(cex.axis=.7),layout=c(3,3))

fm1Quin.fix<-fixef(fm1Quin.nlme)
fm2Quin.nlme<-update(fm1Quin.nlme,fixed=list(lCl~glyco,lKa+lV~1),
                     start=c(fm1Quin.fix[3],0,fm1Quin.fix[2:1]))
summary(fm2Quin.nlme)
fm2Quin.nlmeRE<-ranef(fm2Quin.nlme,aug=T)
plot(fm2Quin.nlmeRE,form=lCl.(Intercept)~Age+Smoke+Ethanol+Weight+Race+Height+glyco+
       Creatinine+Heart,control=list(cex.axis=.7),layout=c(3,3))

options(contrasts=c("contr.treatment","contr.poly"))
fm2Quin.fix<-fixef(fm2Quin.nlme)
fm3Quin.nlme<-update(fm2Quin.nlme,fixed=list(lCl~glyco+Creatinine,lKa+lV~1),
                     start=c(fm2Quin.fix[1:2],.2,fm2Quin.fix[3:4]),
                     control=list(pnlsTol=.01,msTol=.01,tolerance=.01,
                                  pnlsMaxIter=100,msMaxIter=500,
                                  maxIter=5000))
summary(fm3Quin.nlme)

fm3Quin.fix<-fixef(fm3Quin.nlme)
fm4Quin.nlme<-update(fm3Quin.nlme,fixed=list(lCl~glyco+Creatinine+Weight,lKa+lV~1),
                     start=c(fm3Quin.fix[1:3],0,fm3Quin.fix[4:5]),
                     control=list(pnlsTol=.01,msTol=.01,tolerance=.01,
                                  pnlsMaxIter=100,msMaxIter=500,
                                  maxIter=5000))
summary(fm4Quin.nlme)
anova(fm3Quin.nlme,fm4Quin.nlme)
plot(fm4Quin.nlme,xlim=c(0,6.2))

fm5Quin.nlme<-update(fm4Quin.nlme,weights=varPower())

# Example of analogue MOS circuit

head(Wafer)

  # From Section 4.2, 4.3

fm1Wafer<-lme(current~voltage+I(voltage^2),data=Wafer,
              random=list(Wafer=pdDiag(~voltage+I(voltage^2)),
                          Site=pdDiag(~voltage+I(voltage^2))))
summary(fm1Wafer)
plot(fm1Wafer,resid(.)~voltage|Wafer,layout=c(5,2))
plot(fm1Wafer,resid(.)~voltage|Wafer,layout=c(5,2),
     panel=function(x,y,...){
       panel.grid()
       panel.xyplot(x,y)
       panel.loess(x,y,lty=2)
       panel.abline(0,0)
     })
fm2Wafer<-update(fm1Wafer,fixed=.~.+cos(4.5679*voltage)+sin(4.5679*voltage),
                 random=list(Wafer=pdDiag(~voltage+I(voltage^2)),
                             Site=pdDiag(~voltage+I(voltage^2))))
summary(fm2Wafer)
intervals(fm2Wafer)
qqnorm(fm2Wafer)

plot(ranef(fm2Wafer)$Wafer)

  # fm3Wafer cannot be calculated by R because of convergence error.
  # Examples hereafter cannot be calculated.
fm3Wafer<-update(fm2Wafer,random=list(Wafer=~voltage+I(voltage^2),
                                      Site=pdDiag(~voltage+I(voltage^2))),
                 control=list(pnlsTol=.1,msTol=.1,tolerance=.1,
                              pnlsMaxIter=1000,msMaxIter=1000,
                              maxIter=10000))
summary(fm3Wafer)
anova(fm2Wafer,fm3Wafer)

fm1Wafer.nlmeR<-nlme(current~A+B*cos(4.5679*voltage)+C*sin(4.5679*voltage),data=Wafer,
                     fixed=list(A~voltage+voltage^2,B+C~1),
                     random=list(Wafer=A~voltage+voltage^2,
                                 Site=pdBlocked(list(A~1,A~voltage+voltage^2-1))),
                     start=fixef(fm4Wafer),method="REML")


