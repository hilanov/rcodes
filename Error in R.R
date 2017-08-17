# Example of theophylline
require(nlme)
require(lattice)

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

fm4Theo.nlme<-update(fm3Theo.nlme, weights=varConstPower(power=.5))
# Error in eigen(val, only.values = TRUE) : 
#   infinite or missing values in 'x'
# In addition: Warning messages:


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
# does not finish calculation in R


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



  