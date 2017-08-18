require(metafor)
print(dat.bcg,row.names=F)

dat<-escalc(measure="RR",ai=tpos,bi=tneg,ci=cpos,di=cneg,
            data=dat.bcg,append=T)

(res<-rma(ai=tpos,bi=tneg,ci=cpos,di=cneg,data=dat,measure="RR"))
confint(res)

forest(res,slab=paste(dat$author,dat$year,sep=","),
       xlim=c(-16,6),at=log(c(.05,.25,1,4)),atransf=exp,
       ilab=cbind(dat$tpos,dat$tneg,dat$cpos,dat$cneg),
             ilab.xpos=c(-9.5,-8,-6,-4.5),cex=.75)

op<-par(cex=.75,font=2)
text(c(-9.5,-8,-6,-4.5),15,c("TB+","TB-","TB+","TB-"))
text(c(-8.75,-5.25),16,c("Vaccinated","Control"))
text(-16,15,"Author(s) and Year",pos=4)
text(6,15,"Relative Rist [95% CI]",pos=2)
par(op)

funnel(res,main="Random-Effects Model")
regtest(res,model="lm")
