library(nlme)
library(MASS)

fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
            data = Loblolly,
            fixed = Asym + R0 + lrc ~ 1,
            random = Asym ~ 1,
            start = c(Asym = 103, R0 = -8.5, lrc = -3.3))

xvals <-  with(Loblolly,seq(min(age),max(age),length.out=100))
nresamp <- 1000
## pick new parameter values by sampling from multivariate normal distribution based on fit
pars.picked <- mvrnorm(nresamp, mu = fixef(fm1), Sigma = vcov(fm1))

## predicted values: useful below
pframe <- with(Loblolly,data.frame(age=xvals))
pframe$height <- predict(fm1,newdata=pframe,level=0)

## utility function
get_CI <- function(y,pref="") {
  r1 <- t(apply(y,1,quantile,c(0.025,0.975)))
  setNames(as.data.frame(r1),paste0(pref,c("lwr","upr")))
}

set.seed(101)
yvals <- apply(pars.picked,1,
               function(x) { SSasymp(xvals,x[1], x[2], x[3]) }
)
c1 <- get_CI(yvals)

## bootstrapping
sampfun <- function(fitted,data,idvar="Seed") {
  pp <- predict(fitted,levels=1)
  rr <- residuals(fitted)
  dd <- data.frame(data,pred=pp,res=rr)
  ## sample groups with replacement
  iv <- levels(data[[idvar]])
  bsamp1 <- sample(iv,size=length(iv),replace=TRUE)
  bsamp2 <- lapply(bsamp1,
                   function(x) {
                     ## within groups, sample *residuals* with replacement
                     ddb <- dd[dd[[idvar]]==x,]
                     ## bootstrapped response = pred + bootstrapped residual
                     ddb$height <- ddb$pred +
                       sample(ddb$res,size=nrow(ddb),replace=TRUE)
                     return(ddb)
                   })
  res <- do.call(rbind,bsamp2)  ## collect results
  if (is(data,"groupedData"))
    res <- groupedData(res,formula=formula(data))
  return(res)
}

pfun <- function(fm) {
  predict(fm,newdata=pframe,level=0)
}

set.seed(101)
yvals2 <- replicate(nresamp,
                    pfun(update(fm1,data=sampfun(fm1,Loblolly,"Seed"))))
c2 <- get_CI(yvals2,"boot_")

## delta method
ss0 <- with(as.list(fixef(fm1)),SSasymp(xvals,Asym,R0,lrc))
gg <- attr(ss0,"gradient")
V <- vcov(fm1)
delta_sd <- sqrt(diag(gg %*% V %*% t(gg)))
c3 <- with(pframe,data.frame(delta_lwr=height-1.96*delta_sd,
                             delta_upr=height+1.96*delta_sd))

pframe <- data.frame(pframe,c1,c2,c3)

library(ggplot2); theme_set(theme_bw())
ggplot(Loblolly,aes(age,height))+
  geom_line(alpha=0.2,aes(group=Seed))+
  geom_line(data=pframe,col="red")+
  geom_ribbon(data=pframe,aes(ymin=lwr,ymax=upr),colour=NA,alpha=0.3,
              fill="blue")+
  geom_ribbon(data=pframe,aes(ymin=boot_lwr,ymax=boot_upr),
              colour=NA,alpha=0.3,
              fill="red")+
  geom_ribbon(data=pframe,aes(ymin=delta_lwr,ymax=delta_upr),
              colour=NA,alpha=0.3,
              fill="cyan")


ggplot(Loblolly,aes(age))+
  geom_hline(yintercept=0,lty=2)+
  geom_ribbon(data=pframe,aes(ymin=lwr-height,ymax=upr-height),
              colour="blue",
              fill=NA)+
  geom_ribbon(data=pframe,aes(ymin=boot_lwr-height,ymax=boot_upr-height),
              colour="red",
              fill=NA)+
  geom_ribbon(data=pframe,aes(ymin=delta_lwr-height,ymax=delta_upr-height),
              colour="cyan",
              fill=NA)
