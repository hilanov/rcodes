n <- 100
x <- 20
r <- x/n
se <- (r * (1 - r)/n)^0.5
ci <- r + qnorm(0.975) * se * c(-1, 1)
prop.test(x, n)

binom.test(x, n)

library(binom)

binom.confint(x, n) 


n <- 100
x <- 0:n
ci <- sapply(x, function(x) {
  c(x/n, binom.test(x, n)$conf.int[1:2])
})
library(plotrix)
plotCI(x/n, ci[1, ], ci[3, ] - ci[1, ], ci[1, ] - ci[2, ])
