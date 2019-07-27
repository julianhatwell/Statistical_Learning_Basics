library(faraway)
data(sexab)
by(sexab, sexab$csa, summary)
plot(ptsd~csa, sexab)
plot(ptsd~cpa
     , col=csa
     , pch=as.character(csa)
     , sexab)
t.test(ptsd~csa, sexab, var.equal=TRUE)

# manually create dummy vars
d1 <- ifelse(sexab$csa == "Abused", 1, 0)
d2 <- ifelse(sexab$csa == "NotAbused", 1, 0)
lmod <- lm(ptsd ~ d1 + d2, sexab)
sumary(lmod) # note the singularity because matrix is not full rank
model.matrix(lmod)
lmod <- lm(ptsd ~ d2, sexab) # eliminate one term
sumary(lmod)

lmod <- lm(ptsd ~ d1 + d2 - 1, sexab) # eliminate the intercept
sumary(lmod) # these t-tests assume the mean is zero. This is not useful (or believable)

# we know R will form the dummy var directly
lmod <- lm(ptsd ~ csa, sexab)
sumary(lmod)

sexab$csa <- relevel(sexab$csa, ref="NotAbused")
lmod <- lm(ptsd ~ csa, sexab)
sumary(lmod)

# interaction
lmod <- lm(ptsd~cpa+csa+cpa:csa, data=sexab)
sumary(lmod)
model.matrix(lmod)
plot(ptsd~cpa, sexab
     , pch=as.character(csa)
     , col=csa)
abline(coef(lmod)[1], coef(lmod)[2])
abline(coef(lmod)[1] + coef(lmod)[3]
       , coef(lmod)[2] + coef(lmod)[4]
       , col=2)
abline(coef(lmod)[1] + coef(lmod)[3]
       , coef(lmod)[2] # interaction term was not signif
       , col=2
       , lty = 2)
confint(lmod)[3, ]

plot(fitted(lmod), resid(lmod)
     , pch=as.numeric(sexab$csa)
     , xlab="Fitted"
     , ylab="Residuals")

lmod <- lm(ptsd~cpa, sexab)
sumary(lmod)
