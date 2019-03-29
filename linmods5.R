library(faraway)
library(Matching)
data("gala")
lmod <- lm(Species ~ Area +
             Elevation +
             Nearest +
             Scruz +
             Adjacent
           , data=gala)
sumary(lmod)
lmod2 <- lm(Species ~ Elevation, data=gala)
sumary(lmod2)
plot(Species ~ Elevation, data = gala)
abline(lmod2)
colMeans(gala)
cm <- colMeans(gala)[-(1:2)]
fixed_means <- data.frame(
       Area = rep(cm[1], nrow(gala))
       , Elevation = gala$Elevation
       , Nearest = rep(cm[3], nrow(gala))
       , Scruz = rep(cm[4], nrow(gala))
       , Adjacent = rep(cm[5], nrow(gala))
     )
i <- order(gala$Elevation)
p <- predict(lmod, newdata = fixed_means[i, ])
lines(gala$Elevation[i], p, lty = 2, col = "blue")
# abline(lmod$coefficients[1], lmod$coefficients[3]) not correct according to book


data("newhamp")
colSums(newhamp[newhamp$votesys == "D", 2:3])
colSums(newhamp[newhamp$votesys == "H", 2:3])
newhamp$trt <- ifelse(newhamp$votesys == "H", 1, 0)
lmodu <- lm(pObama ~ trt, data = newhamp)
sumary(lmodu)
# is trt (voting method) significant?
# there could be a confounding variable
# that is a variable whose effect depends
# on our independent variable

lmodz <- lm(pObama ~ trt + Dean, data = newhamp)
sumary(lmodz)
# all of a sudden trt is not signif.
sumary(lm(trt ~ Dean, data = newhamp))
# the confounding var is related to voting method (trt)

# to get around this, randomization could be use
# but it is not always safe to assume balance
# another option is matching:
# match similar units who then have different treatments
set.seed(123)
mm <- GenMatch(newhamp$trt, newhamp$Dean
               , caliper = 0.05
               , ties = FALSE
               , pop.size = 1000)
plot(pObama~Dean, data = newhamp, pch=trt + 1)
with(newhamp, segments(
  Dean[mm$matches[,1]]
  , pObama[mm$matches[,1]]
  , Dean[mm$matches[,2]]
  , pObama[mm$matches[,2]]
))
newhamp[c(4,213), "Dean"] # example

pdiff <- newhamp$pObama[mm$matches[,1]] -
  newhamp$pObama[mm$matches[,2]]
t.test(pdiff) # mean diff CI contains zero
plot(pdiff~newhamp$Dean[mm$matches[,1]]
     , xlab="Dean", ylab="Hand-Digital")
abline(h=0)
abline(lmodz$coefficients[1], lmodz$coefficients[3])
abline(lmodz$coefficients[1] +
         lmodz$coefficients[2]
       , lmodz$coefficients[3]
       , lty = 2, col = "blue")
# so differences in voting method
# do not depend on Dean
# voting for Obama seems to depend on Dean

plot(pObama~Dean, data=newhamp)
sumary(lmodu)
abline(h=c(lmodu$coefficients[1]
           , lmodu$coefficients[1] +
             lmodu$coefficients[2])
       , lty = 1:2, col = c("black", "blue"))
abline(lmodz$coefficients[1], lmodz$coefficients[3])
abline(lmodz$coefficients[1] +
lmodz$coefficients[2]
, lmodz$coefficients[3]
, lty = 2, col = "blue")

with(newhamp, segments(
  Dean[mm$matches[,1]]
  , pObama[mm$matches[,1]]
  , Dean[mm$matches[,2]]
  , pObama[mm$matches[,2]]
  , col = gray(0.5)
))

# ex

coefpval <- function(m, v) {
  sm <- summary(m)
  sm$coefficients[v, c(1,4)]
}

lmod1 <- lm(gamble~sex, data=teengamb)
lmod2 <- lm(gamble~sex + status, data=teengamb)
lmod3 <- lm(gamble~sex + income, data=teengamb)
lmod4 <- lm(gamble~sex + verbal, data=teengamb)
lmod5 <- lm(gamble~sex + status + income, data=teengamb)
lmod6 <- lm(gamble~sex + status + verbal, data=teengamb)
lmod7 <- lm(gamble~sex + income + verbal, data=teengamb)
lmod8 <- lm(gamble~sex + status + income + verbal, data=teengamb)
sumary(lmod1)
sumary(lmod2)
sumary(lmod3)
sumary(lmod4)
sumary(lmod5)
sumary(lmod6)
sumary(lmod7)
sumary(lmod8)

mods <- list(lmod1
             , lmod2
             , lmod3
             , lmod4
             , lmod5
             , lmod6
             , lmod7
             , lmod8)

tabs <- sapply(mods, coefpval, v = "sex")

t(tabs)
plot(t(tabs))
colMeans(t(tabs))
sd(tabs[1,])
sd(tabs[2,])

data("odor")
lmod1 <- lm(odor ~ temp, data = odor)
lmod2 <- lm(odor ~ temp + gas, data = odor)
lmod3 <- lm(odor ~ temp + pack, data = odor)
lmod4 <- lm(odor ~ temp + gas + pack, data = odor)
sumary(lmod1)
sumary(lmod2)
sumary(lmod3)
sumary(lmod4)

mods <- list(lmod1
             , lmod2
             , lmod3
             , lmod4)
tabs <- sapply(mods, coefpval, v="temp")
t(tabs)
plot(t(tabs))
colMeans(t(tabs))
sd(tabs[1,])
sd(tabs[2,])

plot(gamble~income, data = teengamb, pch = teengamb$sex + 1)
lmod3 <- lm(gamble~sex + income, data=teengamb)
abline(lmod3$coefficients[1], lmod3$coefficients[3])
abline(lmod3$coefficients[1]+lmod3$coefficients[2]
       , lmod3$coefficients[3]
       , lty = 2
       , col = "blue")

set.seed(123)
mm <- GenMatch(teengamb$sex, teengamb$income
               , caliper = 0.05
               , ties = FALSE
               , pop.size = 1000)
with(teengamb, segments(
  income[mm$matches[,1]]
  , gamble[mm$matches[,1]]
  , income[mm$matches[,2]]
  , gamble[mm$matches[,2]]
  , col = grey(0.5)
))
mm$matches # 14 matches
pdiff <- teengamb$gamble[mm$matches[,1]] -
  teengamb$gamble[mm$matches[,2]]
t.test(pdiff) # mean diff CI contains zero
plot(pdiff~teengamb$gamble[mm$matches[,1]]
     , xlab="income", ylab="diff-gamble")
abline(h=0)

data("happy")
lmod <- lm(happy~money+sex+love+work
           , data = happy)
table(happy$love)
sumary(lmod) # an increase of 1 in love
# leads to an increse of 1.92 happy
# also highly signif
happy$clove <- ifelse(happy$love < 3, 0, 1)
lmod <- lm(happy~money+sex+clove+work
           , data = happy)
sumary(lmod)
# broadly similar, but slightly greater effect
# if clove is 1 compared to 0
lmod <- lm(happy~clove, data = happy)
sumary(lmod)
# still broadly similar, maybe because
# only work is significant of the other predictors
plot(jitter(happy$happy)~jitter(happy$work)
     , pch=happy$clove+1)
xtabs (~ clove + work, happy)
mns <- tapply(happy$happy, happy$clove, mean)
diff(mns) # same as the model with this as the only predictor
