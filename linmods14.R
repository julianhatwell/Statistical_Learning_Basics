library(faraway)
library(MASS)
library(ggplot2)
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
     , pch=as.character(sexab$csa)
     , col=as.numeric(sexab$csa)
     , xlab="Fitted"
     , ylab="Residuals")

lmod <- lm(ptsd~cpa, sexab)
sumary(lmod)

data("whiteside", package="MASS")
ggplot(data = whiteside
       , aes(x = Temp, y = Gas)) +
  geom_point() +
  facet_grid(~Insul) +
  geom_smooth(method = "lm")

lmod <- lm(Gas ~ Temp * Insul, data = whiteside)  
sumary(lmod)

# needs centering so not to predict around zero degrees
whiteside$cTemp <- whiteside$Temp - mean(whiteside$Temp)
lmodc <- lm(Gas ~ cTemp * Insul, data = whiteside)  
sumary(lmodc)

data("fruitfly", package="faraway")
plot(longevity ~ thorax, pch = unclass(activity), col = unclass(activity), data = fruitfly)
legend(0.63, 100, levels(fruitfly$activity), pch = 1:5, col = 1:5)

ggplot(data = fruitfly, aes(x = thorax, y = longevity)) +
  geom_point() +
  facet_wrap(~ activity)

lmod <- lm(longevity ~ thorax * activity, data = fruitfly)
sumary(lmod)
model.matrix(lmod)
plot(lmod)
anova(lmod)

lmodp <- lm(longevity ~ thorax + activity, data = fruitfly)
drop1(lmodp, test="F")
sumary(lmodp)
plot(residuals(lmodp) ~ fitted(lmodp), pch = unclass(activity), col = unclass(activity), data = fruitfly)
abline(h=0)
# remove heteroskedast
lmodl <- lm(log(longevity) ~ thorax + activity, data = fruitfly)
sumary(lmodl)
plot(residuals(lmodl) ~ fitted(lmodl), pch = unclass(activity), col = unclass(activity), data = fruitfly)
abline(h=0)
coef(lmodl)[3:6] # additive increase to log
exp(coef(lmodl)[3:6]) # multiplicative effect on raw measure

lmodh <- lm(thorax ~ activity, data = fruitfly)
anova(lmodh) # demonstrates that randomisation in trial does not bias the result estimate of activity parameters

lmodu <- lm(log(longevity) ~ activity, data = fruitfly)
sumary(lmodu) # standard errors have increased - loss of precision

data("teengamb")
names(teengamb)
teengamb$sexf <- factor(teengamb$sex)
lmod <- lm(status ~ sexf, data = teengamb)
sumary(lmod) # interaction sex and status
lmod <- lm(income ~ sexf, data = teengamb)
sumary(lmod) # no interaction sex and income
lmod <- lm(verbal ~ sexf, data = teengamb)
sumary(lmod) # no interaction sex and verbal

lmod1 <- lm(gamble ~ verbal + income + status + sexf, data = teengamb)
sumary(lmod1)
lmod2 <- lm(gamble ~ verbal + income + status * sexf, data = teengamb)
sumary(lmod2)
anova(lmod1, lmod2)

lmod1 <- lm(gamble ~ income + status + sexf, data = teengamb)
sumary(lmod1)
lmod2 <- lm(gamble ~ income + status * sexf, data = teengamb)
sumary(lmod2)
anova(lmod1, lmod2)

plot(gamble~sexf, data = teengamb)

plot(residuals(lmod2)~fitted(lmod2), col = teengamb$sexf)
plot(gamble~status, data=teengamb)
plot(log(gamble+0.1)~status, data=teengamb)

lmod1 <- lm(log(gamble+0.1) ~ verbal + income + status + sexf, data = teengamb)
sumary(lmod1)
lmod2 <- lm(log(gamble+0.1) ~ verbal + income + status * sexf, data = teengamb)
sumary(lmod2)
anova(lmod1, lmod2)
plot(residuals(lmod1)~fitted(lmod1), col = teengamb$sexf)

data("infmort")
names(infmort)
summary(infmort)
hist(infmort$mortality)
hist(infmort$income)
infmort$logmort <- log(infmort$mortality)
infmort$logincome <- log(infmort$income)
hist(infmort$logmort)
hist(infmort$logincome)

infmort$region <- relevel(infmort$region, ref = "Europe")

plot(logincome~region, data=infmort)
plot(logmort~region, data=infmort)
plot(logincome~oil, data=infmort)
plot(logmort~oil, data=infmort)
plot(logmort~logincome, data=infmort)

lmod1 <- lm(logmort ~ logincome + region + oil, data = infmort)
sumary(lmod1)
plot(residuals(lmod1)~fitted(lmod1), col = infmort$region)
plot(lmod1, which = 5)

lmod1 <- lm(logmort ~ logincome + region + oil, data = infmort
            , subset = !grepl("Saudi_Arabia*", rownames(infmort)) )
sumary(lmod1)
plot(residuals(lmod1)~fitted(lmod1), col = infmort$region)
plot(lmod1, which = 5)
exp(coef(lmod1))
lmod1 <- lm(logmort ~ logincome + region + oil, data = infmort
            , subset = !grepl("Saudi_Arabia*|Libya*", rownames(infmort)) )
sumary(lmod1)
plot(residuals(lmod1)~fitted(lmod1), col = infmort$region)
plot(lmod1, which = 5)
lmod1 <- lm(logmort ~ logincome + region, data = infmort
            , subset = !grepl("Saudi_Arabia*|Libya*", rownames(infmort)) )
sumary(lmod1)
plot(residuals(lmod1)~fitted(lmod1), col = infmort$region)
plot(lmod1, which = 5)

exp(coef(lmod1))
pred_func_inc <- function(x) exp(6.331055 + 0.044496 + log(x) *(-0.443217)) # Australia
inc <- seq (min(infmort$income), max(infmort$income), 1)
plot(pred_func_inc(inc)~inc, type="l")

data("ToothGrowth")
names(ToothGrowth)
plot(dose~supp, data=ToothGrowth) # does has been randomised over supp
plot(len~dose, data=ToothGrowth, pch = as.character(supp), col = supp)
# 'centering' dose to 1.0, then it's half and double for the other values
ToothGrowth$dosec <- ToothGrowth$dose - 1.0
lmod <- lm(len ~ dosec + supp, data = ToothGrowth)
sumary(lmod)
plot(len~dosec, data = ToothGrowth, pch = as.character(supp), col = supp)
abline(coef(lmod)[1], coef(lmod)[2])
abline(coef(lmod)[1] + coef(lmod)[3], coef(lmod)[2], col = 2)

#
plot(lmod, which = 1)
plot(lmod, which = 2)
plot(lmod, which = 3)
plot(lmod, which = 5)

t.test(ToothGrowth$len[ToothGrowth$supp == "OJ"], ToothGrowth$len[ToothGrowth$supp == "VC"])
# t-test shows that we can't leave out the dose variable, could we or would we leave out supp? That's prob what we're testing
lmod1 <- lm(len ~ dosec, data = ToothGrowth)
sumary(lmod1) # dose is still signif, at the same coef - because of randomisation

lmod2 <- lm(len ~ supp, data = ToothGrowth)
sumary(lmod2) # supp no longer signif, at the same coef - because of standard errors

lmodi <- lm(len ~ dosec * supp, data = ToothGrowth)
sumary(lmodi) # the interaction appears to be significant
anova(lmod, lmodi)
plot(len~dosec, data = ToothGrowth, pch = as.character(supp), col = supp)
abline(coef(lmodi)[1], coef(lmodi)[2])
abline(coef(lmodi)[1] + coef(lmodi)[3], coef(lmodi)[2] + coef(lmodi)[4], col = 2)
plot(lmodi, which = 1)
plot(lmodi, which = 2)
plot(lmodi, which = 3)
plot(lmodi, which = 5)

# still a bit misbehaving resids - add a quad term
lmodq <- lm(len ~ poly(dosec, 2) * supp, data = ToothGrowth)
sumary(lmodq) # the interaction appears to be significant
anova(lmod, lmodi, lmodq)
plot(len~dosec, data = ToothGrowth, pch = as.character(supp), col = supp)
pred_mat <- unique(data.frame(supp = as.character(ToothGrowth$supp)
                              , dosec = ToothGrowth$dosec))
pred_mat$supp <- factor(pred_mat$supp)
pred_mat$preds <- predict(lmodq, pred_mat)
plot(len~dosec, data = ToothGrowth, pch = as.character(supp), col = supp)
lines(preds~dosec, data = pred_mat[pred_mat$supp == "OJ", ])
lines(preds~dosec, data = pred_mat[pred_mat$supp == "VC", ], col = 2)
plot(lmodi, which = 1)
plot(lmodi, which = 2)
plot(lmodi, which = 3)
plot(lmodi, which = 5)

ggplot(data = ToothGrowth
       , aes(x = dosec, y = len, color = supp)) +
  geom_point() +
  geom_smooth(method = "loess", lty = 3) +
  geom_line(data = pred_mat
            , aes(y = preds))

ggplot(data = ToothGrowth
       , aes(x = log(dose), y = len, color = supp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_smooth(method = "loess", se = FALSE, lty = 3)

# side note - orthogonality of polynoms
poly(ToothGrowth$dosec, 2)
z <- poly(ToothGrowth$dosec, 2)
matplot(cbind(ToothGrowth$dosec, ToothGrowth$dosec^2, predict(z, ToothGrowth$dosec)), type = "l")

data("chredlin", package="faraway")
ggplot(data = chredlin
       , aes(x = side, y = involact)) +
  geom_point(position = position_jitter(width = 0.2
                                        , height = 0)) +
  stat_summary(geom = "errorbar", colour = "red")

lmods <- lm(involact ~ race + side
            , data = chredlin)
t.test(chredlin$involact[chredlin$side == "s"], chredlin$involact[chredlin$side == "n"])
  