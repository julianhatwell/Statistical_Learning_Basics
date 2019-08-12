library(faraway)
library(MASS)
library(ggplot2)
library(quantreg)
library(lmtest)

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

t.test(chredlin$involact[chredlin$side == "s"]
       , chredlin$involact[chredlin$side == "n"])

lmodnull <- lm(involact~1, data=chredlin)

lmodside <- lm(involact ~ side, data=chredlin)

lmods <- lm(involact ~ race + side
            , data = chredlin)
anova(lmodside)
anova(lmodnull, lmodside, lmods)
summary(lmods)
lmodfull <- lm(involact ~ race +
                       fire + theft +
                       age + log(income)
               , data = chredlin)
anova(lmodfull)
lmodfull <- lm(involact ~ race +
                       fire + theft +
                       age + side
               , data = chredlin)
anova(lmodfull)

# the idea is to see how covariates affect race in the neighbourhood
# should we adjust the response - are the conclusions robust?
listcombo <- unlist(
        sapply(0:5
               , function(x) {
                       combn(5, x, simplify = FALSE)
               })
        , recursive = FALSE)

predterms <- lapply(listcombo, function(x) {
        paste(c("race", c("fire", "theft", "age", "log(income)", "side")[x])
              , collapse = "+")})
coefm <- matrix(NA, 32, 2)
for (i in 1:32) {
        lmi <- lm(as.formula(paste("involact ~ ", predterms[[i]]))
                  , data = chredlin)
        coefm[i, ] <- sumary(lmi)$coef[2, c(1,4)]
}
rownames(coefm) <- predterms
colnames(coefm) <- c("beta", "pvalue")
round(coefm, 4) # we are looking at how stable the race variable was
# significance is robust to adding side

data("uswages")
names(uswages)
lmodfull <- lm(wage ~ educ + exper + 
               race + smsa + ne +
               we + so + pt # exclude mw - the regions are already dummy coded
               , data = uswages)  
summary(lmodfull)               
# looks like we can exclude similar regions
lmodreg <- lm(wage ~ educ + exper + 
               race + smsa + 
               we + pt # exclude mw - the regions are already dummy coded
               , data = uswages)  
summary(lmodreg)               
# everything else is significant

# check model accumptions
plot(lmodreg, which = 1) # heteroskedastic
plot(lmodreg, which = 2)

# better start again
ggplot(data = uswages, aes(x = educ, y = wage)) +
        geom_point() +
        stat_smooth(method = lm)
ggplot(data = uswages, aes(x = educ, y = log(wage))) +
        geom_point() +
        stat_smooth(method = lm)
# exper looks like a poly 2
ggplot(data = uswages, aes(x = exper, y = wage)) +
        geom_point() +
        stat_smooth(method = lm)
ggplot(data = uswages, aes(x = exper, y = log(wage))) +
        geom_point() +
        stat_smooth(method = lm)

lmodt <- lm(log(wage) ~ educ + poly(exper, 2) + 
                    race + smsa + ne +
                    we + so + pt
            , data = uswages)
sumary(lmodt)
# it seems that none of the regions are signif
lmodr <- lm(log(wage) ~ educ + poly(exper, 2) + 
                    race + smsa + pt
            , data = uswages)
sumary(lmodr)
anova(lmodr, lmodt) # we can def do without region
plot(lmodr, which = 1)
plot(lmodr, which = 2) # this is not great
plot(lmodr, which = 3)
plot(lmodr, which = 5)
termplot(lmodr
         , partial.resid = TRUE
         , terms = 1)
termplot(lmodr
         , partial.resid = TRUE
         , terms = 2)
termplot(lmodr
         , partial.resid = TRUE
         , terms = 3)
termplot(lmodr
         , partial.resid = TRUE
         , terms = 4)
termplot(lmodr
         , partial.resid = TRUE
         , terms = 5)

plot(density(resid(lmodr))) # could resid outliers be a problem
head(sort(cooks.distance(lmodr))) # these are tiny

matplot(dfbeta(lmodr)
     , ylab="Changes in Coef")
which.max(dfbeta(lmodr)[,4])

# try without this one point
lmodro <- lm(log(wage) ~ educ + poly(exper, 2) + 
                    race + smsa + pt
            , data = uswages[-1576, ])
sumary(lmodro)
plot(lmodro, which = 2) # not making this much better. signs of long tail distr


# we have a large sample, so it's probably OK, but...
# try robust method
rlmodr <- rlm(log(wage) ~ educ + poly(exper, 2) + 
                      race + smsa + pt
              , data = uswages)
summary(rlmodr)
head(sort(rlmodr$w), 50) # a lot of instances have been downweighted

# lad regression
lmodlad <- rq(log(wage) ~ educ + poly(exper, 2) + 
                      race + smsa + pt
              , data = uswages)


coef(lmodr)
coef(rlmodr) # very close
coef(lmodlad) # slightly less but still very close
# just accept the LSq model

data(clot)
plot(time~conc
     , col = clot$lot
     , pch = as.character(clot$lot)
     , data = clot)

plot(I(1/time)~conc
     , col = clot$lot
     , pch = as.character(clot$lot)
     , data = clot)

plot(time~log(conc)
     , col = clot$lot
     , pch = as.character(clot$lot)
     , data = clot)

plot(I(1/time)~I(log(conc))
     , col = clot$lot
     , pch = as.character(clot$lot)
     , data = clot)
     
lmod <- lm(I(1/time)~(log(conc)) * lot
           , data = clot)
sumary(lmod)
plot(lmod, which = 1)
plot(lmod, which = 2)
abline(0,1) # looks like short tailed, not serious
plot(lmod, which = 3)
plot(lmod, which = 5)
shapiro.test(resid(lmod)) # normal resids
dfb <- dfbetas(lmod)
matplot(dfb[,2:4])
halfnorm(hatvalues(lmod))
summary(lmod)
lo <- factor(c("one", "two"))
# when do they predict the smae
plot(I(1/time)~I(log(conc))
     , col = clot$lot
     , pch = as.character(clot$lot)
     , data = clot
     , xlim = c(0, 5)
     , ylim = c(-0.1, 0.1))
abline(coef(lmod)[1:2])
abline(coef(lmod)[1] + coef(lmod)[3]
       , coef(lmod)[2] + coef(lmod)[4]
       , col = "red")

# when does the slope cancel out the intercept for lot two?
xtest <- clot[c(1, 10), ]
xtest$conc <- exp(-coef(lmodt)[3]/coef(lmodt)[4]) # raw concentration level
1/predict(lmod, xtest)

data(fortune)
plot(wealth~age, data = fortune
     , pch = as.character(fortune$region)
     , col = fortune$region)
ggplot(data = fortune
       , aes(x = age, y = log(wealth), colour = region)) +
        geom_point() +
        stat_smooth(method = lm) +
        facet_grid(.~region)
lmod <- lm(log(wealth)~age + region, data=fortune)
sumary(lmod)
glmod <- glm(wealth~age + region, data=fortune
             , family = poisson)
summary(glmod)

data(hips)
plot(faft~fbef, data = hips
     , col = hips$grp
     , pch = as.character(hips$grp))
lmod0 <- lm(faft~1, data = hips)
lmod1 <- lm(faft~fbef, data = hips)
lmod2 <- lm(faft~fbef+grp, data = hips)
lmod3 <- lm(faft~fbef*grp, data = hips)
anova(lmod0, lmod1, lmod2, lmod3)
sumary(lmod2)
t.test((hips$faft - hips$fbef)[hips$grp == "treat"]
       , (hips$faft - hips$fbef)[hips$grp == "control"])
dfbetas(lmod2)
hatv <- hatvalues(lmod2)
names(hatv) <- rownames(hips)
tail(sort(hatv)) # suddenly double the size, these resids
halfnorm(hatvalues(lmod2), nlab = 3)
plot(lmod2, which = 1)
plot(resid(lmod2)[-c(49, 50, 70)]~fbef
     , data = hips[-c(49, 50, 70), ]
     , ylim = c(-30, 25)
     , xlim = c(75, 140))
points(resid(lmod2)[c(49, 50, 70)]~fbef
       , data = hips[c(49, 50, 70), ]
       , col = "red")
plot(lmod2, which = 3)
plot(lmod2, which = 5) # possibly 3 high leverage points
shapiro.test(resid(lmod2)) # reject normal distrib
qqnorm(rstandard(lmod2))
abline(0, 1)
plot(lmod2, which = 2)
qt(0.95, summary(lmod2)$df[2]) # nothing is strictly an outlier

lmod4 <- lm(faft~fbef+grp
            , data = hips[-c(49, 50, 70), ])
sumary(lmod4)
t.test((hips$faft - hips$fbef)[hips$grp == "treat"][-c(49, 50, 70)]
       , (hips$faft - hips$fbef)[hips$grp == "control"][-c(49, 50, 70)])
confint(lmod4)[3, ] # not sure why the t-test is no good here

# both legs have been included, so we probably have correlated resids
dwtest(faft~fbef+grp
       , data = hips) # yes some autocorrel
agg <- aggregate(hips[,c("fbef", "faft")]
          , list(hips$person)
          , mean)
status <- unique(hips[, c("grp", "person")])
rownames(status) <- status$person
agg$grp <- status[agg$Group.1, "grp"]
lmod5 <- lm(faft~fbef+grp, data = agg)
sumary(lmod5)
coef(lmod4)[3]
confint(lmod4)[3, ] # not sure why the t-test is no good here
coef(lmod5)[3]
confint(lmod5)[3, ]
# confint is larger, but so is effect