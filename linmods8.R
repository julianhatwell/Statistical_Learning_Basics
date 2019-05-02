library(faraway)
library(lmtest)
library(nlme)
library(mgcv)
library(quantreg)
library(ggplot2)
library(MASS)
# correlated resids
data("globwarm")
lmod <- lm(nhtemp~wusa+jasper+westgreen+chesapeake+
             tornetrask+urals+mongolia+tasman, data = globwarm)
sumary(lmod)
cor(resid(lmod)[-1], resid(lmod)[-length(resid(lmod))])
plot(resid(lmod)[-1], resid(lmod)[-length(resid(lmod))])
dwtest(lm(nhtemp~wusa+jasper+westgreen+chesapeake+
     tornetrask+urals+mongolia+tasman, data = globwarm))
# do a gls
glmod <- gls(nhtemp~wusa+jasper+westgreen+chesapeake+
                 tornetrask+urals+mongolia+tasman
             , correlation = corAR1(form = ~year)
             , data = na.omit(globwarm))
# phi is autocorrelation with year.
intervals(glmod, which = "var-cov")

# within blocks correlated resids
glmod <- gls(yield ~ variety, data = oatvar
             , correlation = corCompSymm(form = ~1 | block))
summary(glmod)
intervals(glmod, which = "var-cov")

# correlated with population size
data(fpe)
lmod <- lm(A2~A+B+C+D+E+F+G+H+J+K+N-1, weights = 1/EI, data=fpe)
coef(lmod)
wlmod <- lm(A2~A+B+C+D+E+F+G+H+J+K+N-1, data=fpe)
coef(wlmod)
# don't allow weights above 1. Crude method: offset = 1, omit negatives
lmod <- lm(A2~offset(A+G+K)+C+D+E+F+N-1, weights = 1/EI, data=fpe)
coef(lmod)

# more sophisticated solution
lmod <- lm(A2~A+B+C+D+E+F+G+H+J+K+N-1, weights = 1/EI, data=fpe)
M<-list(w=1/fpe$EI, X=model.matrix(lmod), y=fpe$A2, Ain=rbind(diag(11), -diag(11))
        , C=matrix(0,0,0), array(0,0), S=list(), off=NULL, p = rep(0.5, 11)
        , bin=c(rep(0, 11), rep(-1, 11)))
a <- pcls(M)
names(a) <- colnames(model.matrix(lmod))
round(a, 3)

# unequal variance
lmod <- lm(dist~speed, cars)
plot(resid(lmod))
plot(resid(lmod)~fitted(lmod))
plot(resid(lmod)~speed, data=cars)
glmod <- gls(dist~speed, cars, weights = varConstPower(1, form = ~speed))
summary(glmod)

# finding estimate of true variance from multiple indep measures having same value
data("corrosion")
plot(loss~Fe, data=corrosion, xlab="Iron Content", ylab="Corrosion")
lmod <- lm(loss~Fe, data=corrosion)
sumary(lmod)
abline(coef(lmod))
lmoda <- lm(loss ~ factor(Fe), data=corrosion) # dummy variable for each distinct value - saturated
points(corrosion$Fe, fitted(lmoda), pch=3)
anova(lmod, lmoda) # lack of fit because pure error is low.

data("gala")
lmod <- lm(Species~ Area+
             Elevation+
             Nearest+
             Scruz+Adjacent
           , data = gala)
sumary(lmod)
rlmod <- rlm(Species~ Area+
             Elevation+
             Nearest+
             Scruz+Adjacent
           , data = gala)
summary(rlmod)
wts <- rlmod$w
names(wts) <- rownames(gala)
head(sort(wts),10)

l1mod <- rq(Species~ Area+
            Elevation+
            Nearest+
            Scruz+Adjacent
          , data = gala)
summary(l1mod)

set.seed(123)
ltsmod <- ltsreg(Species~ Area+
              Elevation+
              Nearest+
              Scruz+Adjacent
            , data = gala)
coef(ltsmod)

ltsmod <- ltsreg(Species~ Area+
                   Elevation+
                   Nearest+
                   Scruz+Adjacent
                 , data = gala
                 , nsamp="exact")

coef(ltsmod)
bcoef <- matrix(0, 1000, 6)
for (i in 1:1000) {
  newy <- predict(ltsmod) +
    resid(ltsmod)[sample(30, replace = TRUE)]
  brg <- ltsreg(newy ~ Area+
                  Elevation+
                  Nearest+
                  Scruz+Adjacent
                , data = gala
                , nsamp="best")
  bcoef[i, ] <- coef(brg)
}
colnames(bcoef) <- names(coef(ltsmod))
cred.ints <- apply(bcoef, 2, function(x) quantile(x, c(0.025, 0.975)))
# predictor <- "Area"
predictor <- "Adjacent"
bcoef <- data.frame(bcoef)
ggplot(data=bcoef, aes(x=Adjacent)) +
  geom_density() +
  geom_vline(xintercept = cred.ints[, predictor]
             , linetype = "dashed") +
  xlim(cred.ints[, predictor] * c(0.975,1.025))

plot(lmod, which=5)

lmodi <- lm(Species ~ Area+
               Elevation+
               Nearest+
               Scruz+Adjacent
             , data = gala
             , subset = rownames(gala) != "Isabela")
summary(lmodi)

data("star")
plot(light ~ temp, star)
gs1 <- lm(light ~ temp, star)
abline(coef(gs1))
gs2 <- rlm(light ~ temp, star)
abline(coef(gs2), lty = 2)
gs3 <- ltsreg(light ~ temp, star, nsamp="exact")
abline(coef(gs3), lty = 5)

# exercises
# finding weights for gls wls
data("pipeline")
plot(Lab ~ Field, data=pipeline)
lmod <- lm(Lab ~ Field, data=pipeline)
plot(lmod)
# test constant variances
sumary(lm(sqrt(abs(residuals(lmod))) ~ fitted(lmod)))
# create groups
i <- order(pipeline$Field)
npipe <- pipeline[i, ]
ff <- gl(12, 9)[-108] # generate factor levels
meanfield <- unlist(lapply(split(npipe$Field, ff), mean))
varlab <- unlist(lapply(split(npipe$Lab, ff), var))
plot(varlab~meanfield)
plot(log(varlab[-12])~log(meanfield[-12]))
# estimate variance parameters for a_0 * pred ^ a_1
lm(log(varlab[-12])~log(meanfield[-12]))
glmod <- gls(Lab ~ Field
             , data=pipeline
             , weights = varConstPower(
               const = 1.935
               , power = 1.671
               , form = ~Field))
summary(glmod) # not sure about this


# consider transformations instead
plot(log(Lab) ~ log(Field), data=pipeline)
plot(sqrt(Lab) ~ sqrt(Field), data=pipeline)
plot(I(1/Lab) ~ I(1/Field), data=pipeline)
# log log might work.
lmod <- lm(log(Lab) ~ log(Field), data=pipeline)
# variance is no longer significant
sumary(lm(sqrt(abs(residuals(lmod))) ~ fitted(lmod)))

data("divusa")
lmod <- lm(divorce~ unemployed +
             femlab +
             marriage +
             birth +
             military
           , data = divusa)
sumary(lmod)
plot(lmod)
# plot subsequent pairs of resids
n <- length(residuals(lmod))
plot(
  tail(residuals(lmod), n-1) ~ head(residuals(lmod), n-1)
  , xlab = expression(hat(epsilon)[i])
  , ylab = expression(hat(epsilon)[i + 1])
)
abline(h=0, v=0, col=grey(0.75))
cor(resid(lmod)[-1], resid(lmod)[-length(resid(lmod))])
plot(resid(lmod)[-1], resid(lmod)[-length(resid(lmod))])
dwtest(lmod)
# gls to allow correlated errors
glmod <- gls(divorce~ unemployed +
              femlab +
              marriage +
              birth +
              military
            , data = divusa
            , correlation = corAR1()
            #, correlation = corCAR1(form = ~year) # same
            , method = "ML")
summary(glmod)

data("salmonella")
lmod <- lm(colonies ~ log(dose +1), data= salmonella)
summary(lmod)
plot(colonies ~ log(dose +1), data= salmonella)
abline(lmod)
lmoda <- lm(colonies ~ factor(log(dose +1)), data= salmonella)
points(log(salmonella$dose +1)
       , fitted(lmoda)
       , pch = 3
       , col = "red")
anova(lmod, lmoda)
sqrt(1091.3/4) # compare to resid st.err of 10.84
summary(lmoda)$r.squared # this is poor to begin with

data("cars")
lmod <- lm(dist~speed, data=cars)
sumary(lmod)
plot(dist~speed, data=cars)
abline(lmod)
lmoda <- lm(dist~factor(speed), data=cars)
points(cars$speed
       , fitted(lmoda)
       , pch = 3
       , col = "red")
anova(lmod, lmoda)
sqrt(6764.8/17) # compare to resid st.err of 15.37959
summary(lmoda)$r.squared

data("stackloss")
lmod <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.
           , data = stackloss)
ladmod <- rq(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.
             , data = stackloss)
hubmod <- rlm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.
              , data = stackloss)
ltsmod <- ltsreg(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.
              , data = stackloss)
ltsmod <- ltsreg(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.
                 , data = stackloss, nsamp="exact")

coef(ltsmod)

summary(lmod)
summary(ladmod)
summary(hubmod)
coef(ltsmod)

bcoef <- matrix(0, 1000, 4)
for (i in 1:1000) {
  newy <- predict(ltsmod) +
    resid(ltsmod)[sample(21, replace = TRUE)]
  brg <- ltsreg(newy ~ Air.Flow + Water.Temp + Acid.Conc.
                , data = stackloss
                , nsamp="best")
  bcoef[i, ] <- coef(brg)
}
colnames(bcoef) <- names(coef(ltsmod))
cred.ints <- apply(bcoef, 2, function(x) quantile(x, c(0.025, 0.975)))
cred.ints

plot(lmod, which=5) # point 21 is influential outlier
lmod <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.
           , data = stackloss[-21, ])
summary(lmod)

data("cheddar")
lmod <- lm(taste ~ Lactic + H2S + Acetic, data = cheddar)
cheesetime <- 1:nrow(cheddar)
plot(resid(lmod)~cheesetime)
glmod <- gls(taste ~ Lactic + H2S + Acetic
          , data = cheddar
          , correlation = corAR1(form = ~cheesetime)
          , method = "ML")
summary(glmod)
intervals(glmod, which = "var-cov")
sumary(lmod)
dwtest(lmod) # there is barely any correl

lmod <- lm(taste ~ Lactic + H2S + Acetic + cheesetime
            , data = cheddar)
sumary(lmod)

data("crawl")
View(crawl)
lmod <- lm(crawling~temperature, data = crawl)
sumary(lmod)
plot(lmod)
# variance appears to be constanct
sumary(lm(sqrt(abs(residuals(lmod))) ~ fitted(lmod)))
# this doesn't seem appropriate because SD is not correlated to n
lmod <- lm(crawling~temperature, data = crawl, weights = crawl$n)
sumary(lmod)
# SD suggests varying quality of score, so this is better
lmod <- lm(crawling~temperature, data = crawl, weights = 1/crawl$SD)
sumary(lmod)

data("gammaray")
View(gammaray)
# there is a measurement error
plot(flux~time, data=gammaray)
plot(log(flux)~log(time), data=gammaray)

lmod <- lm(flux~log(time)
           , data = gammaray
           , weights = 1/gammaray$error)
sumary(lmod)

data("fat", package = "faraway")
lmod <- lm(brozek~age + weight + height + neck + 
             chest + abdom + hip + thigh + knee +
             ankle + biceps + forearm + wrist
           , data = fat)

rlmod <- rlm(brozek~age + weight + height + neck + 
             chest + abdom + hip + thigh + knee +
             ankle + biceps + forearm + wrist
           , data = fat)
wts <- rlmod$w
names(wts) <- rownames(fat)
head(sort(wts), 40) # top two are among the largest resids
head(sort(resid(rlmod)), 40)
x <- model.matrix(lmod)
x0 <- apply(x, 2, median)
x0
x[224, ]
fat$brozek[224]
x[207, ]
fat$brozek[207]
# these guys are close to the median predictors but very high or very low on the response
plot(weight~height, data =fat) # not the same outliers
plot(rlmod)
