library(faraway)
library(lmtest)
library(nlme)
library(mgcv)

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
