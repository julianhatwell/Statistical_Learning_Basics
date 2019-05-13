library(faraway)
library(MASS)
library(pls)

data("fat", package = "faraway")
plot(neck ~ knee, data = fat)
plot(chest ~ thigh, data = fat)
plot(hip ~ wrist, data = fat)

cfat <- fat[, 9:18]
prfat <- prcomp(cfat)
dim(prfat$rot)
dim(prfat$x)
summary(prfat)

round(prfat$rot[,1], 2)
prfat$rot

# centre, scale and princomp
cfat <- fat[, 9:18]
prfat <- prcomp(cfat, scale. = TRUE)
summary(prfat)

round(prfat$rot[,1], 2)
round(prfat$rot[,2], 2)

robfat <- cov.rob(cfat)
md <- mahalanobis(cfat, center = robfat$center, cov = robfat$cov)
n <- nrow(cfat); p <- ncol(cfat)

plot(qchisq(1:n/(n+1), p), sort(md)
     , xlab = expression(paste(chi^2, "quantiles"))
     , ylab = "sorted mahalanobis distances")
abline(0,1)

which(md > 50)
cfat.rob <- cfat[-which(md > 50), ]
prfat.rob <- prcomp(cfat.rob, scale. = TRUE)
summary(prfat.rob)

round(prfat$rot[,1], 2)
round(prfat$rot[,2], 2)

lmoda <- lm(fat$brozek~., data = cfat)
sumary(lmoda)
lmodpcr <- lm(fat$brozek ~ prfat$x[, 1:2])
sumary(lmodpcr)

lmod <- lm(fat$brozek ~ scale(abdom) +
             I(scale(ankle) - scale(abdom))
           , data = cfat)
sumary(lmod)

data("meatspec")
trainmeat <- meatspec[1:172, ]
testmeat <- meatspec[173:215, ]
lmod <- lm(fat ~ ., data = trainmeat)
sumary(lmod)$r.squared
rmse <- function(x, y) {
  sqrt(mean((x-y)^2))
}
rmse(lmod$fitted.values, trainmeat$fat)
rmse(predict(lmod, testmeat), testmeat$fat)

lmod_step <- step(lmod)
rmse(lmod_step$fitted.values, trainmeat$fat)
rmse(predict(lmod_step, testmeat), testmeat$fat)

meatpca <- prcomp(trainmeat[, -101])
round(meatpca$sdev, 3)
matplot(1:100, meatpca$rotation[, 1:3]
        , type = "l"
        , xlab = "frequency"
        , col = 1)

lmod_pls <- pcr(fat ~ .
                , data = trainmeat
                , ncomp = 50)
rmse(predict(lmod_pls
             , newdata = testmeat
             , ncomp=4), testmeat$fat)

plot(lmod$coefficients[-1]
     , xlab="Frequency"
     , ylab = "Coefficient"
     , type="l")
coefplot(lmod_pls, ncomp = 4
         , xlab="Frequency"
         , ylab = "Coefficient")

plot(meatpca$sdev[1:10]
     , type = "l"
     , xlab = "PC"
     , ylab = "SD ov PC")

pcrmse <- RMSEP(lmod_pls, newdata = testmeat)
plot(pcrmse, main = "")
which.min(pcrmse$val)
pcrmse$val[which.min(pcrmse$val)]

set.seed(123)
lmod_pls_cv <- pcr(fat~., data = trainmeat
                   , validation="CV"
                   , ncomp = 50)
pcr_cv <- RMSEP(lmod_pls_cv, estimate = "CV")
plot(pcr_cv, main= "")
which.min(pcr_cv$val)
pcr_cv$val[which.min(pcr_cv$val)]

rmse(predict(lmod_pls_cv
             , newdata = testmeat
             , ncomp=18), testmeat$fat)

set.seed(123)
plsmod <- plsr(fat~.
               , data=trainmeat
               , ncomp=50
               , validation="CV")

coefplot(plsmod, ncomp=4, xlab="Frequency")
plsCV <- RMSEP(plsmod, estimate="CV")
plot(plsCV, main = "")

ypred <- predict(plsmod, ncomp=15)
rmse(ypred, trainmeat$fat)

ytpred <- predict(plsmod, newdata=testmeat, ncomp=15)
rmse(ytpred, testmeat$fat)
