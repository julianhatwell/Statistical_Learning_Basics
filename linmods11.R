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

# ridge
library(MASS)
rgmod <- lm.ridge(fat ~ .
                  , lambda = seq(0, 5e-8
                                , length.out=21)
                  , data=trainmeat)
which.min(rgmod$GCV)
matplot(rgmod$lambda
        , coef(rgmod)
        , type="l"
        , xlab = expression(lambda)
        , ylab = expression(hat(beta)))
abline(v=as.numeric(names(which.min(rgmod$GCV))))

ypred <- cbind(1, as.matrix(trainmeat[, -101])) %*%
  coef(rgmod)[which.min(rgmod$GCV), ]
rmse(ypred, trainmeat$fat)
ypred <- cbind(1, as.matrix(testmeat[, -101])) %*%
  coef(rgmod)[which.min(rgmod$GCV), ]
rmse(ypred, testmeat$fat) # quite bad

c(ytpred[13], ypred[13], testmeat$fat[13])
# one bad instance, but how would you find this in practice?
rmse(ypred[-13], testmeat$fat[-13])

# lasso
library(lars)
data(state)
statedata <- data.frame(state.x77, row.names = state.abb)
lassmod <- lars(as.matrix(statedata[, -4]), statedata$Life.Exp)
plot(lassmod)

set.seed(123)
cv.lassmod <- cv.lars(as.matrix(statedata[, -4])
                      , statedata$Life.Exp)
cv.lassmod$index[which.min(cv.lassmod$cv)]
predict(lassmod, s=cv.lassmod$index[which.min(cv.lassmod$cv)]
        , type="coef", mode="fraction")$coef
coef(lm(Life.Exp~Population+Murder+HS.Grad+Frost
        , data = statedata))

trainy <- trainmeat$fat
trainx <- as.matrix(trainmeat[, -101])
lassmod <- lars(trainx, trainy)
set.seed(123)
cv.out <- cv.lars(trainx, trainy)
cv.out$index[which.min(cv.out$cv)]

testx <- as.matrix(testmeat[,-101])
predlars <- predict(lassmod, testx
                    , s=cv.out$index[which.min(cv.out$cv)]
                    , mode = "fraction")
rmse(testmeat$fat, predlars$fit)

predlars <- predict(lassmod, testx
                    , s=cv.out$index[which.min(cv.out$cv)]
                    , type="coef"
                    , mode = "fraction")
plot(predlars$coefficients
     , type = "h"
     , ylab = "Coefficients")
sum(predlars$coefficients != 0)

# exercises
# centre, scale and princomp
data("seatpos")
csp <- seatpos[
  , c("HtShoes", "Ht", "Seated", "Arm", "Thigh", "Leg")]

prcsp <- prcomp(csp, scale. = TRUE)
summary(prcsp)
plot(prcsp$sdev
     , type = "l"
     , xlab = "PC"
     , ylab = "SD of PC") # this might suggest only 1, but second is interpretable also, even a third - some contrast between arm and thigh (gender differences?)

prcsp$rotation[,1:3] # generic size and contrast between body and arm/thigh circumferences

csp2 <- seatpos[
  , c("HtShoes", "Ht", "Seated", "Arm", "Thigh", "Leg"
      , "Age", "Weight")]
prcsp2 <- prcomp(csp2, scale. = TRUE)
summary(prcsp2)

plot(prcsp2$sdev
     , type = "l"
     , xlab = "PC"
     , ylab = "SD of PC") # better at suggesting 3
# 1 - generic size, 2 - age 3 arm/thigh vs rest of body, 4 arm vs thigh
prcsp2$rotation[,1:4] # generic size

newval <- data.frame(Age = 64.800, Weight = 263.7
                     , HtShoes = 181.080, Ht = 178.560
                     , Seated = 91.440, Arm = 35.640
                     , Thigh = 40.950, Leg = 38.790)

# scalings
sc <- scale(seatpos)
attr(sc, "scaled:center")
attr(sc, "scaled:scale")

newval.sc <- (newval - attr(sc, "scaled:center")[-9])/attr(sc, "scaled:scale")[-9]

lmod_pcr1 <- pcr(seatpos$hipcenter ~ .
                 , scale = TRUE
                 , center = TRUE
                , data = csp
                , ncomp =1)
lmod_pcr4 <- pcr(seatpos$hipcenter ~ .
                 , scale = TRUE
                 , center = TRUE
                 , data = csp
                 , ncomp =4)
predict(lmod_pcr1, newval)
predict(lmod_pcr4, newval)

set.seed(123)
lmod_pls_cv <- pcr(hipcenter~., data = seatpos
                   , validation="CV"
                   , ncomp = 8)
pcr_cv <- RMSEP(lmod_pls_cv, estimate = "CV")
plot(pcr_cv, main= "")
which.min(pcr_cv$val)
pcr_cv$val[which.min(pcr_cv$val)]
predict(lmod_pls_cv, newval, ncomp = 3)

set.seed(123)
plsmod <- plsr(hipcenter~.
               , data = seatpos
               , ncomp=8
               , validation="CV")

coefplot(plsmod, ncomp=8, xlab="Frequency")
plsCV <- RMSEP(plsmod, estimate="CV")
plot(plsCV, main = "")
predict(plsmod, newval, ncomp=3)

library(glmnet)
x <- model.matrix(seatpos$hipcenter ~ .-1
               , data=as.data.frame(sc)[,-9]) 
y <- seatpos$hipcenter
fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)
cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)
cv.ridge$lambda[which.min(cv.ridge$cvm)]

rgmod <- lm.ridge(seatpos$hipcenter ~ .
         , lambda = seq(3, 5
                        , length.out=21)
         , data=as.data.frame(sc)[,-9])
which.min(rgmod$GCV)
matplot(rgmod$lambda
        , coef(rgmod)
        , type="l"
        , xlab = expression(lambda)
        , ylab = expression(hat(beta)))
abline(v=as.numeric(names(which.min(rgmod$GCV))))

data(fat)
names(fat)
fat_deps <- fat[, -(1:3)]
siri <- fat$siri
train_idx <- c(rep(TRUE, 9), FALSE)
test_idx <- !c(rep(TRUE, 9), FALSE)
train <- fat_deps[train_idx, ]
test <- fat_deps[test_idx, ]
siri_train <- siri[train_idx]
siri_test <- siri[test_idx]

lmod <- lm(siri_train ~ ., data = train)
lmod_step <- step(lmod)
prc <- prcomp(train, scale. = TRUE)
summary(prc)
plot(prc$sdev
     , type = "l"
     , xlab = "PC"
     , ylab = "SD of PC")
prcmod <- pcr(siri_train ~ ., data = train, ncomp = 4)

set.seed(123)
plsmod <- plsr(siri_train ~ ., data = train
               , ncomp=15
               , validation="CV")

coefplot(plsmod, ncomp=15, xlab="Frequency")
plsCV <- RMSEP(plsmod, estimate="CV")
plot(plsCV, main = "") # 4

rgmod <- lm.ridge(siri_train ~ ., data = train
                  , lambda = seq(0, 0.1
                                 , length.out=21))
which.min(rgmod$GCV)
matplot(rgmod$lambda
        , coef(rgmod)
        , type="l"
        , xlab = expression(lambda)
        , ylab = expression(hat(beta)))
abline(v=as.numeric(names(which.min(rgmod$GCV))))

x <- model.matrix(siri_train ~ .-1, data = train) 
y <- siri_train
fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)
cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)
cv.ridge$lambda[which.min(cv.ridge$cvm)]

fit.ridge=glmnet(x,y,alpha=0
                 , lambda = cv.ridge$lambda[which.min(cv.ridge$cvm)])
x_test <- model.matrix(siri_test ~. -1, data = test)

rmse(siri_test, predict(lmod, test))
rmse(siri_test, predict(lmod_step, test))
rmse(siri_test, predict(prcmod, test))
rmse(siri_test, predict(plsmod, test))
rmse(siri_test, predict(fit.ridge, newx = x_test))

data("gasoline")
names(gasoline)
View(gasoline)

data(kanga)
nrow(na.omit(kanga))
names(kanga)
kanga.pca <- prcomp(na.omit(kanga[, -(1:2)]))
summary(kanga.pca)
kanga.pca$rotation[,1]
kanga.pca.sc <- prcomp(na.omit(kanga[, -(1:2)]), scale = TRUE)
summary(kanga.pca.sc)
kanga.pca.sc$rotation[,1]
kanga.pca.sc$rotation[,2]

robfat <- cov.rob(na.omit(kanga)[, -(1:2)])
md <- mahalanobis(na.omit(kanga)[, -(1:2)], center = robfat$center, cov = robfat$cov)
n <- nrow(na.omit(kanga)[, -(1:2)]); p <- ncol(na.omit(kanga)[, -(1:2)])

plot(qchisq(1:n/(n+1), p), sort(md)
     , xlab = expression(paste(chi^2, "quantiles"))
     , ylab = "sorted mahalanobis distances")
abline(0,1)

plot(kanga.pca$x[,2]~kanga.pca$x[,1], col=na.omit(kanga)[,1]) # species
plot(kanga.pca$x[,2]~kanga.pca$x[,1], col=na.omit(kanga)[,2]) # sex
