library(mboost)
library(GAMBoost)
library(CoxBoost)
library(TH.data)
data("bodyfat")
lm1 <- lm(DEXfat ~ hipcirc +
            kneebreadth +
            anthro3a
          , data = bodyfat)
coef(lm1)
glm1 <- glmboost(DEXfat ~ hipcirc +
             kneebreadth +
             anthro3a
           , data = bodyfat)
coef(glm1)
coef(glm1, off2int = TRUE)

# test train split to compare predictive performance of models
dat.train <- bodyfat[-(1:10),-1] ## removing age
dat.test <- bodyfat[1:10,-1]

lm1 <- glm(DEXfat ~ hipcirc +
             kneebreadth +
             anthro3a
           , data =dat.train)
## boosting:
glm1 <- glmboost(DEXfat ~ . , data = dat.train)
glm2 <- glmboost(DEXfat ~ . , data = dat.train
                 , family = Laplace())
glm3 <- glmboost(DEXfat ~ . , data = dat.train
                 , family = GammaReg())
glm4 <- glmboost(DEXfat ~ . , data = dat.train
                 , family = Huber())

# OLS
mean((predict(lm1, dat.test) - dat.test$DEXfat)^2)
mean(dat.test$DEXfat)
mean((predict(glm1, dat.test) - dat.test$DEXfat)^2)
mean((predict(glm2, dat.test) - dat.test$DEXfat)^2)
mean((exp(predict(glm3, dat.test)) - dat.test$DEXfat)^2)
mean((predict(glm4, dat.test) - dat.test$DEXfat)^2)

# gam models
gam1 <- gamboost(DEXfat ~ hipcirc +
                   kneebreadth +
                   anthro3a , data = dat.train)
mean((predict(gam1, dat.test) - dat.test$DEXfat)^2)
plot(gam1)
gam2 <- glmboost(DEXfat ~ . , data = dat.train)
mean((predict(gam2, dat.test) - dat.test$DEXfat)^2)
gam3 <- glmboost(DEXfat ~ . , data = dat.train
                 , family=Laplace())
mean((predict(gam3, dat.test) - dat.test$DEXfat)^2)

# linear effect for hipcirc
# smooth effects for kneebreadth and anthro3a
gam4 <- gamboost(DEXfat ~ bols(hipcirc) +
                   bbs(kneebreadth) +
                   bbs(anthro3a)
                 , data = dat.train)
mean((predict(gam4, dat.test) - dat.test$DEXfat)^2)
plot(gam4)

# choose stopping and verbose
gam1 <- gamboost(DEXfat ~ hipcirc +
                   kneebreadth +
                   anthro3a
                 , data = dat.train
                 , control = boost_control(mstop = 500
                                           , trace = TRUE))
mean((predict(gam1.m, dat.test) - dat.test$DEXfat)^2)

# using AIC to find optimum
AIC(gam1)
AIC(gam1[149])

# using cv to find optimum
cvr <- cvrisk(gam1)
## default: 25-fold Bootstrap (takes a few seconds)
plot(cvr)
mstop(cvr)

## include all variables, 100 iterations
gam2 <- gamboost(DEXfat ~ . , data = dat.train)
cvr <- cvrisk(gam2) ## 25-fold bootstrap
mstop(cvr)
gam2 <- gam2[mstop(cvr)] ## set to optimal iteration
mean((predict(gam2, dat.test) - dat.test$DEXfat)^2)

# smoothness of splines
set.seed(1234)
x <- runif(150, -0.2, 0.2)
y = (0.5 - 0.9* exp(-50*x^2))*x + 0.02 *rnorm(150)
y <- y[order(x)] ## order obs by size of x
x <- x[order(x)] ## just for easier plotting

par(mfrow = c(1,2))
## model fit
plot(x, y, las = 1, main = "model fit at m = 1" )
## observations
curve((0.5 - 0.9* exp(-50*x^2))*x, add=TRUE, from = -.2
      , to = .2, lty = 2, lwd = 2) ## true function
## now carry out one boosting iteration
gam1 <- gamboost(y ~ x
        , control = boost_control(mstop = 1))
lines(x , fitted(gam1), col = 2, lwd = 2) ## plot fitted values
## residual plot
plot(x, y - fitted(gam1[1]) , ylab = "residuals", main = "residuals at m = 1"
     , ylim = c(-.1, .1), las = 1) ## residuals
lines(smooth.spline(x, y - fitted(gam1))
      , col = 4, lwd = 2) ## show remaining structure

## model fit
plot(x, y, las = 1, main = "model fit at m = 5" )
curve((0.5 - 0.9* exp(-50*x^2))*x
      , add=TRUE, from = -.2, to = 0.2
      , lty =2, lwd = 2)
lines(x , fitted(gam1[5]), col = 2, lwd = 2)
## residual plot
plot(x, y - fitted(gam1[5])
     , ylab = "residuals"
     , main = "residuals at m = 5"
     , las = 1, ylim = c(-.1, .1))
lines(smooth.spline(x, y - fitted(gam1[5]))
      , col = 4, lwd = 2)

## model fit
plot(x, y, las = 1, main = "model fit at m = 30" )
curve((0.5 - 0.9* exp(-50*x^2))*x
      , add=TRUE, from = -.2, to = 0.2
      , lty =2, lwd = 2)
lines(x , fitted(gam1[30]), col = 2, lwd = 2)
## residual plot
plot(x, y - fitted(gam1[30])
     , ylab = "residuals"
     , main = "residuals at m = 30"
     , las = 1, ylim = c(-.1, .1))
lines(smooth.spline(x, y - fitted(gam1[30]))
      , col = 4, lwd = 2)
par(mfrow = c(1,1))

set.seed(123)
cvr <- cvrisk(gam1, grid = 1:200) ## max mstop = 200
mstop(cvr)

# slow over-fitting
plot(x, y, las = 1
     , main = "model fit at m = 1000" )
curve((0.5 - 0.9* exp(-50*x^2))*x
      , add=TRUE, from = -.2
      , to = 0.2, lty =2, lwd = 2)
lines(x , fitted(gam1[1000])
      , col = 2, lwd = 2)
## model fit mstop = 50000
plot(x, y, las = 1
     , main = "model fit at m = 50000" )
curve((0.5 - 0.9* exp(-50*x^2))*x
      , add=TRUE, from = -.2, to = 0.2
      , lty =2, lwd = 2)
lines(x , fitted(gam1[50000])
      , col = 2, lwd = 2)

glm1 <- GLMBoost(y = bodyfat$DEXfat
                 , x = as.matrix(bodyfat[
                   , c("hipcirc"
                      , "kneebreadth"
                      , "anthro3a")])
                 , family = gaussian()
                 , stepno = 100)
summary(glm1) 

# likelihood ratio boosting
set.seed(123)
cv.glm1 <- cv.GLMBoost(y = bodyfat$DEXfat
                       , x = as.matrix(bodyfat[
                         , c("hipcirc"
                             , "kneebreadth"
                             , "anthro3a")])
                       , family = gaussian()
                       , maxstepno = 100
                       , just.criterion=TRUE)
cv.glm1$selected
glm1 <- GLMBoost(y = bodyfat$DEXfat
                 , x = as.matrix(bodyfat[
                   , c("hipcirc"
                       , "kneebreadth"
                       , "anthro3a")])
                 , family = gaussian()
                 , stepno = cv.glm1$selected)
plot(glm1)

# likelihood ratio boosting
set.seed(123)
cv.gam1 <- cv.GAMBoost(y = bodyfat$DEXfat
                       , x = as.matrix(bodyfat[
                         , c("hipcirc"
                             , "kneebreadth"
                             , "anthro3a")])
                       , family = gaussian()
                       , maxstepno = 100
                       , just.criterion=TRUE)
cv.gam1$selected
gam1 <- GAMBoost(y = bodyfat$DEXfat
                 , x = as.matrix(bodyfat[
                   , c("hipcirc"
                       , "kneebreadth"
                       , "anthro3a")])
                 , family = gaussian()
                 , stepno = cv.glm1$selected)
plot(gam1)