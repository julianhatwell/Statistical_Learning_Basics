library(ISLR)
library(boot)
library(MASS)
set.seed(143)

Default$balanceK <- Default$balance/1000
Default$incomeK <- Default$income/1000

fit1 <- glm(default~balanceK+incomeK, data = Default, family = binomial)

error.rates <- numeric(1000)
error.rates.student <- numeric(1000)

for (i in 1:1000) {
  train <- sample(nrow(Default), floor(nrow(Default)/2))
  
  fit2 <- glm(default~balanceK+incomeK, data = Default
              , subset = train, family = binomial)
  
  preds <- predict(fit2, newdata = Default[-train,], type = "response")
  
  preds.classes <- rep("No", length(preds))
  
  preds.classes[preds > 0.5] <- "Yes"
  
  error.rates[i] <- mean(preds.classes != Default[-train,]$default)
}

for (i in 1:1000) {
  train <- sample(nrow(Default), floor(nrow(Default)/2))
  
  fit3 <- glm(default~balanceK+incomeK+student, data = Default
              , subset = train, family = binomial)
  preds <- predict(fit3, newdata = Default[-train,], type = "response")
  
  preds.classes <- rep("No", length(preds))
  
  preds.classes[preds > 0.5] <- "Yes"
  error.rates.student[i] <- mean(preds.classes != Default[-train,]$default)
}

t.test(error.rates, error.rates.student)

set.seed(1123)
boot.fn <- function(data, index) {
  fit4 <- glm(default~balanceK+incomeK, data = Default
              , subset = index, family = binomial)
  
  return(coef(fit4)[2:3])
}
bt <- boot(Default, boot.fn, R = 1000)
summary(bt)

weekly.preds <- logical(nrow(Weekly))
for (i in 1:nrow(Weekly)) {
  fit5 <- glm(Direction~Lag1+Lag2, data = Weekly[-i,], family = binomial)
  weekly.preds[i] <- if (predict(fit5, newdata = Weekly[i,], type = "response") > 0.5) {"Up"} else {"Down"}
}
mean(weekly.preds != Weekly$Direction)

set.seed(10)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
# y = B0+B1X+B2X^2+e
# B0=0, B1=1, B2=-2

simdata <- data.frame(y=y, x=x)
qfit1 <- glm(y~x, data = simdata)
qfit2 <- glm(y~poly(x,2), data = simdata)
qfit3 <- glm(y~poly(x,3), data = simdata)
qfit4 <- glm(y~poly(x,4), data = simdata)

err1 <- cv.glm(simdata, qfit1)$delta[1]
err2 <- cv.glm(simdata, qfit2)$delta[1]
err3 <- cv.glm(simdata, qfit3)$delta[1]
err4 <- cv.glm(simdata, qfit4)$delta[1]

attach(Boston)
mu <- mean(medv)
s.err <- sd(medv)/sqrt(nrow(Boston))

Boston.boot.fn <- function(data, index) {
  mean(data[index])
}
Boston.boot.fn2 <- function(data, index) {
  median(data[index])
}
Boston.boot.fn3 <- function(data, index) {
  quantile(data[index], probs = 0.1)
}
set.seed(201)
bt <- boot(medv, Boston.boot.fn, R=1000)
t.test(medv)$conf.int[1:2]
bt$t0 + c(-1.96,1.96) * sd(bt$t)
mumd <- median(medv)
bt.md <- boot(medv, Boston.boot.fn2, R=1000)
bt.qnt <- boot(medv, Boston.boot.fn3, R=1000)
