# ISLR 7
# Question 3
b2 <- function(X) {
  X <- if (X >= 1) { (X - 1)^2 } else { 0 }
  X
}
x <- seq(-2,2,0.01)
y <- 1 + x - 2 * sapply(x, b2)
plot(x,y, type = "l")

# ppaguay solution
x = -2:2
y = 1 + x + -2 * (x-1)^2 * I(x>1)
plot(x, y)

# Question 4
b1 <- function(X) {
  I(0 <= X & X <= 2) - (X - 1) * I(1 <= X & X <= 2)
}
b2 <- function(X) {
  (X - 3) * I(3 <= X & X <= 4) + I(4 <= X & X <= 5)
}
y <- 1 + 1 * sapply(x, b1) + 3 * sapply(x, b2)
plot(x,y, type = "l")

# Question 6
library(ISLR)
# polynomial
# set up manual cv
k <- 5
pn <- 10
val.errors <- matrix(nrow = k, ncol = pn)
folds <- integer(0)
# method to get close to equal size folds
set.seed(12321)
while (length(folds) < nrow(Wage)) {
  folds <- c(folds, sample(1:k, replace = FALSE))
}
folds <- folds[1:nrow(Wage)]

for (i in 1:k) {
  Wage.train <- Wage[folds != i,]
  Wage.test <- Wage[folds == i,]
  
  for(j in 1:pn) {
    fit.poly <- lm(wage~poly(age,j), data = Wage.train)
    pred.poly <- predict(fit.poly, Wage.test)
    val.errors[i,j] <- mean((pred.poly - Wage.test$wage)^2)
  }
}

cv.errors <- apply(val.errors, 2, mean)
which.min(cv.errors)
plot(cv.errors)

# ppaquay solution
library(boot) # contains cv.glm for doing K-Fold on a glm
set.seed(12)
deltas <- rep(NA, pn)

for (j in 1:pn) {
  fit <- glm(wage ~ poly(age, j), data = Wage)
  deltas[j] <- cv.glm(Wage, fit, K = k)$delta[2]
}
plot(deltas, xlab = "Degree", ylab = "Test MSE", type = "l")
points(which.min(deltas), deltas[which.min(deltas)], col = "red", cex = 2, pch = 20)

# set up anova
fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
fit.6=lm(wage~poly(age,6),data=Wage)
fit.7=lm(wage~poly(age,7),data=Wage)
fit.8=lm(wage~poly(age,8),data=Wage)
fit.9=lm(wage~poly(age,9),data=Wage)
fit.10=lm(wage~poly(age,10),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5,fit.6,fit.7,fit.8,fit.9,fit.10)
summary(fit.10)

# make a plot
attach(Wage)
age.lims <- range(age)
age.grid <- seq(from = age.lims[1], to = age.lims[2])
preds <- predict(fit.4, newdata = data.frame(age = age.grid), se = TRUE)
se.bands <- cbind(preds$fit - preds$se.fit * 1.96
                  , preds$fit + preds$se.fit * 1.96)
plot(wage~age, col = "darkgrey")
lines(age.grid, preds$fit, lwd=2, col = "blue")
matlines(age.grid, se.bands, lty = 2, col = "blue")

# step function
# use manual cv set up from poly section
k <- 5
pn <- 10
val.errors <- matrix(nrow = k, ncol = pn)

for(j in 2:pn) {
  Wage$age.step <- cut(Wage$age, j)
  for (i in 1:k) {
    Wage.train <- Wage[folds != i,]
    Wage.test <- Wage[folds == i,]
    fit.step <- lm(wage~age.step, data = Wage.train)
    pred.step <- predict(fit.step, Wage.test)
    val.errors[i,j] <- mean((pred.step - Wage.test$wage)^2)
  }
}

cv.errors <- apply(val.errors, 2, mean)
which.min(cv.errors)
plot(2:pn, cv.errors[2:pn])

# ppaquay solution
set.seed(12)
cvs <- rep(NA, pn)
for (j in 2:pn) {
  Wage$age.cut <- cut(Wage$age, j)
  fit <- glm(wage ~ age.cut, data = Wage)
  cvs[j] <- cv.glm(Wage, fit, K = 10)$delta[2]
}
plot(2:10, cvs[-1], xlab = "Cuts", ylab = "Test MSE", type = "l")
d.min <- which.min(cvs)
points(which.min(cvs), cvs[which.min(cvs)], col = "red", cex = 2, pch = 20)

# create a plot
fit.step <- lm(wage~cut(age, which.min(cv.errors)), data = Wage)
preds <- predict(fit.step, newdata = data.frame(age = age.grid), se = TRUE)
se.bands <- cbind(preds$fit - preds$se.fit * 1.96
                  , preds$fit + preds$se.fit * 1.96)
plot(wage~age, col = "darkgrey")
lines(age.grid, preds$fit, lwd=2, col = "blue")
matlines(age.grid, se.bands, lty = 2, col = "blue")

# Question 7
library(lattice)

bwplot(wage~factor(year), Wage, groups = year,
       panel = function(x, y, groups, subscripts, ...) {
         panel.grid(h = -1, v = 0)
         panel.stripplot(x, y, ..., jitter.data = TRUE,
                         groups = groups, subscripts = subscripts)
         panel.average(x[y < 250], y[y < 250], subscripts, fun = mean
                       , col = "black", lwd = 0.1, ...)
         panel.average(x[y >= 250], y[y >= 250], subscripts, fun = mean
                       , col = "black", lwd = 0.1, ...)
         panel.average(x, y, subscripts, fun = mean
                       , col = "black", lwd = 2, ...)
       },
       auto.key =
         list(points = TRUE, lines = FALSE, columns = 4))

bwplot(wage~maritl, Wage, groups = maritl,
       panel = function(x, y, groups, subscripts, ...) {
         panel.grid(h = -1, v = 0)
         panel.stripplot(x, y, ..., jitter.data = TRUE,
                         groups = groups, subscripts = subscripts)
         panel.average(x[y < 250], y[y < 250], subscripts, fun = mean
                       , col = "black", lwd = 0.1, ...)
         panel.average(x[y >= 250], y[y >= 250], subscripts, fun = mean
                       , col = "black", lwd = 0.1, ...)
         panel.average(x, y, subscripts, fun = mean
                       , col = "black", lwd = 2, ...)
       },
       auto.key =
         list(points = TRUE, lines = FALSE, columns = 4))

bwplot(wage~race, Wage, groups = race,
       panel = function(x, y, groups, subscripts, ...) {
         panel.grid(h = -1, v = 0)
         panel.stripplot(x, y, ..., jitter.data = TRUE,
                         groups = groups, subscripts = subscripts)
         panel.average(x[y < 250], y[y < 250], subscripts, fun = mean
                       , col = "black", lwd = 0.1, ...)
         panel.average(x[y >= 250], y[y >= 250], subscripts, fun = mean
                       , col = "black", lwd = 0.1, ...)
         panel.average(x, y, subscripts, fun = mean
                       , col = "black", lwd = 2, ...)
       },
       auto.key =
         list(points = TRUE, lines = FALSE, columns = 4))

bwplot(wage~race, Wage, groups = race,
       panel = function(x, y, groups, subscripts, ...) {
         panel.grid(h = -1, v = 0)
         panel.stripplot(x, y, ..., jitter.data = TRUE,
                         groups = groups, subscripts = subscripts)
         panel.average(x[y < 250], y[y < 250], subscripts, fun = mean
                       , col = "black", lwd = 0.1, ...)
         panel.average(x[y >= 250], y[y >= 250], subscripts, fun = mean
                       , col = "black", lwd = 0.1, ...)
         panel.average(x, y, subscripts, fun = mean
                       , col = "black", lwd = 2, ...)
       },
       auto.key =
         list(points = TRUE, lines = FALSE, columns = 4))

bwplot(wage~education, Wage, groups = education,
       panel = function(x, y, groups, subscripts, ...) {
         panel.grid(h = -1, v = 0)
         panel.stripplot(x, y, ..., jitter.data = TRUE,
                         groups = groups, subscripts = subscripts)
         panel.average(x[y < 250], y[y < 250], subscripts, fun = mean
                       , col = "black", lwd = 0.1, ...)
         panel.average(x[y >= 250], y[y >= 250], subscripts, fun = mean
                       , col = "black", lwd = 0.1, ...)
         panel.average(x, y, subscripts, fun = mean
                       , col = "black", lwd = 2, ...)
       },
       auto.key =
         list(points = TRUE, lines = FALSE, columns = 4))

bwplot(wage~jobclass, Wage, groups = jobclass,
       panel = function(x, y, groups, subscripts, ...) {
         panel.grid(h = -1, v = 0)
         panel.stripplot(x, y, ..., jitter.data = TRUE,
                         groups = groups, subscripts = subscripts)
         panel.average(x[y < 250], y[y < 250], subscripts, fun = mean
                       , col = "black", lwd = 0.1, ...)
         panel.average(x[y >= 250], y[y >= 250], subscripts, fun = mean
                       , col = "black", lwd = 0.1, ...)
         panel.average(x, y, subscripts, fun = mean
                       , col = "black", lwd = 2, ...)
       },
       auto.key =
         list(points = TRUE, lines = FALSE, columns = 4))

bwplot(wage~health, Wage, groups = health,
       panel = function(x, y, groups, subscripts, ...) {
         panel.grid(h = -1, v = 0)
         panel.stripplot(x, y, ..., jitter.data = TRUE,
                         groups = groups, subscripts = subscripts)
         panel.average(x[y < 250], y[y < 250], subscripts, fun = mean
                       , col = "black", lwd = 0.1, ...)
         panel.average(x[y >= 250], y[y >= 250], subscripts, fun = mean
                       , col = "black", lwd = 0.1, ...)
         panel.average(x, y, subscripts, fun = mean
                       , col = "black", lwd = 2, ...)
       },
       auto.key =
         list(points = TRUE, lines = FALSE, columns = 4))

bwplot(wage~health_ins, Wage, groups = health_ins,
       panel = function(x, y, groups, subscripts, ...) {
         panel.grid(h = -1, v = 0)
         panel.stripplot(x, y, ..., jitter.data = TRUE,
                         groups = groups, subscripts = subscripts)
         panel.average(x[y < 250], y[y < 250], subscripts, fun = mean
                       , col = "black", lwd = 0.1, ...)
         panel.average(x[y >= 250], y[y >= 250], subscripts, fun = mean
                       , col = "black", lwd = 0.1, ...)
         panel.average(x, y, subscripts, fun = mean
                       , col = "black", lwd = 2, ...)
       },
       auto.key =
         list(points = TRUE, lines = FALSE, columns = 4))

# step function for year
# use manual cv set up from poly section
k <- 5
val.errors <- rep(NA, 5)

for (i in 1:k) {
  Wage.train <- Wage[folds != i,]
  Wage.test <- Wage[folds == i,]
  fit.step <- lm(wage~factor(year), data = Wage.train)
  pred.step <- predict(fit.step, Wage.test)
  val.errors[i] <- mean((pred.step - Wage.test$wage)^2)
  print(coef(fit.step))
}
# best results from cv fold 2
# (Intercept) factor(year)2004 factor(year)2005 factor(year)2006 factor(year)2007 
# 106.872826         3.719619         4.658725         7.651620         7.957385 
# factor(year)2008 factor(year)2009 
# 6.889894        10.631755 

# create a step function from these:
step.func.year <- function(yr) {
  keyVals <- data.frame(year = c(2003, 2004, 2005, 2006, 2007, 2008, 2009)
                      , constant = c(106.872826
                      , 106.872826 + 3.719619
                      , 106.872826 + 4.658725
                      , 106.872826 + 7.651620
                      , 106.872826 + 7.957385
                      , 106.872826 + 6.889894
                      , 106.872826 + 10.631755))
  return(keyVals[keyVals$year == yr, "constant"])
}
preds <- sapply(Wage$year, step.func.year)
error <- mean((Wage$wage - preds)^2)
error
# this didn't do so well as the test estimate
plot(Wage$year, Wage$wage, col = "lightgrey")
points(Wage$year[order(Wage$year)], preds[order(Wage$year)], col = "blue", type = "l")
# it looks just like the panel average function

# local regression
year.lo <- loess(wage~year, data = Wage, span = 2)
plot(Wage$year, Wage$wage, col = "lightgrey")
lines(year.lo$x[order(Wage$year)], year.lo$fitted[order(Wage$year)])

# gam
library(gam)
gam1 <- gam(wage~lo(year, span = 2), data = Wage)
gam2 <- gam(wage~lo(year, span = 2) + s(age, 4), data = Wage)
gam3 <- gam(wage~lo(year, span = 2) + s(age, 4) + maritl, data = Wage)
gam4 <- gam(wage~lo(year, span = 2) + s(age, 4) + maritl + education, data = Wage)
gam5 <- gam(wage~lo(year, span = 2) + s(age, 4) + maritl + education + health_ins, data = Wage)
gam6 <- gam(wage~lo(year, span = 2) + s(age, 4) + maritl + education + health_ins + health, data = Wage)
gam7 <- gam(wage~lo(year, span = 2) + s(age, 4) + maritl + education + health_ins + health + jobclass, data = Wage)
gam8 <- gam(wage~lo(year, span = 2) + s(age, 4) + maritl + education + health_ins + health + jobclass + race, data = Wage)
anova(gam1, gam2, gam3, gam4, gam5, gam6, gam7, gam8)

# bit of trial and error but looks like gam7 is the one
plot(gam7)
