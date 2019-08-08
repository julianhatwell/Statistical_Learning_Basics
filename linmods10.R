library(faraway)
library(leaps)
library(splines)

data("state")
statedata <- data.frame(state.x77, row.names = state.abb)
View(statedata)
lmod <- lm(Life.Exp ~ ., statedata)
sumary(lmod)
lmod <- update(lmod, . ~ . - Area)
sumary(lmod)
lmod <- update(lmod, . ~ . - Illiteracy)
sumary(lmod)
lmod <- update(lmod, . ~ . - Income)
sumary(lmod)
lmod <- update(lmod, . ~ . - Population)
sumary(lmod)
sumary(lm(Life.Exp~Illiteracy+Murder+Frost, data=statedata))

# criterion based selections
b <- regsubsets(Life.Exp ~ ., data = statedata)
rs <- summary(b)
rs$which
AIC <- 50 * log(rs$rss/50) + (2:8)*2
plot(AIC ~ I(2:8)
     , ylab = "AIC"
     , xlab = "Number of Predictors")
plot(rs$adjr2 ~ I(2:8)
     , ylab = "adjR2"
     , xlab = "Number of Predictors")
plot(rs$cp ~ I(2:8)
     , ylab = "Mallow's C"
     , xlab = "Number of Predictors")
abline(0,1)

lmod <- lm(Life.Exp ~ ., data = statedata)
step(lmod)

# checking for influence points
h <- lm.influence(lmod)$hat
names(h) <- state.abb
rev(sort(h))

b <- regsubsets(Life.Exp ~ .
                , data = statedata
                , subset = state.abb != "AK")
rs <- summary(b)
rs$which[which.max(rs$adjr2), ]
stripchart(data.frame(scale(statedata))
           , method = "jitter"
           , las = 2
           , vertical = TRUE)
lmod <- lm(Life.Exp ~ log(Population) +
             Income + Illiteracy + Income +
             Murder + HS.Grad + Frost + log(Area)
           , data = statedata)
rs <- summary(b)
rs$which[which.max(rs$adjr2), ]
rs$adjr2

# exercises
data("prostate")
names(prostate)
fmla <- as.formula("lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45")
lmod <- lm(fmla
           , data = prostate)
sumary(lmod)
lmod <- update(lmod, .~. -gleason)
sumary(lmod)
lmod <- update(lmod, .~. -lcp)
sumary(lmod)
lmod <- update(lmod, .~. -pgg45)
sumary(lmod)
lmod <- update(lmod, .~. -age)
sumary(lmod)
lmod <- update(lmod, .~. -lbph)
sumary(lmod)

b <- regsubsets(lpsa ~ .
                , data = prostate)
rs <- summary(b)
rs$which
n <- nrow(prostate)
AIC <- n * log(rs$rss/n) + (2:9)*2
plot(AIC ~ I(2:9)
     , ylab = "AIC"
     , xlab = "Number of Predictors")
plot(rs$adjr2 ~ I(2:9)
     , ylab = "adjR2"
     , xlab = "Number of Predictors")
plot(rs$cp ~ I(2:9)
     , ylab = "Mallow's C"
     , xlab = "Number of Predictors")
abline(0,1)

data("teengamb")
fmla <- as.formula("gamble~sex+status+income+verbal")
lmod <- lm(fmla
           , data = teengamb)
sumary(lmod)
lmod <- update(lmod, .~.-status)
sumary(lmod)
lmod <- update(lmod, .~.-verbal)
sumary(lmod)

b <- regsubsets(gamble ~ .
                , data = teengamb)
rs <- summary(b)
rs$which
n <- nrow(teengamb)
AIC <- n * log(rs$rss/n) + (2:5)*2
plot(AIC ~ I(2:5)
     , ylab = "AIC"
     , xlab = "Number of Predictors")
plot(rs$adjr2 ~ I(2:5)
     , ylab = "adjR2"
     , xlab = "Number of Predictors")
plot(rs$cp ~ I(2:5)
     , ylab = "Mallow's C"
     , xlab = "Number of Predictors")
abline(0,1)

data("divusa")
lmod <- lm(divorce~., data=divusa)
sumary(lmod)
lmod <- update(lmod, .~.-unemployed)
sumary(lmod)

b <- regsubsets(divorce ~ .
                , data = divusa)
rs <- summary(b)
rs$which
n <- nrow(divusa)
AIC <- n * log(rs$rss/n) + (2:7)*2
plot(AIC ~ I(2:7)
     , ylab = "AIC"
     , xlab = "Number of Predictors")
plot(rs$adjr2 ~ I(2:7)
     , ylab = "adjR2"
     , xlab = "Number of Predictors")
plot(rs$cp ~ I(2:7)
     , ylab = "Mallow's C"
     , xlab = "Number of Predictors")
abline(0,1)

data("trees")
lmod <- lm(log(Volume) ~ Height + I(Height^2) +
             Girth + I(Girth^2) + Height:Girth
           , data = trees)
sumary(lmod)

lmod_step <- step(lmod)
sumary(lmod_step)

data("stackloss")
lmod <- lm(stack.loss ~ ., data = stackloss)
sumary(lmod)
lmod <- update(lmod, .~.-Acid.Conc.)
h <- lm.influence(lmod)$hat
names(h) <- row.names(stackloss)
rev(sort(h))
lmod <- lm(stack.loss ~ ., data = stackloss)
sumary(lmod)
h <- lm.influence(lmod)$hat
names(h) <- row.names(stackloss)
rev(sort(h))
plot(lmod, which = 5)
stackloss.cl <- stackloss[-17, ] # tried 21 also
lmod <- lm(stack.loss ~ ., data = stackloss.cl)
sumary(lmod)

data("seatpos")
lmod <- lm(hipcenter~., data = seatpos)
sumary(lmod)
x <- as.data.frame(t(apply(seatpos, 2, mean)))
predict(lmod, newdata = x, interval = "prediction")
lmod_step <- step(lmod)
sumary(lmod_step)

b <- regsubsets(hipcenter~., data = seatpos)
rs <- summary(b)
rs$which
n <- nrow(seatpos)
aic <- n * log(rs$rss/n) + (2:9)*2
plot(AIC ~ I(2:9)
     , ylab = "AIC"
     , xlab = "Number of Predictors")

funky <- function(x) sin(2 * pi * x^3)^3
x <- seq(0, 1, 0.01)
y <- funky(x) + 0.1 * rnorm(101)
matplot(x, cbind(y, funky(x)), type = c("pl")
        , ylab = "y", pch = 20
        , lty = 1, col = 1)

lmodb <- lm(y~bs(x, 12))
matplot(x, cbind(y, funky(x), lmodb$fitted.values)
        , type = c("pll")
        , ylab = "y", pch = 20
        , lty = c(1, 1, 2), col = c(1, 1, 2))
lmod_step <- step(lmodb)
sumary(lmod_step)
rss <- sum(resid(lmodb)^2)
n <- length(x)
aic <- n * log(rss/n) + 13*2
aic

# from 3 to 20
nnots <- 3:20
fits <- matrix(NA, ncol = length(nnots), nrow = length(x))
aic <- numeric(length(nnots))
for (i in nnots) {
  lmodb <- lm(y~bs(x, i))
  rss <- sum(resid(lmodb)^2)
  n <- length(x)
  aic[i-2] <- n * log(rss/n) + (i+1)*2
  fits[, i-2] <- lmodb$fitted.values
}
matplot(fits, type = "l")
aic
plot(nnots, aic) # 14 is best

lmodb <- lm(y~bs(x, 14))
matplot(x, cbind(y, funky(x), lmodb$fitted.values)
        , type = c("pll")
        , ylab = "y", pch = 20
        , lty = c(1, 1, 2), col = c(1, 1, 2))

data("odor")
lmod <- lm(odor ~ temp + 
             I(temp^2) +
             gas + I(gas^2) +
             temp:gas +
             pack + I(pack^2) +
             temp:pack + gas:pack
           , data = odor)
sumary(lmod)

lmodb <- lm(odor ~ # temp + 
             I(temp^2) +
             gas + I(gas^2) +
             # temp:gas +
             pack # + I(pack^2)
             # temp:pack + gas:pack
             , data = odor)
sumary(lmodb)

lmod_step <- step(lmod)
sumary(lmod_step)
odor[which.min(predict(lmod_step, odor)), ]
newvals <- data.frame(temp = c(0,0,1), gas = c(0,0,0), pack= c(0,1,1))
predict(lmod, newvals) # Yes, I predicted Karright!!!

lmod_pl <- lm(odor ~ polym(temp, gas, pack, degree = 2)
              , data = odor)
sumary(lmod_pl)

lmod_step_pl <- step(lmod_pl) # can't separate
