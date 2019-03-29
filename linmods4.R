library(faraway)
data("fat", package = "faraway")
lmod <- lm(brozek~age + weight + height + neck + 
              chest + abdom + hip + thigh + knee +
              ankle + biceps + forearm + wrist
           , data = fat)
x <- model.matrix(lmod)
y <- fat$brozek

# typical male
x0 <- apply(x, 2, median)

# manual calc predict
(y0 <- x0%*%coef(lmod))
(y0 <- sum(x0*coef(lmod)))
# easy calc
(y0 <- predict(lmod, newdata = data.frame(t(x0))))

df.residual(lmod)
summary(lmod)$sigma # RSE

xtxi <- solve(t(x) %*% x) # same as: xtxi <- summary(lmod)$cov.unscaled 
H <- xtxi %*% t(x) # Hat matrix
H[, 1:2] # as wide as number of training instances
bhat <- xtxi %*% t(x) %*% y # the coefficients, same as: bhat <- solve(crossprod(x, x), crossprod(x, y))
x %*% bhat # same as predict function

sqrt(1 + t(x0) %*% xtxi %*% x0) # pred
sqrt(t(x0) %*% xtxi %*% x0) # conf

# what happens here is
# by matrix mult, the square root is taken of the
# se residual * square values of the pred instance * xtxi
# this latter term is equavalent to the hat matrix of one instance mult by that instance
# So this basically takes the residual variance and weights it appropriately accross each coefficient
# Square and square root simply gets everything to be positive
# for the pred interval, 1 + is included in the equation
# this includes a whole residual deviance into the equation, as well as the amount for the coeffs

# se resid * sqrt(Hat matrix for pred instance * pred instance) * t-distribution quantiles
confint <- c(1, -1) * (summary(lmod)$sigma * sqrt(t(x0) %*% xtxi %*% x0) * qt(0.025, lmod$df.residual))[1, 1]
y0 + confint
predict(lmod, newdata = data.frame(t(x0)), interval = "confidence")

# same again but for pred int
predint <- c(1, -1) * (summary(lmod)$sigma * sqrt(1 + t(x0) %*% xtxi %*% x0) * qt(0.025, lmod$df.residual))[1, 1]
y0 + confint
predict(lmod, newdata = data.frame(t(x0)), interval = "prediction")

# extrapolation
(x1 <- apply(x, 2, function(z) {quantile(z, 0.95)}))
predict(lmod, newdata = data.frame(t(x1)), interval = "confidence")
predict(lmod, newdata = data.frame(t(x1)), interval = "prediction")

# autoregression
data("airpass", package = "faraway")
plot(pass~year, airpass, type="l")
lmod <- lm(log(pass)~year, data = airpass)
lines(exp(predict(lmod))~year, data = airpass)

# make the autoregressive model
lagdf <- embed(log(airpass$pass), 14)
colnames(lagdf) <- c("y", paste0("lag", 1:13))
lagdf <- data.frame(lagdf)
armod <- lm(y~lag1 + lag12 + lag13, data = lagdf)
sumary(armod)
lines(airpass[14:144, "year"], exp(predict(armod)), lty=2, col = "red")

# predict the next value
lagdf[131, ]
lag1 <- lagdf[131, "y"]
lag12 <- lagdf[131, "lag11"]
lag13 <- lagdf[131, "lag12"]
predict(armod, newdata = data.frame(lag1, lag12, lag13), interval = "prediction")

# ex
data("prostate", package = "faraway")
lmod <- lm(lpsa~., data = prostate)
newdata <- data.frame(lcavol=1.44692, lweight = 3.62301
                      , age=65.00000, lbph=0.30010
                      , svi=0.00000, lcp=-0.79851
                      , gleason=7.00000, pgg45=15.00000)
predict(lmod, newdata = newdata, interval = "prediction")
newdata <- data.frame(lcavol=1.44692, lweight = 3.62301
                      , age=20.00000, lbph=0.30010
                      , svi=0.00000, lcp=-0.79851
                      , gleason=7.00000, pgg45=15.00000)
predict(lmod, newdata = newdata, interval = "prediction")
hist(prostate$age) # age 20 years outside the seen data range.

sumary(lmod) # remove what is outside above the 5% level
lmod <- update(lmod, .~. -age -lbph - lcp - gleason - pgg45)
sumary(lmod)
predict(lmod, newdata = newdata, interval = "prediction")
# the interval is narrower because the residual variance is less
# noisy variables have been removed so more confident of pred

data("teengamb", package = "faraway")
lmod <- lm(gamble~., data=teengamb)
sqmod <- lm(sqrt(gamble)~., data=teengamb)
sumary(lmod)
male_av_all <- apply(teengamb, 2, mean)
male_av_all["sex"] <- 0; male_av_all["gamble"] <- 0
male_av_male <- apply(teengamb[teengamb$sex == 0, ], 2, mean)
male_av_male["gamble"] <- 0

predict(lmod, newdata = data.frame(t(male_av_all)), interval = "prediction")
predict(lmod, newdata = data.frame(t(male_av_male)), interval = "prediction")
predict(sqmod, newdata = data.frame(t(male_av_all)), interval = "prediction")^2
predict(sqmod, newdata = data.frame(t(male_av_male)), interval = "prediction")^2

male_max_all <- apply(teengamb, 2, max)
male_max_all["sex"] <- 0; male_max_all["gamble"] <- 0
# all male same
predict(lmod, newdata = data.frame(t(male_max_all)), interval = "prediction")
predict(sqmod, newdata = data.frame(t(male_max_all)), interval = "prediction")^2

predict(sqmod, newdata = data.frame(sex = 1, status=20
                                    , income=1, verbal=10)
        , interval = "prediction")

data("snail", package="faraway")
xtabs(water~temp + humid, data = snail)/4
xt <- xtabs(water~temp + humid, data = snail)/4
mean(xt[, 1:2])
lmod <- lm(water~temp + humid, data = snail)
predict(lmod
        , newdata = data.frame(temp=25
                               , humid=60)
        , interval = "prediction")
lmod <- lm(water~temp + humid, data = snail)
predict(lmod
        , newdata = data.frame(temp=30
                               , humid=75)
        , interval = "prediction")
sumary(lmod)
predict(lmod
        , newdata = data.frame(temp=0
                               , humid=0)
        , interval = "prediction")
test_h <- (80 - (52.6 + -0.18 * 25)) / 0.47
predict(lmod
        , newdata = data.frame(temp=25
                               , humid=test_h)
        , interval = "prediction")

test_t <- 1 / -0.18
test_h <- 1 / 0.47
predict(lmod
        , newdata = data.frame(temp=test_t
                               , humid=test_h)
        , interval = "prediction")

plot(mdeaths, type = "l")
md <- embed(mdeaths, 14)
colnames(md) <- c("deaths", paste0("lag", 1:13))
md <- data.frame(md)
armod <- lm(deaths~lag1 + lag12 + lag13, data = md)
sumary(armod)
armod <- lm(log(deaths)~lag1 + lag12 + lag13, data = md)
sumary(armod)
lag1 <- md[59, "deaths"]
lag12 <- md[59, "lag11"]
lag13 <- md[59, "lag12"]
(pred1 <- exp(predict(armod
            , newdata = data.frame(lag1, lag12, lag13)
            , interval = "prediction")))
lag1 <- pred1[1]
lag12 <- md[59, "lag10"]
lag13 <- md[59, "lag11"]
(pred2 <- exp(predict(armod
                     , newdata = data.frame(lag1, lag12, lag13)
                     , interval = "prediction")))
preds <- fitted(armod)
preds <- ts(preds, start = c(1975, 2), frequency = 12)
lines(exp(preds), lty=2, col="red")

lmod <- lm(brozek~age + weight + height + neck + 
             chest + abdom + hip + thigh + knee +
             ankle + biceps + forearm + wrist
           , data = fat)
lmods <- lm(brozek~ age + weight + height + abdom, data = fat)
anova(lmods, lmod)
sumary(lmod)
sumary(lmods)
xmean <- apply(model.matrix(lmod), 2, mean)
predict(lmod, newdata = data.frame(t(xmean))
        , interval = "prediction")
predict(lmods, newdata = data.frame(t(xmean))
        , interval = "prediction")
model.matrix(lmods)[25:50,]
fat_outliers <- fat[-c(39, 42), ]
lmod <- lm(brozek~age + weight + height + neck + 
             chest + abdom + hip + thigh + knee +
             ankle + biceps + forearm + wrist
           , data = fat_outliers)

lmods <- lm(brozek~ age + weight + height + abdom
            , data = fat_outliers)
anova(lmods, lmod)
predict(lmod, newdata = data.frame(t(xmean))
        , interval = "prediction")
predict(lmods, newdata = data.frame(t(xmean))
        , interval = "prediction")
