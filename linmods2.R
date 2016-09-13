library(faraway)
head(gala[, -2])
lmod <- lm(Species ~ Area +
             Elevation + Nearest +
             Scruz + Adjacent
           , data = gala)
summary(lmod)
sumary(lmod)

x <- model.matrix(~ Area +
                    Elevation + Nearest +
                    Scruz + Adjacent
                  , data = gala)
y <- gala$Species
xtxi <- solve(t(x) %*% x)
bhat <- xtxi %*% t(x) %*% y
# or
bhat <- solve(crossprod(x, x), crossprod(x, y))
# t(x) %*% x = crossprod(x, x)

deviance(lmod) # RSS
df.residual(lmod)
sqrt(deviance(lmod) / df.residual(lmod)) # RSE
summary(lmod)$sigma # RSE

xtxi <- summary(lmod)$cov.unscaled # solve(t(x) %*% x)
sqrt(diag(xtxi)) * summary(lmod)$sigma # coefficient SE * RSE
summary(lmod)$coef[, 2]

lmod <- lm(gamble ~ sex + status + verbal + income, data = teengamb)
sumary(lmod)
# r.squared = 1 - RSS / mean centred values
1 - sum((lmod$fitted.values - teengamb$gamble)^2) / sum((teengamb$gamble - mean(teengamb$gamble))^2)
1 - deviance(lmod) / sum((teengamb$gamble - mean(teengamb$gamble))^2)

which.max(abs(teengamb$gamble - lmod$fitted.values))
which.max(resid(lmod))
mean(resid(lmod)) # effectively zero
median(resid(lmod))
cor(resid(lmod), fitted(lmod)) # effectively zero, randomly distributed around the least squares line
cor(resid(lmod), teengamb$income) # effectively zero
coef(lmod)[2]

lmod <- lm(wage ~ exper + educ, data = uswages)
sumary(lmod)
lmod <- lm(log(wage) ~ exper + educ, data = uswages)
sumary(lmod)

set.seed(201)
x <- 1:20
y <- x + rnorm(20)
x <- model.matrix(~ x + I(x^2))

xtxi <- solve(t(x) %*% x)
bhat <- xtxi %*% t(x) %*% y
bhat <- solve(crossprod(x, x), crossprod(x, y))

lmod <- lm(y ~ x.mod.mat)
sumary(lmod)

x <- model.matrix(~ x + I(x^2) + I(x^3))

xtxi <- solve(t(x) %*% x) # does not solve.

predictors <- c("lpsa", "lweight", "svi", "lbph", "age", "lcp", "pgg45", "gleason")
rsq <- numeric(length(predictors))
fmla <- "lcavol ~ lpsa"

for (i in seq_along(predictors)) {
  lmod <- lm(as.formula(fmla), data = prostate)
  rsq[i] <- summary(lmod)$r.squared
  if(i == max(seq_along(predictors))) break
  fmla <- paste(fmla, "+", predictors[i + 1])
}
plot(rsq, type = "l"
     , xlab = "Number of predictors")

plot(lcavol~lpsa, data = prostate)
abline(lm(lcavol~lpsa, data = prostate), col = "red")
abline(lm(lpsa~lcavol, data = prostate), col = "green") # it's parallel!

lched <- lm(taste ~ Acetic + H2S + Lactic, data = cheddar)
sumary(lched)
cor(cheddar$taste # response
    , lched$fitted.values)^2 # this is equal to the r.squared stat

lched <- lm(taste ~ Acetic + H2S + Lactic - 1, data = cheddar)
sumary(lched) # when the intercept is ommitted, 
# r.squared is wrong in the lm summary

cor(cheddar$taste # however, this calculation is correct
    , lched$fitted.values)^2 # this is equal to the r.squared stat

x <- model.matrix(~ x1 + x2 + x3 + x4, data = wafer)
lmod <- lm(resist ~ x, data = wafer)
sumary(lmod)
x <- model.matrix(~ x1 + x2 + x3, data = wafer)
lmod <- lm(resist ~ x, data = wafer)
sumary(lmod)
