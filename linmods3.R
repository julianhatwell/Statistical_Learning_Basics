library(faraway)
library(lattice)
library(latticeExtra)
library(ellipse)
library(ggplot2)

lmod <- lm(gamble ~ sex + status + verbal + income, data = teengamb)

Fstat <- function(y, fitted.y, p) {
  TSS <- sum((y - mean(y))^2)
  RSS <- sum((fitted.y - y)^2)
  numerator <- (TSS - RSS) / (p - 1)
  denominator <- RSS / (length(y) - p)
  numerator / denominator
}

Fstat(teengamb$gamble
      , lmod$fitted.values
      , length(lmod$coefficients))

model.fstat <- summary(lmod)$fstatistic
pf(model.fstat[1], model.fstat[2], model.fstat[3], lower.tail = FALSE)
summary(lmod)

data("gala", package = "faraway")
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
nullmod <- lm(Species ~ 1, data = gala)
anova(nullmod, lmod)
# manual calc
(rss0 <- deviance(nullmod))
(rss <- deviance(lmod))
(df0 <- df.residual(nullmod))
(df <- df.residual(lmod))
fstat <- ((rss0-rss)/(df0-df))/(rss/df)
summary(lmod) # bottom line of output

# testing effect of Area and Nearest when the other predictors are kept in the model
# must use F-test when testing hypothesis includes more than one predictor, can't combine t-tests
lmods <- lm(Species ~ Elevation + Nearest + Scruz, data = gala)
anova(lmods, lmod)

# subspace testing - can we substitute a linear combo of some predictors?
lmods <- lm(Species ~ I(Area + Nearest) + Elevation + Nearest + Scruz, data = gala)
anova(lmods, lmod) # null is rejected, so more granular model is chosen.

# fix a level for a parameter
lmods <- lm(Species ~ Area + offset(0.5 * Elevation) + Nearest + Scruz + Adjacent, data = gala)
anova(lmods, lmod) # null is rejected, so free parameter is a better choice
summary(lmods)

# permutation testing
lmod <- lm(Species ~ Nearest + Scruz, data = gala)
lms <- summary(lmod)
lms$fstatistic
1-pf(lms$fstatistic[1], lms$fstatistic[2], lms$fstatistic[3])
nreps <- 4000
set.seed(123)
fstats <- numeric(nreps)
for(i in 1:nreps) {
  lmods <- lm(sample(Species, replace = TRUE) ~ Nearest + Scruz, data = gala)
  fstats[i] <- summary(lmods)$fstatistic[1]
}
sum(fstats)
histogram(fstats)
# alternative approach should be very slightly faster
set.seed(123)
for(i in 1:nreps) {
  if (i == 1) {
    lmods <- lm(sample(Species, replace = TRUE) ~ Nearest + Scruz, data = gala)  
    } else {
    lmods <- update(lmods, sample(Species, replace = TRUE) ~ .)
  }
  fstats[i] <- summary(lmods)$fstatistic[1]
}
sum(fstats)
histogram(fstats)

# result - proportion that are greater than the traget statistic
mean(fstats > lms$fstatistic[1])

# if we test just one predictor, permute the predictor instead
summary(lmod)$coefficients[3, ]
tstats <- numeric(nreps)

set.seed(123)
for(i in 1:nreps) {
  lmods <- update(lmod, . ~ . -Scruz + sample(Scruz, replace = TRUE))
  tstats[i] <- summary(lmods)$coefficients[3, 3]
}
sum(tstats)
histogram(tstats)
mean(abs(tstats) > abs(summary(lmod)$coefficients[3, 3]))

# confints
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
sumary(lmod)

# manual calc
cfs <- summary(lmod)$coefficients[, 1]
nr <- length(summary(lmod)$coefficients[, 1])
ses <- summary(lmod)$coefficients[, 2]
dfs <- summary(lmod)$df[2]

matrix(cfs, nrow = nr, ncol = 2) +
  matrix(c(-1, 1), nrow = nr, ncol = 2, byrow = TRUE) *
  summary(lmod)$coefficients[, 2] *
  qt(0.975, dfs)

# easy calc
confint(lmod)

# bivariate confints
plot(ellipse(lmod, c(2, 6)), type = "l", ylim = c(-0.13, 0)) # area and adjacent
points(coef(lmod)[2], coef(lmod)[6], pch = 19)
abline(v=confint(lmod)[2,], lty = 2)
abline(h=confint(lmod)[6,], lty = 2)
abline(h=0, col = "red", lty = 3)
abline(v=0, col = "red", lty = 3)
points(0, 0, cex=2, col="red")
text(-0.06, -0.05, "A")
text(0.024, -0.036, "B")

# points A and B do not agree with the univariate confints.

# bootstrapping errors to get an empirical estimate
set.seed(123)
coefmat <- matrix(NA, nreps, 6)
resids <- residuals(lmod)
preds <- fitted(lmod)
for (i in 1:nreps) {
  booty <- preds + sample(resids, replace = TRUE)
  bmod <- update(lmod, booty ~ .)
  coefmat[i, ] <- coef(bmod)
}
colnames(coefmat) <- c("Intercept", colnames(gala[, 3:7]))
apply(coefmat, 2, function(x) {quantile(x, c(0.025, 0.975))})

coefmat <- data.frame(coefmat)
ggplot(coefmat, aes(x = Area)) +
  geom_density() +
  geom_vline(xintercept = confint(lmod)["Area", ])
ggplot(coefmat, aes(x = Adjacent)) +
  geom_density() +
  geom_vline(xintercept = confint(lmod)["Adjacent", ])

# ex
lmod <- lm(lpsa ~ ., data = prostate)
lmods <- summary(lmod)
q95 <- qt(0.975, lmods$df[2])
q90 <- qt(0.95, lmods$df[2])
lmods$coefficients["age", 1] + c(-1, 1) * lmods$coefficients["age", 2] * q95
lmods$coefficients["age", 1] + c(-1, 1) * lmods$coefficients["age", 2] * q90

# bivariate confints - age and lbph
plot(ellipse(lmod, c(4, 5)), type = "l") # area and adjacent
points(coef(lmod)[4], coef(lmod)[5], pch = 19)
abline(v=confint(lmod)[4,], lty = 2)
abline(h=confint(lmod)[5,], lty = 2)
abline(h=0, col = "red", lty = 3)
abline(v=0, col = "red", lty = 3)
points(0, 0, cex=2, col="red")

# permutation test
summary(lmod)$coefficients[4, ]
tstats <- numeric(nreps)

set.seed(123)
for(i in 1:nreps) {
  bmod <- update(lmod, . ~ . - age + sample(age, replace = TRUE))
  tstats[i] <- summary(bmod)$coefficients[4, 3]
}
histogram(tstats)
mean(abs(tstats) > abs(summary(lmod)$coefficients[4, 3]))

which(lmods$coefficients[, 4] > 0.05)[-1]
redmod <- update(lmod, .~. -age -lbph - lcp - gleason - pgg45)
anova(redmod,lmod) # no sig diff in anova, prefer smaller model

data("cheddar")
ch <- lm(taste~., data = cheddar)
sumary(ch) # H2S and Lactic are significant

ch_adj <- lm(taste ~ exp(Acetic) + exp(H2S) + Lactic, data = cheddar)
sumary(ch_adj) # only lactic is significant at 5%
plot(ch_adj)

plot(resid(ch_adj))
plot(resid(ch))

# ftest
# permutation testing
lms <- summary(ch)
lms$fstatistic
1-pf(lms$fstatistic[1], lms$fstatistic[2], lms$fstatistic[3])
nreps <- 4000

resids <- residuals(ch_adj)
preds <- fitted(ch_adj)

set.seed(123)
fstats <- numeric(nreps)
for (i in 1:nreps) {
  booty <- preds + sample(resids, replace = TRUE)
  bmod <- update(ch_adj, booty ~ .)
  fstats[i] <- summary(bmod)$fstatistic[1]
}
quantile(fstats, c(0.025, 0.975))

# result - proportion that are greater than the traget statistic
mean(fstats > lms$fstatistic[1])

fstats <- data.frame(fstats)
ggplot(coefmat, aes(x = fstats)) +
  geom_density() +
  geom_vline(xintercept = lms$fstatistic[1])

# we might prefer the original model because we know the normal assumptions hold. 
# But it doesn't actually matter

lms$coefficients[3, 1] * 0.01 # for a 0.01 increase on this scale

sapply(seq(0:100), function(x) {exp(x + 0.01) / exp(x)}) # in increase of 0.01 on the log scale
# equivalent to an increase of 1%

data("teengamb")
summary(teengamb)
gamb <- lm(gamble~., data = teengamb)
sumary(gamb) # girls (sex == 0) are less likely to gamble (-22.12). sex and income are signif
gamb1 <- lm(gamble~income, data = teengamb)
anova(gamb1, gamb) # larger model is significantly better

gamb2 <- update(gamb1, .~. + sex, data=teengamb)
anova(gamb1, gamb2, gamb)

data("sat")
fit.sat <- lm(total ~ expend + salary + ratio, data = sat)
sumary(fit.sat) # none are signif on their own
confint(fit.sat)["salary", ]
fit.null <- lm(total ~ 1, data = sat)
anova(fit.null, fit.sat)  # model with all three is signif better than null

# permutation testing
lms <- summary(fit.sat)
lms$fstatistic
1-pf(lms$fstatistic[1], lms$fstatistic[2], lms$fstatistic[3])
nreps <- 4000
set.seed(123)
fstats <- numeric(nreps)
for(i in 1:nreps) {
  fit.sat <- update(fit.sat, sample(total, replace = TRUE) ~ .)
  fstats[i] <- summary(fit.sat)$fstatistic[1]
}
sum(fstats)
histogram(fstats)

# result - proportion that are greater than the traget statistic
mean(fstats > lms$fstatistic[1])# so the calculated one was definitely signif

# if we test just one predictor, permute the predictor instead
fit.sat <- lm(total ~ expend + salary + ratio, data = sat)
tstats <- numeric(nreps)

set.seed(123)
for(i in 1:nreps) {
  b.fit <- update(fit.sat, . ~ . -salary + sample(salary, replace = TRUE))
  tstats[i] <- summary(b.fit)$coefficients[4, 3]
}
histogram(tstats)
mean(abs(tstats) > abs(summary(fit.sat)$coefficients[3, 3])) # none are greater. highly signif

# manual calc
cfs <- summary(fit.sat)$coefficients[, 1]
nr <- length(summary(fit.sat)$coefficients[, 1])
ses <- summary(fit.sat)$coefficients[, 2]
dfs <- summary(fit.sat)$df[2]

matrix(cfs, nrow = nr, ncol = 2) +
  matrix(c(-1, 1), nrow = nr, ncol = 2, byrow = TRUE) *
  summary(fit.sat)$coefficients[, 2] *
  qt(0.975, dfs)

fit.sat2 <- update(fit.sat, .~. + takers)
anova(fit.sat, fit.sat2)

# manual calc
cfs <- summary(fit.sat2)$coefficients[, 1]
nr <- length(summary(fit.sat2)$coefficients[, 1])
ses <- summary(fit.sat2)$coefficients[, 2]
dfs <- summary(fit.sat2)$df[2]

matrix(cfs, nrow = nr, ncol = 2) +
  matrix(c(-1, 1), nrow = nr, ncol = 2, byrow = TRUE) *
  summary(fit.sat2)$coefficients[, 2] *
  qt(0.975, dfs)

summary(fit.sat2)

# see relationship here https://stats.stackexchange.com/questions/56881/whats-the-relationship-between-r2-and-f-test

data("happy")
happ.fit <- lm(happy ~ ., data = happy)
sumary(happ.fit) # only love at the 1% level
table(happy$happy)
ggplot(aes(x = happy), data = happy) +
  geom_bar()

# if we test just one predictor, permute the predictor instead
money_t <- summary(happ.fit)$coefficients[2, 3]
tstats <- numeric(nreps)

set.seed(123)
for(i in 1:nreps) {
  b.fit <- update(happ.fit, . ~ . -money + sample(money, replace = TRUE))
  # b.fit <- lm(happy~sex + love + work + sample(money, replace = TRUE), data = happy)
  tstats[i] <- summary(b.fit)$coefficients[5, 3]
}
x <- seq(-3, 3, length.out = 300)
y <- dt(x, df = nrow(happy)) * 100
histogram(tstats) + as.layer(
  xyplot(y~x, type="l")
)
mean(abs(tstats) > abs(money_t)) # none are greater. highly signif

# bootstrapping errors to get an empirical estimate
coefmat <- matrix(NA, nreps, 5)
resids <- residuals(happ.fit)
preds <- fitted(happ.fit)
set.seed(123)
for (i in 1:nreps) {
  booty <- preds + sample(resids, replace = TRUE)
  b.fit <- update(happ.fit, booty ~ .)
  coefmat[i, ] <- coef(b.fit)
}
colnames(coefmat) <- c("Intercept", colnames(happy[, 2:5]))
b.confint <- apply(coefmat, 2, function(x) {quantile(x, c(0.025, 0.975))})

coefmat <- data.frame(coefmat)
ggplot(coefmat, aes(x = money)) +
  geom_density() +
  geom_vline(xintercept = confint(happ.fit)["money", ], lty = 3, color = "blue") +
  geom_vline(xintercept = b.confint[, "money"])
ggplot(coefmat, aes(x = sex)) +
  geom_density() +
  geom_vline(xintercept = confint(happ.fit)["sex", ], lty = 3, color = "blue") +
  geom_vline(xintercept = b.confint[, "sex"])
ggplot(coefmat, aes(x = love)) +
  geom_density() +
  geom_vline(xintercept = confint(happ.fit)["love", ], lty = 3, color = "blue") +
  geom_vline(xintercept = b.confint[, "love"])
ggplot(coefmat, aes(x = work)) +
  geom_density() +
  geom_vline(xintercept = confint(happ.fit)["work", ], lty = 3, color = "blue") +
  geom_vline(xintercept = b.confint[, "work"])

data("punting")
punt.fit <- lm(Distance~RStr + LStr + RFlex + LFlex, data = punting)
sumary(punt.fit) # none are signif
summary(punt.fit) # see F-test. Collectively they are signif

# permutation testing
lms <- summary(punt.fit)
lms$fstatistic
1-pf(lms$fstatistic[1], lms$fstatistic[2], lms$fstatistic[3])
nreps <- 4000
set.seed(123)
fstats <- numeric(nreps)
for(i in 1:nreps) {
  b.fit <- update(punt.fit, sample(Distance, replace = TRUE) ~ .)
  fstats[i] <- summary(b.fit)$fstatistic[1]
}
histogram(fstats)

# result - proportion that are greater than the traget statistic
mean(fstats > lms$fstatistic[1])# so the calculated one was definitely signif
punt.fit.rstr <- update(punt.fit, .~.-RStr)
punt.fit.lstr <- update(punt.fit, .~.-LStr)
anova(punt.fit.rstr, punt.fit)
anova(punt.fit.lstr, punt.fit)

# bivariate confints
plot(ellipse(punt.fit, c(2, 3)), type = "l") # area and adjacent
points(coef(punt.fit)[2], coef(punt.fit)[3], pch = 19)
abline(v=confint(punt.fit)[2,], lty = 2)
abline(h=confint(punt.fit)[3,], lty = 2)
abline(h=0, col = "red", lty = 3)
abline(v=0, col = "red", lty = 3)
points(0, 0, cex=2, col="red")

# test that two separate variables is any different from a linear comb
punt.fit.ovr <- lm(Distance ~ I(RStr + LStr), data = punting)
punt.fit.two <- lm(Distance ~ RStr + LStr, data = punting)

anova(punt.fit.ovr, punt.fit.two)
punt.fit.ovr.two <- lm(Distance~I(RStr + LStr) + RFlex + LFlex, data = punting)
anova(punt.fit.ovr.two, punt.fit)

punt.fit.rflx <- update(punt.fit, .~.-RFlex)
punt.fit.lflx <- update(punt.fit, .~.-LFlex)

anova(punt.fit.rflx, punt.fit)
anova(punt.fit.lflx, punt.fit)

punt.fit.hang <- update(punt.fit, Hang ~ .)
summary(punt.fit.hang)
