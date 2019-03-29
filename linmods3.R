library(faraway)
library(lattice)
library(ellipse)

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
  lmods <- lm(sample(Species) ~ Nearest + Scruz, data = gala)
  fstats[i] <- summary(lmods)$fstatistic[1]
}
sum(fstats)
histogram(fstats)
# alternative approach should be very slightly faster
set.seed(123)
for(i in 1:nreps) {
  if (i == 1) {
    lmods <- lm(sample(Species) ~ Nearest + Scruz, data = gala)  
    } else {
    lmods <- update(lmods, sample(Species) ~ .)
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
  lmods <- lm(sample(Species) ~ Nearest + sample(Scruz), data = gala)
  tstats[i] <- abs(summary(lmods)$coefficients[3, 3])
}
sum(tstats)
histogram(tstats)
mean(abs(tstats > abs(summary(lmod)$coefficients[3, 3])))

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