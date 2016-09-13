library(faraway)

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