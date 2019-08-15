data("coagulation")
coagulation
plot(coag~diet, data = coagulation)
stripchart(coag~diet, data = coagulation
           , vertical = TRUE, method = "stack")
lmod <- lm(coag~diet, data = coagulation)
sumary(lmod)
round(coef(lmod), 1)
model.matrix(lmod)
anova(lmod)

lmod <- lm(coag~diet-1, data = coagulation)
sumary(lmod)
lmnull <- lm(coag ~ 1, data = coagulation)
anova(lmnull, lmod)

qqnorm(resid(lmod))
qqline(resid(lmod))
plot(jitter(fitted(lmod)), resid(lmod))
med <- with(coagulation, tapply(coag, diet, median))
ar <- with(coagulation, abs(coag-med[diet]))
anova(lm(ar~diet, data = coagulation)) # Levene's Test
# no evidence of non-constant variance
bartlett.test(coag~diet, data = coagulation)

lmod <- lm(coag~diet, data = coagulation)
summary(lmod)
confint(lmod)
5 + c(-1, 1) * qt(0.975, 20) * 1.53 # se of this coef
