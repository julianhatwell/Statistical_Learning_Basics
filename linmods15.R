library(faraway)
library(ggplot2)
library(lattice)
library(car)
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

aov1 <- aov(coag~diet, data = coagulation)
summary(aov1)

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
5 + c(-1, 1) * qt(0.975, 20) * 1.53 # se of this coef, good for one test
# manual calc of tukey's honest significant difference, the terms at the end relate to the sample sizes
5 + c(-1, 1) * qtukey(0.95, 4, 20)/sqrt(2) * summary(lmod)$sigma * sqrt((1/4) + (1/6))
# or easier
(tci <- TukeyHSD(aov(coag~diet, data = coagulation)))
plot(tci)


data(jsp)
jsp$mathcent <- jsp$math - mean(jsp$math)
ggplot(data = jsp, aes(x = school, y = mathcent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Centered Math Scores")

lmod <- lm(mathcent ~ school - 1, data = jsp)
summary(lmod)
anova(lmod)
aov(mathcent ~ school - 1, data = jsp)

# bonferroni
pvals <- summary(lmod)$coef[, 4]
padj <- p.adjust(pvals, method = "bonferroni")
coef(lmod)[padj < 0.05]
# benjamin and hochberg method, better for large numbers of comparisons
names(which(sort(pvals) < 1:length(pvals)*0.05/length(pvals)))
# more conveniently
padj <- p.adjust(pvals, method = "fdr")
coef(lmod)[padj < 0.05]

data(pulp)
names(pulp)
plot(bright~operator, data = pulp)
summary(aov(bright~operator, data = pulp))
summary(lm(bright~operator, data = pulp))
stripchart(bright~operator, data = pulp
           , vertical = TRUE, method = "stack")
stripplot(jitter(bright)~operator, data = pulp
           , vertical = TRUE, method = "stack")
stripplot(bright~jitter(as.numeric(operator), 0.5), data = pulp
          , vertical = TRUE, method = "stack"
          , xlab = "operator"
          , scales = list(x = list(labels = c("a","a", "b", "c", "d"))))

data("chickwts")
plot(weight~feed, data = chickwts)
lmod <- lm(weight~feed, data = chickwts)
summary(lmod)
# no intercept model against null
lmod <- lm(weight~feed - 1, data = chickwts)
sumary(lmod)
lmnull <- lm(weight~1, data = chickwts)
anova(lmnull, lmod)

qqnorm(resid(lmod))
qqline(resid(lmod))
plot(jitter(fitted(lmod)), resid(lmod))
med <- with(chickwts, tapply(weight, feed, median))
ar <- with(chickwts, abs(weight-med[feed]))
anova(lm(ar~feed, data = chickwts)) # Levene's Test
# no evidence of non-constant variance
bartlett.test(weight~feed, data = chickwts)
# to see if group variances are the same
leveneTest(weight~feed, data = chickwts)
