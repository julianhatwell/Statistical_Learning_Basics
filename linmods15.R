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
plot(bright~operator, data = pulp)
summary(aov(bright~operator, data = pulp))
summary(lm(bright~operator, data = pulp))
stripchart(bright~operator, data = pulp
           , vertical = TRUE, method = "stack")
stripplot(jitter(bright)~operator, data = pulp)
stripplot(bright~operator, data = pulp
          , horizontal = FALSE
          , jitter.data = TRUE
)

# bonferroni
pvals <- summary(lmod)$coef[, 4]
padj <- p.adjust(pvals, method = "bonferroni")
coef(lmod)[padj < 0.05]
# benjamin and hochberg method, better for large numbers of comparisons
names(which(sort(pvals) < 1:length(pvals)*0.05/length(pvals)))
# more conveniently
padj <- p.adjust(pvals, method = "fdr")
coef(lmod)[padj < 0.05]
# with 4 choose 2 pairwise tests, none are signif

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

data("PlantGrowth")
plot(weight~group, data = PlantGrowth)
stripchart(weight~group,data = PlantGrowth
           , vertical = TRUE, method = "stack")
stripplot(weight~group, data = PlantGrowth
          , horizontal = FALSE
          , jitter.data = TRUE
)
          
lmod <- lm(weight~group, data = PlantGrowth)
lmnull <- lm(weight~1, data = PlantGrowth)
anova(lmnull, lmod)
summary(lmod)
qqnorm(resid(lmod)) # skew?
qqline(resid(lmod))
plot(jitter(fitted(lmod)), resid(lmod)) # heterosked?
# no evidence of non-constant variance
bartlett.test(weight~group, data = PlantGrowth)
# to see if group variances are the same - no evidence of diff
leveneTest(weight~group, data = PlantGrowth)
# abolute median resids (manual Levene's)
med <- with(PlantGrowth, tapply(weight, group, median))
ar <- with(PlantGrowth, abs(weight-med[group]))
anova(lm(ar~group, data = PlantGrowth)) # same

# family wise error
pvals <- summary(lmod)$coef[, 4]
padj <- p.adjust(pvals, method = "bonferroni")
coef(lmod)[padj < 0.05]
# benjamin and hochberg method, better for large numbers of comparisons
names(which(sort(pvals) < 1:length(pvals)*0.05/length(pvals)))
# more conveniently
padj <- p.adjust(pvals, method = "fdr")
coef(lmod)[padj < 0.05]
# with 3 choose 2 pairwise tests, none are signif, but none were when unadjusted in any case

PlantGrowth$cgrp <- factor(ifelse(PlantGrowth$group == "ctrl", "ctrl", "trt"))
summary(lm(weight~cgrp, data = PlantGrowth))
leveneTest(weight~cgrp, data = PlantGrowth)

data("infmort")
summary(aov(income~region, data = infmort))
summary(lm(income~region, data = infmort))
plot(income~region, data = infmort)
plot(log(income)~region, data = infmort)
summary(lm(log(income)~region, data = infmort))

data("anaesthetic")
plot(breath~tgrp, data = anaesthetic)
stripchart(breath~tgrp, data = anaesthetic
           , vertical = TRUE, method = "stack")
stripplot(breath~tgrp, data = anaesthetic
          , horizontal = FALSE
          , jitter.data = TRUE
)
ggplot(data = anaesthetic
       , aes(x = tgrp, y = breath)) +
  geom_boxplot() +
  geom_point(position = position_jitter(width = 0.1))
summary(lm(breath~tgrp, data = anaesthetic))
# box cox will fail because presence of zeros
ggplot(data = anaesthetic
       , aes(x = tgrp, y = sqrt(breath))) +
  geom_boxplot() +
  geom_point(position = position_jitter(width = 0.1))
lmod <- lm(sqrt(breath)~tgrp, data = anaesthetic)
summary(lmod)
qqnorm(resid(lmod)) # not good
qqline(resid(lmod)) 
plot(jitter(fitted(lmod)), resid(lmod)) # heterosked?
# no evidence of non-constant variance
bartlett.test(breath~tgrp, data = anaesthetic) # not satisfactory
# to see if group variances are the same - no evidence of diff
leveneTest(breath~tgrp, data = anaesthetic)
# Wikipedia says that Bartlett's test is more sensitive to violations of normality than Levene's test.
# So you may have non-normal data instead of heteroscedastic data.

data("butterfat")
maturebutter <- butterfat[butterfat$Age == "Mature",]
plot(Butterfat ~ Breed, data = maturebutter)
ggplot(data = maturebutter
       , aes(x = Breed, y = Butterfat)) +
  geom_boxplot() +
  geom_point(position = position_jitter(width = 0.1))
stripchart(Butterfat ~ Breed, data = maturebutter
           , vertical = TRUE, method = "stack")
stripplot(Butterfat~Breed, data = maturebutter
          , xlab = "bfat"
          , horizontal = FALSE
          , jitter.data = TRUE
)
summary(aov(Butterfat ~ Breed, data = maturebutter))
lmod <- lm(Butterfat ~ Breed, data = maturebutter)
summary(lmod)
qqnorm(resid(lmod)) # not good
qqline(resid(lmod)) 
plot(jitter(fitted(lmod)), resid(lmod)) # heterosked?

bartlett.test(Butterfat ~ Breed, data = maturebutter) 
# to see if group variances are the same - no evidence of diff
leveneTest(Butterfat ~ Breed, data = maturebutter)

(tci <- TukeyHSD(aov(Butterfat ~ Breed, data = maturebutter)))
plot(tci)

data("denim")
plot(waste~supplier, data = denim)
summary(aov(waste~supplier, data = denim))
lmod <- lm(waste~supplier, data = denim)
summary(lmod)
qqnorm(resid(lmod)) # outliers might be a problem
qqline(resid(lmod)) 
plot(jitter(fitted(lmod)), resid(lmod)) # heterosked?

bartlett.test(waste~supplier, data = denim) # this is sensitive to non-normality. confirms problem with outlier
leveneTest(waste~supplier, data = denim)

denmax <- denim[denim$waste < 30,]
nrow(denmax) # two outliers

plot(waste~supplier, data = denmax)
summary(aov(waste~supplier, data = denmax))
lmod <- lm(waste~supplier, data = denmax)
summary(lmod)
qqnorm(resid(lmod)) # outliers might be a problem
qqline(resid(lmod)) 
plot(jitter(fitted(lmod)), resid(lmod)) # heterosked?

bartlett.test(waste~supplier, data = denmax) # now reveals differet variances
leveneTest(waste~supplier, data = denmax) # now reveals differet variances

# bonferroni
pvals <- summary(lmod)$coef[, 4]
padj <- p.adjust(pvals, method = "bonferroni")
coef(lmod)[padj < 0.05]
# benjamin and hochberg method, better for large numbers of comparisons
names(which(sort(pvals) < 1:length(pvals)*0.05/length(pvals)))
# more conveniently
padj <- p.adjust(pvals, method = "fdr")
coef(lmod)[padj < 0.05]
# with 4 choose 2 pairwise tests, none are signif
(tci <- TukeyHSD(aov(waste~supplier, data = denmax)))
plot(tci)
