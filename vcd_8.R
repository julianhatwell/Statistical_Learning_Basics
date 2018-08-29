library(vcd)
library(vcdExtra)
library(MASS)
library(car)
library(VGAM)
library(rms)
library(reshape2)
library(ggplot2)
library(directlabels)
library(effects)
library(nnet)

data("Arthritis", package = "vcd")
head(Arthritis$Improved, 8)
arth.polr <- polr(Improved ~ Sex + Treatment + Age,
                  data = Arthritis, Hess = TRUE)
summary(arth.polr)
Anova(arth.polr)

# watch out as the signs are reversed
# need to compare PO with non-PO models (latter allows slope to change for different coeffs)
arth.po <- vglm(Improved ~ Sex + Treatment + Age, data = Arthritis,
                family = cumulative(parallel = TRUE))
arth.po
# parrallel = FALSE is the NPO model
arth.npo <- vglm(Improved ~ Sex + Treatment + Age, data = Arthritis,
                 family = cumulative(parallel = FALSE))
arth.npo

# this coef function is specific to the vglm object
coef(arth.po, matrix = TRUE) # only the intercept changes
coef(arth.npo, matrix = TRUE) # all the coefs change

VGAM::lrtest(arth.npo, arth.po) # equivalent to anova for vglm objects

# or can caluclate it manually
tab <- cbind(
  Deviance = c(deviance(arth.npo), deviance(arth.po)),
  df = c(df.residual(arth.npo), df.residual(arth.po))
)
tab <- rbind(tab, diff(tab))
rownames(tab) <- c("GenLogit", "PropOdds", "LR test")
tab <- cbind(tab, pvalue=1-pchisq(tab[,1], tab[,2]))
tab

# partial proportional odds, where selected coefs may change.
# cumulative(parallel = FALSE ~ sex) - separate slopes fit for males/females
arth.ppo <- vglm(Improved ~ Sex + Treatment + Age, data = Arthritis,
                 family = cumulative(parallel = FALSE ~ Sex))
coef(arth.ppo, matrix = TRUE)

arth.po2 <- lrm(Improved ~ Sex + Treatment + Age, data = Arthritis)
arth.po2

# diagnostic
# solid line is actual mean X | Y
# dashed is E(X) | Y for the OP model.
# if the lines don't hold together, it's reason to believe the variable is not monotonic
op <- par(mfrow=c(1,3))
plot.xmean.ordinaly(Improved ~ Sex + Treatment + Age, data=Arthritis,
                    lwd=2, pch=16, subn=FALSE)
par(op)

# effect plots - append the fitted probabilities to the df
arth.fitp <- cbind(Arthritis,
                   predict(arth.polr, type = "probs"))
head(arth.fitp)

# this is an unpivot operation
plotdat <- melt(arth.fitp,
                id.vars = c("Sex", "Treatment", "Age", "Improved"),
                measure.vars = c("None", "Some", "Marked"),
                variable.name = "Level",
                value.name = "Probability")
## view first few rows
head(plotdat)

gg <- ggplot(plotdat, aes(x = Age, y = Probability, colour = Level)) +
  geom_line(size = 2.5) + theme_bw() + xlim(10, 80) +
  geom_point(color = "black", size = 1.5) +
  facet_grid(Sex ~ Treatment,
             labeller = label_both
  )
direct.label(gg)

# plot effects works on polr object
plot(Effect("Age", arth.polr))
plot(Effect("Age", arth.polr), style = "stacked",
     key.args = list(x = .55, y = .9))
plot(Effect(c("Treatment", "Sex", "Age"), arth.polr),
     style = "stacked", key.arg = list(x = .8, y = .9))
# latent variable model - imagine an invisible linear reaching a tipping point
plot(Effect(c("Treatment", "Age"), arth.polr, latent = TRUE), lwd = 3)

# nested dichotomies method
data("Womenlf", package = "car")
some(Womenlf)
# create dichotomies
Womenlf <- within(Womenlf,{
  working <-  recode(partic, " 'not.work' = 'no'; else = 'yes' ")
  fulltime <- recode(partic,
                     " 'fulltime' = 'yes'; 'parttime' = 'no'; 'not.work' = NA")})
some(Womenlf)

with(Womenlf, table(partic, working))
with(Womenlf, table(partic, fulltime, useNA = "ifany"))
mod.working <- glm(working ~ hincome + children, family = binomial,
                   data = Womenlf)
summary(mod.working)

mod.fulltime <- glm(fulltime ~ hincome + children, family = binomial,
                    data = Womenlf)
summary(mod.fulltime)

cbind(working = coef(mod.working), fulltime = coef(mod.fulltime))
# the stats should be additive
LRtest <- function(model)
  c(LRchisq = model$null.deviance - model$deviance,
    df = model$df.null - model$df.residual)

tab <- rbind(working = LRtest(mod.working),
             fulltime = LRtest(mod.fulltime))
tab <- rbind(tab, All = colSums(tab))
tab <- cbind(tab, pvalue = 1- pchisq(tab[,1], tab[,2]))
tab

Anova(mod.working)
Anova(mod.fulltime)

# to plot fitted values, have to combine the submodels
predictors <- expand.grid(hincome = 1 : 50,
                          children =c('absent', 'present'))
fit <- data.frame(predictors,
                  p.working = predict(mod.working, predictors, type = "response"),
                  p.fulltime = predict(mod.fulltime, predictors, type = "response"),
                  l.working = predict(mod.working, predictors, type = "link"),
                  l.fulltime = predict(mod.fulltime, predictors, type = "link")
)
print(some(fit, 5), digits = 3)

fit <- within(fit, {
  `full-time` <- p.working * p.fulltime
  `part-time` <- p.working * (1 - p.fulltime)
  `not working` <- 1 - p.working
})
# unpivot
fit2 <- melt(fit,
             measure.vars = c("full-time", "part-time", "not working"),
             variable.name = "Participation",
             value.name = "Probability")

gg <- ggplot(fit2,
             aes(x = hincome, y = Probability, colour= Participation)) + 
  facet_grid(~ children, 
             labeller = label_both) + 
  geom_line(size = 2) + theme_bw() +
  scale_x_continuous(limits = c(-3, 55)) +
  scale_y_continuous(limits = c(0, 1))
direct.label(gg, list("top.bumptwice", dl.trans(y = y + 0.2)))

# fitting logits
fit3 <- melt(fit,
             measure.vars = c("l.working", "l.fulltime"),
             variable.name = "Participation",
             value.name = "LogOdds")
levels(fit3$Participation) <- c("working", "full-time")

gg <- ggplot(fit3,
             aes(x = hincome, y = LogOdds, colour = Participation)) + 
  facet_grid(~ children, 
             labeller = label_both) + 
  geom_line(size = 2) + theme_bw() +
  scale_x_continuous(limits = c(-3, 50)) +
  scale_y_continuous(limits = c(-5, 4))
direct.label(gg, list("top.bumptwice", dl.trans(y = y + 0.2)))

levels(Womenlf$partic)
# choose not working as baseline category
Womenlf$partic <- relevel(Womenlf$partic, ref = "not.work")
wlf.multinom <- multinom(partic ~ hincome + children,
                         data = Womenlf, Hess = TRUE)
summary(wlf.multinom, Wald = TRUE)

# confint approximation
stats <- summary(wlf.multinom, Wald = TRUE)
z <- stats$Wald.ratios
p <- 2 * (1 - pnorm(abs(z)))
zapsmall(p)

wlf.multinom2 <- multinom(partic ~ hincome * children,
                          data = Womenlf, Hess = TRUE)
Anova(wlf.multinom2) # interaction term is not signif

# model plots
predictors <- expand.grid(hincome = 1 : 50,
                          children = c("absent", "present"))
fit <- data.frame(predictors,
                  predict(wlf.multinom, predictors, type = "probs")
)

fit2 <- melt(fit,
             measure.vars = c("not.work", "fulltime", "parttime"),
             variable.name = "Participation",
             value.name = "Probability")
levels(fit2$Participation) <- c("not working", "full-time", "part-time")

gg <- ggplot(fit2,
             aes(x = hincome, y = Probability, colour = Participation)) + 
  facet_grid(~ children, 
             labeller = label_both) + 
  geom_line(size = 2) + theme_bw() +
  scale_x_continuous(limits = c(-3, 50)) +
  scale_y_continuous(limits = c(0, 0.9))   
direct.label(gg, list("top.bumptwice", dl.trans(y = y + 0.2)))

# putting the levels into a more natural order from Effect plot
Womenlf$partic <- ordered(Womenlf$partic,
                          levels=c("not.work", "parttime", "fulltime"))
wlf.multinom <- update(wlf.multinom, . ~ .)

plot(Effect(c("hincome", "children"), wlf.multinom),
     style = "stacked", key.args = list(x = .05, y = .9))