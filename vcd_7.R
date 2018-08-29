library(ggplot2)
theme_set(theme_bw())  # set default ggplot theme
library(MASS)
library(popbio)
library(lmtest)
library(vcd)
library(vcdExtra)
library(effects)
library(gpairs)
library(splines)
library(rms)
library(car)
p <- c(.05, .10, .25, .50, .75, .90, .95)
odds <- p / (1 - p)
data.frame(p,
           odds = as.character(fractions(odds)),
           logit = log(odds))

data("Arthritis", package = "vcd")
Arthritis$Better <- as.numeric(Arthritis$Improved > "None")

with(Arthritis,
     logi.hist.plot(Age, Improved > "None"
                    , type = "hist"
                    , counts = TRUE
                    , ylabel = "Probability (Better)"
                    , xlab = "Age"
                    #, col.cur = "blue"
                    , col.hist = "lightblue"
                    #, col.box = "lightblue"
                    )
)

arth.logistic <- glm(Better ~ Age, data = Arthritis, family = binomial)
coeftest(arth.logistic) # easy way to get the coefs

exp(coef(arth.logistic)) # multiplicative increment of odds for beta increase by 1
exp(10 * coef(arth.logistic)["Age"]) # cumulative 10 year increase

# compare with linear (lm)
arth.lm <- glm(Better ~ Age, data = Arthritis)
coef(arth.lm) # rule of thumb compare this to logistic coef / 4 

# this is the test against the intercept only
anova(arth.logistic, test = "Chisq")
# this is a test against the saturated model. how good is the fit?
LRstats(arth.logistic)

# building up a visualisation
# jitter so points don't over plot.
# wide limits to emphasise where logi diverges from lin
plot(jitter(Better, .1) ~ Age, data = Arthritis,
     xlim = c(15, 85), pch = 16,
     ylab = "Probability (Better)")

xvalues <- seq(15, 85, 5)
pred.logistic <- predict(arth.logistic
                  , newdata=data.frame(Age=xvalues)
                  , type="response", se.fit=TRUE)
# confint
upper <- pred.logistic$fit +
  1.96*pred.logistic$se.fit
lower <- pred.logistic$fit - 
  1.96*pred.logistic$se.fit

# shape of the confint
polygon(c(xvalues, rev(xvalues)),
        c(upper, rev(lower)),
        col=rgb(0,0,1,.2), border=NA)
# fitted line
lines(xvalues, pred.logistic$fit
      , lwd = 4 , col = "blue")

# linear line
abline(arth.lm, lwd=2)

# smooth local regression
lines(lowess(Arthritis$Age
             , Arthritis$Better
             , f = .9)
      , col = "red", lwd = 2)


# basic logistic regression plot
gg <- ggplot(Arthritis, aes(x = Age, y = Better)) +
 xlim(5, 95) +
 geom_point(position = position_jitter(height = 0.02, width = 0)) +
 stat_smooth(method = "glm", method.args = list(family = binomial)
             , alpha = 0.1, fill = "blue"
             , size = 2.5, fullrange = TRUE)

# add linear model and loess smoothers
gg <- gg + stat_smooth(method = "lm", se = FALSE
                       , size = 1.2, color = "black"
                       , fullrange = TRUE)
gg <- gg + stat_smooth(method = "loess", se = FALSE
                       , span = 0.95, colour = "red"
                       , size = 1.2)
gg

data("SpaceShuttle", package = "vcd")
shuttle.mod <- glm(cbind(nFailures, 6 - nFailures) ~ Temperature,
                   data = SpaceShuttle, na.action = na.exclude,
                   family = binomial)

# alternative, be explicit about number of trials
# give fraction and number of trials as weights
SpaceShuttle$trials <- 6
shuttle.modw <- glm(nFailures / trials ~ Temperature, weight = trials,
                    data = SpaceShuttle, na.action = na.exclude,
                    family = binomial)

# these are the same
all.equal(coef(shuttle.mod), coef(shuttle.modw))

# testing, vs. null model
anova(shuttle.mod, test = "Chisq")

ggplot(SpaceShuttle, aes(x = Temperature, y = nFailures / trials)) +
 xlim(30, 81) +
 xlab("Temperature (F)") +
 ylab("O-Ring Failure Probability") +
 geom_point(position=position_jitter(width = 0, height = 0.01)
            , aes(size = 2)) +
 theme(legend.position = "none") +
 geom_smooth(method = "glm", method.args = list(family = binomial)
             , fill = "blue",  aes(weight = trials)
             , fullrange = TRUE, alpha = 0.2, size = 2)

arth.logistic2 <- glm(Better ~ I(Age-50) + Sex + Treatment,
                      data = Arthritis,
                      family = binomial)
coeftest(arth.logistic2) # quick look at the coefs

# this is useful - odds ratio (exp(coefs)) and confint
exp(cbind(OddsRatio = coef(arth.logistic2),
          confint(arth.logistic2)))

# α = −0.578: At age 50, females given the placebo have an odds of improvement of exp(–0.578) = 0.56.
# β1 = 0.0487: Each year of age multiplies the odds of improvement by exp(0.0487) = 1.05, or a 5% increase.
# β2 = −1.49: Males are only exp(–1.49) = 0.26 times as likely to show improvement relative to females. (Or, females are exp(1.49) = 4.437 times more likely than males to improve.)
# β3 = 1.76: People given the active treatment are exp(1.76) = 5.8 times more likely to show improvement compared to those given the placebo.

library(ggplot2)
gg <- ggplot(Arthritis, aes(Age, Better, color = Treatment)) +
  xlim(5, 95) + theme_bw() +
  geom_point(position = position_jitter(height = 0.02, width = 0)) +
  stat_smooth(method = "glm"
              , method.args = list(family = binomial)
              , alpha = 0.2, aes(fill = Treatment)
              , size = 2.5, fullrange = TRUE)
gg   # show the plot

# look at!
gg + facet_wrap(~ Sex)
# this is because of the small sample size within specific conditions
addmargins(xtabs(~Sex + Treatment, data = Arthritis), 2)

# this plots the mulitple predictors on one surface
binreg_plot(arth.logistic2, type = "link")
binreg_plot(arth.logistic2, type = "response")

# could use subset to separate a complex model
# need to set the same limits
binreg_plot(arth.logistic2, type = "link", subset = Sex == "Female",
            main = "Female", xlim=c(25, 75), ylim = c(-3, 3))
binreg_plot(arth.logistic2, type = "link", subset = Sex == "Male",
            main = "Male", xlim=c(25, 75), ylim = c(-3, 3))

binreg_plot(arth.logistic2, subset = Sex == "Female",
            main = "Female", xlim = c(25, 75))
binreg_plot(arth.logistic2, subset = Sex == "Male", 
            main = "Male", xlim = c(25, 75))

# effects plots
arth.eff2 <- allEffects(arth.logistic2, partial.residuals = TRUE)
names(arth.eff2)

# showing how Sex has been varied holding the others, to generate the effect
arth.eff2[["Sex"]]
arth.eff2[["Sex"]]$model.matrix

plot(arth.eff2, rows = 1, cols = 3,
     type="response", residuals.pch = 15)

arth.full <- Effect(c("Age", "Treatment", "Sex"), arth.logistic2)

# logit scale with translated axes
plot(arth.full, multiline = TRUE, ci.style = "bands",
     colors = c("red", "blue"), lwd = 3,
     ticks = list(at = c(.05, .1, .25, .5, .75, .9, .95)),
     key.args = list(x = .52, y = .92, columns = 1), 
     grid = TRUE)

# response scale
plot(arth.full, multiline = TRUE, ci.style = "bands", 
     type="response",
     colors = c("red", "blue"), lwd = 3,
     key.args = list(x = .52, y = .92, columns = 1), 
     grid = TRUE)

set.seed(1235)
data("Donner", package = "vcdExtra") # load the data
some(Donner, 8)
nrow(Donner)
# response is more convenient as a factor
Donner$survived <- factor(Donner$survived, labels = c("no", "yes"))

xtabs(~ family, data = Donner)
table(Donner$family) # same thing, 1 dimensional
# collapse small families into "Other"
fam <- Donner$family
levels(fam)[c(3, 4, 6, 7, 9)] <- "Other"

# reorder, putting Other last
fam = factor(fam,levels(fam)[c(1, 2, 4:6, 3)])
Donner$family <- fam

xtabs(~family, data=Donner)
xtabs(~ survived + family, data = Donner)

plot(survived ~ family
     , data = Donner
     , col = c("pink", "lightblue"))
gpairs(Donner[,c(4, 2, 3, 1)]
       , lower.pars = list(scatter = "points"
                           , conditional = "boxplot"
                           , mosaic = "mosaic")
       , diag.pars = list(fontsize = 20
                        , hist.color = "gray")
       , mosaic.pars = list(gp = shading_Friendly)
       , outer.rot = c(45, 45)
)

# basic plot: survived vs. age, colored by sex, with jittered points
gg <- ggplot(Donner
             , aes(age, as.numeric(survived=="yes")
                   , color = sex)) + 
  ylab("Survived") +
  theme_bw() + 
  geom_point(position = position_jitter(height = 0.02, width = 0)) 

# add conditional linear logistic regressions
gg + stat_smooth(method = "glm"
    , method.args = list(family = binomial)
                 , formula = y ~ x
                 , alpha = 0.2, size = 2
                 , aes(fill = sex))

# add conditional quadratic logistic regressions
gg + stat_smooth(method = "glm"
    , method.args = list(family = binomial)
                 , formula = y ~ poly(x,2)
                 , alpha = 0.2, size = 2
                 , aes(fill = sex))

# add loess smooth
gg + stat_smooth(method = "loess"
                 , method.args = list(span=0.9)
                 , alpha = 0.2
                 , size = 2, aes(fill = sex)) + 
  coord_cartesian(ylim = c(-.05,1.05))

# if the true response were linear, all these plots would be similar

# modeling
# linear logistic/binom
donner.mod1 <- glm(survived ~ age + sex,
                   data = Donner, family  =binomial)
Anova(donner.mod1)

# interaction term included
donner.mod2 <- glm(survived ~ age * sex,
                   data = Donner, family = binomial)
Anova(donner.mod2) # but not signif

# quadratic in age
donner.mod3 <- glm(survived ~ poly(age, 2) + sex,
                   data = Donner, family = binomial)
# quadratic in age and interaction
donner.mod4 <- glm(survived ~ poly(age, 2) * sex,
                   data = Donner, family = binomial)
Anova(donner.mod4)
# goodness of fit test for glms
LRstats(donner.mod1, donner.mod2, donner.mod3, donner.mod4)

mods <- list(donner.mod1, donner.mod2, donner.mod3, donner.mod4)
LR <- sapply(mods, function(x) x$deviance)
LR <- matrix(LR, 2, 2)
rownames(LR) <- c("additive", "non-add")
colnames(LR) <- c("linear", "non-lin")
LR <- cbind(LR, diff = LR[,1] - LR[,2])
LR <- rbind(LR, diff = c(LR[1,1:2] - LR[2,1:2], NA))

# fitting a natural spline
donner.mod5 <- glm(survived ~ ns(age,2) * sex,
                   data = Donner, family = binomial)
Anova(donner.mod5)

donner.mod6 <- glm(survived ~ ns(age,4) * sex,
                   data = Donner, family = binomial)
Anova(donner.mod6)

LRstats(donner.mod4, donner.mod5, donner.mod6)

# The 4 term spline has a lower AIC and higher DF
donner.eff6 <- allEffects(donner.mod6
                          , xlevels = list(age=seq(0, 50, 5)))
plot(donner.eff6
     , ticks = list(at=c(0.001, 0.01, 0.05, 0.1, 0.25, 0.5
                         , 0.75, 0.9, 0.95, 0.99, 0.999)))

set.seed(12345)
library(effects)
data("Arrests", package = "effects")
Arrests[sample(nrow(Arrests), 6),]
nrow(Arrests[Arrests$sex != "Male",])
# a little tidy up
Arrests$year <- as.factor(Arrests$year) # might be nonlin
arrests.mod <- glm(released ~ employed + citizen + checks
                   + colour*year + colour*age,
                   family = binomial, data = Arrests)

Anova(arrests.mod)
# heard to interpret interaction coefs on the logit scale
coeftest(arrests.mod)
# seems to show a clear effect for prob release greater for white
# conints do not overlap
plot(Effect("colour", arrests.mod),
     lwd = 3, ci.style = "bands", main = "",
     xlab = list("Skin color of arrestee", cex = 1.25),
     ylab = list("Probability(released)", cex = 1.25)
)

# colour x age interaction - older blacks treated less harshly
plot(Effect(c("colour", "age"), arrests.mod),
     lwd = 3
     , multiline = TRUE
     , ci.style = "bands"
     , xlab = list("Age", cex = 1.25)
     , ylab = list("Probability(released)", cex = 1.25)
     , key.args = list(x = .1, y = .99, cex = 1.2, columns = 1)
)
# colour x year interaction - police training turn of millenium
plot(Effect(c("colour", "year"), arrests.mod),
     lwd = 3, multiline = TRUE,
     xlab = list("Year", cex = 1.25),
     ylab = list("Probability(released)", cex = 1.25),
     key.args = list(x = .7, y = .99, cex = 1.2, columns = 1)
)

# for all the high order terms
arrests.effects <- allEffects(arrests.mod,
                              xlevels = list(age = seq(15, 45, 5)))
plot(arrests.effects,
     ylab = "Probability(released)"
     , ci.style = "bands", ask = FALSE)

data("ICU", package = "vcdExtra")
names(ICU)
ICU <- ICU[,-c(4, 20)]  # remove redundant race, coma
# for reference fit a full model, all vars
icu.full <- glm(died ~ ., data = ICU, family = binomial)
summary(icu.full)

LRtest <- function(model) {
  LR <- c(LRchisq = (model$null.deviance - model$deviance)
    , df = (model$df.null - model$df.residual))
  LR[["pvalue"]] <- 1 - pchisq(LR[1], LR[2])
  LR
}
(LR <- LRtest(icu.full))

icu.full1 <- update(icu.full, . ~ . - renal - fracture)
anova(icu.full1, icu.full, test = "Chisq")

# to use some tools in the trs package
dd <- datadist(ICU[,-1])
options(datadist = "dd")
icu.lrm1 <- lrm(died~age+sex+service+cancer+renal+
                      infect+cpr+systolic+hrtrate+
                      previcu+admit+fracture+ph+
                      pco+bic+creatin+white+uncons
                #po # something's wrong with this vars
            , data = ICU)
#icu.lrm1 <- lrm(died ~ ., data = ICU)
icu.lrm1 <- update(icu.lrm1, . ~ . - renal - fracture)

sum.lrm1 <- summary(icu.lrm1)
plot(sum.lrm1, log = TRUE
     , main = "Odds ratio for 'died'"
     , cex = 1.25
     , col = rgb(0.1, 0.1, 0.8
                 , alpha = c(0.3, 0.5, 0.8))
     )

icu.step1 <- stepAIC(icu.full1, trace = FALSE)
icu.step1$anova
icu.step2 <- stepAIC(icu.full, trace = FALSE, k = log(200))
icu.step2$anova

coeftest(icu.step1)
coeftest(icu.step2)
anova(icu.step2, icu.step1, test = "Chisq")
# model 2 is significantly better fit but 1 is easier to interpret

# exploring further, allow non-linear and 
# 2 way interactions of binary predictors
icu.glm3 <- update(icu.step2, . ~ . - age + ns(age, 3) + 
                     (cancer + admit + uncons) ^ 2)
anova(icu.step2, icu.glm3, test = "Chisq")

# interactions with age and the binary terms
icu.glm4 <- update(icu.step2, . ~ . + age * (cancer + admit + uncons))
anova(icu.step2, icu.glm4, test = "Chisq")
# these have not added any significant value

# refit with lrm to use nomogram plot
icu.lrm2 <- lrm(died ~ age + cancer  + admit + uncons, data = ICU)
plot(nomogram(icu.lrm2), cex.var = 1.2, lplabel = "Log odds death")

levels(ICU$cancer) <- c("-", "Cancer")
levels(ICU$admit) <- c("-","Emerg")
levels(ICU$uncons) <- c("-","Uncons")

icu.glm2 <- glm(died ~ age + cancer + admit + uncons,
                data = ICU, family = binomial)
binreg_plot(icu.glm2, type = "link", conf_level = 0.68,
            legend = FALSE, 
            labels = TRUE, labels_just = c("right", "bottom"),
            cex = 0, point_size = 0.8, pch = 15:17,
            ylab = "Log odds (died)",
            ylim = c(-7, 4))

options(datadist = NULL)
detach(package:rms)
detach(package:Hmisc)

# diagnostics
infl <- influence.measures(donner.mod3)
names(infl)
summary(infl)

op <- par(mar = c(5, 4, 1, 1) + .1, cex.lab = 1.2)
library(car)
res <- influencePlot(donner.mod3
                     , id.col = "blue"
                     , scale = 8, id.n = 2)
# label the contours of influence
k <- length(coef(donner.mod3))
n <- nrow(Donner)
text(x = c(2, 3) * k / n, y = -1.8, c("2k/n", "3k/n"), cex = 1.2)
# show data together with diagnostics for influential cases
idx <- which(rownames(Donner) %in% rownames(res))
cbind(Donner[idx,2:4], res)

influenceIndexPlot(donner.mod3
                   , vars=c("Cook", "Studentized", "hat"), 
                   id.n=4)


icu.glm2 <- glm(died ~ age + cancer  + admit + uncons,
               data = ICU, family = binomial)
res <- influencePlot(icu.glm2, id.col = "red", 
                     scale = 8, id.cex = 1.5, id.n = 3)
idx <- which(rownames(ICU) %in% rownames(res))
cbind(ICU[idx, c("died", "age", "cancer", "admit", "uncons")], res)

influenceIndexPlot(icu.glm2, vars = c("Cook", "Studentized", "hat"), 
                   id.n = 4)

# custom plotting of DFBETAS
infl <- influence.measures(icu.glm2)
dfbetas <- data.frame(infl$infmat[,2:5])
colnames(dfbetas) <- c("dfb.age", "dfb.cancer", "dfb.admit", 
                       "dfb.uncons")
head(dfbetas)

op <- par(mar = c(5, 5, 1, 1) + .1)
cols <- ifelse(ICU$died == "Yes", "red", "blue")
plot(dfbetas[,1], type = "h", col = cols,
     xlab = "Observation index", 
     ylab = expression(Delta * beta[Age]), 
     cex.lab = 1.3)
points(dfbetas[,1], col = cols)
# label some points
big <- abs(dfbetas[,1]) > .25
idx <- 1 : nrow(dfbetas)
text(idx[big], dfbetas[big, 1], label = rownames(dfbetas)[big],
     cex = 0.9, pos = ifelse(dfbetas[big, 1] > 0, 3, 1), 
     xpd = TRUE)
abline(h = c(-.25, 0, .25), col = "gray")
par(op)

scatterplotMatrix(dfbetas, smooth = FALSE
                  , id.n = 2, ellipse = TRUE
                  , levels = 0.95, robust = FALSE
                  , diagonal = "histogram"
                  , groups = ICU$died
                  , col = c("blue", "red"))

# restating the linear and non-linear with age models
donner.mod1 <- glm(survived ~ age + sex,
                  data = Donner, family = binomial)
donner.mod3 <- glm(survived ~ poly(age, 2) + sex,
                  data = Donner, family = binomial)

crPlots(donner.mod1, ~age, id.n=2)
# this fits a loess to the residuals. 
# There is strong pattern remaining from after the regression
crPlots(donner.mod3, ~poly(age,2), id.n=2)
# note you have to express the whole term to include
# this plot shows that the residuals do not have
# any further non-linear effect
# except perhaps at the extreme right.

col <- ifelse(Donner$survived == "yes", "blue", "red")
pch <- ifelse(Donner$sex == "Male", 16, 17)
avPlots(donner.mod1, id.n = 2,
       col = col, pch = pch, col.lines = "darkgreen")

col <- ifelse(Donner$survived == "yes", "blue", "red")
pch <- ifelse(Donner$sex == "Male", 16, 17)
avPlot(donner.mod1, "age", id.n = 2, pch = pch, col = col, col.lines = "darkgreen", cex.lab = 1.2)
text(30, -0.4, expression(beta[age]*" = -0.034"), pos = 4, cex = 1.25, col = "darkgreen")

avPlot(donner.mod1, "sexMale", id.n = 2, pch = pch, col = col, col.lines = "darkgreen", cex.lab = 1.2)
text(0, 0.1, expression(beta[sexMale]*" = -1.21"), pos = 4, cex = 1.25, col = "darkgreen")

op <- par(mfrow = c(2, 2), mar = c(4, 4, 1, 2.5) + .1, cex.lab = 1.4)
plot(died ~ age, data = ICU, col = c("lightblue", "pink"))
plot(died ~ cancer, data = ICU, col = c("lightblue", "pink"))
plot(died ~ admit, data = ICU, col = c("lightblue", "pink"))
plot(died ~ uncons, data = ICU, col = c("lightblue", "pink"))
par(op)

pch <- ifelse(ICU$died=="No", 1, 2)
avPlots(icu.glm2, id.n=2, pch=pch, cex.lab=1.3)

icu.glm2a <- glm(died ~ age + cancer  + admit + uncons + systolic,
                 data = ICU, family = binomial)
anova(icu.glm2, icu.glm2a, test = "Chisq")
avPlot(icu.glm2a, "systolic", id.n = 3, pch = pch)