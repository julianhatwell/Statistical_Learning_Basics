library(faraway)
library(ggplot2)
library(lattice)
library(car)
data("composite")
composite
ggplot(data = composite
       , aes(y = strength, x = laser
             , group = tape
             , linetype = tape)) +
  geom_line() +
  theme(legend.position = "top"
    , legend.direction = "horizontal")
ggplot(data = composite
       , aes(y = strength, x = tape
             , group = laser
             , linetype = laser)) +
  geom_line() +
  theme(legend.position = "top"
        , legend.direction = "horizontal")

lmod <- lm(strength ~ laser + tape + laser:tape, data = composite)
summary(lmod) # as many cases as parameters

# Tukey's Test for non additivity
# testing if the product of parameters is not equal to zero
lmod <- lm(strength ~ laser + tape, data = composite)
coef(lmod)
lasercoefs <- rep(c(0, coef(lmod)[2:3]), 3) # first is ref
tapecoefs <- rep(c(0, coef(lmod)[4:5]), each = 3)
tmod <- update(lmod, .~. + I(lasercoefs*tapecoefs))
anova(lmod, tmod) # not signif, but multiplicative assumption

# now check the main effect
anova(lmod) 

# these are really ordered factors
composite$tape <- as.ordered(composite$tape)
composite$laser <- as.ordered(composite$laser)
lmod <- lm(strength ~ laser + tape, data = composite)
summary(lmod)
# coding
round(model.matrix(lmod), 2)

# because it's factors, it's hard to extrapolate
# however, these factors suggest numeric
composite$ntape <- rep(c(6.42, 13, 27), each = 3)
composite$nlaser <- rep(c(40, 50, 60), 3)
# poly(log) for evenly spaced levels
lmodn <- lm(strength ~ nlaser + poly(log(ntape), 2), data = composite)
summary(lmodn) # now it's possible to extrapolate

# with replication
data(pvc)
p <- ggplot(data = pvc
            , aes(x = operator, y = psize)) +
  geom_point() +
  stat_summary(fun.y = "mean"
               , geom = "line"
               , aes(group=resin))
p  
opmeans <- with(pvc[pvc$operator==1, ]
                , sapply(split(psize, resin), mean))
opmeans
tdf <- data.frame(x = rep(0.9, 8)
                  , y = opmeans, label = 1:8)
p + geom_text(data=tdf, aes(x=x, y=y, label=label))

ggplot(data = pvc
       , aes(x=resin, y=psize, shape = operator)) +
  geom_point() +
  stat_summary(fun.y = "mean", geom = "line"
               , aes(group=operator
                     , linetype = operator)) +
  theme(legend.position = "top"
        , legend.direction = "horizontal")

lmod <- lm(psize ~ operator * resin, data = pvc)
anova(lmod) # the interaction is not signif
qqnorm(resid(lmod), main = "")
qqline(resid(lmod))
plot(lmod, which = 2)
plot(fitted(lmod), resid(lmod))
plot(lmod, which=1)
plot(resid(lmod)~operator, data = pvc) # looks like uneq variance
# we need one resid from each pair because they are mirror + -
# there are 48 entries
pvce <- pvc[(1:24)*2, ]
pvce$resids <- sqrt(abs(resid(lmod)))[(1:24)*2]
vmod <- lm(resids ~ operator + resin, data = pvce)
anova(vmod) # signif dif var for operator but not resin
# discovering non-contant variance v. important in manufacturing

# check main effects
lmod <- lm(psize ~ operator + resin, data = pvc)
summary(lmod)
# confints
TukeyHSD(aov(psize ~ operator + resin, data = pvc))
# note that taking repeated measurements requires repeated measures methods
# only repeating the entire experiment constitutes independent samples.

data("warpbreaks")
plot(breaks ~ wool, warpbreaks)
plot(breaks ~ tension, warpbreaks)
with(warpbreaks, interaction.plot(wool, tension, breaks))
with(warpbreaks, interaction.plot(tension, wool, breaks))
ggplot(data = warpbreaks
       , aes(x = wool, y = breaks, shape = tension)) +
  geom_point(position = position_jitter(width = 0.1)) +
  stat_summary(fun.y = "mean", geom = "line"
               , aes(group = tension
                     , linetype = tension)) +
  theme(legend.position = "top", legend.direction = "horizontal")
ggplot(data = warpbreaks
       , aes(x = tension, y = breaks, shape = wool)) +
  geom_point(position = position_jitter(width = 0.1)) +
  stat_summary(fun.y = "mean", geom = "line"
               , aes(group = wool
                     , linetype = wool)) +
  theme(legend.position = "top", legend.direction = "horizontal")

# evidence of non-constance variance and an interation
lmod <- lm(sqrt(breaks) ~ wool * tension, data = warpbreaks)
plot(lmod, which = 1)
anova(lmod)
summary(lmod)
# in these cases can be useful to create one combined factor
lmod <- lm(sqrt(breaks) ~ wool:tension-1, data = warpbreaks)
summary(lmod) # smallest coef here is smallest number of breaks
# B High - but is it signif?
TukeyHSD(aov(sqrt(breaks) ~ wool:tension-1, data = warpbreaks))
# in the end not many signif diffs between medium and high
# might depend on relative costs of A and B

data("speedo")
lmod <- lm(y~., data = speedo)
summary(lmod)
model.matrix(lmod) # note that + is coded as zero
# a qqnorm may show signif effects as outliers
qqnorm(coef(lmod)[-1], pch=names(coef(lmod)[-1]))
# because of coding, the e+ is the large negative resid, and is not preferred
# likewise the g+ is preferred
halfnorm(coef(lmod)[-1], labs=names(coef(lmod)[-1]))

# exercises
with(butterfat, interaction.plot(Breed, Age, Butterfat))
with(butterfat, interaction.plot(Age, Breed, Butterfat))
ggplot(data = butterfat
       , aes(x = Breed, y = Butterfat
             , shape = Age
             , colour = Age)) +
  geom_point(position = position_jitter(width = 0.1)) +
  stat_summary(fun.y = "mean", geom = "line"
               , aes(group = Age
                     , linetype = Age)) +
  theme(legend.position = "top", legend.direction = "horizontal")
ggplot(data = butterfat
       , aes(x = Age, y = Butterfat
             , shape = Breed
             , colour = Breed)) +
  geom_point(position = position_jitter(width = 0.1)) +
  stat_summary(fun.y = "mean", geom = "line"
               , aes(group = Breed
                     , linetype = Breed)) +
  theme(legend.position = "top", legend.direction = "horizontal")

lmodi <- lm(Butterfat ~ Breed * Age, data = butterfat)
anova(lmodi)
lmod <- lm(Butterfat ~ Breed + Age, data = butterfat)
anova(lmod)
lmo <- lm(Butterfat ~ Breed, data = butterfat)
anova(lmo)
anova(lmo, lmod, lmodi) # seems like only breed is signif
# but there is evidence of non-constant variance

summary(aov(Butterfat ~ Breed * Age, data = butterfat))
TukeyHSD(aov(Butterfat ~ Breed + Age, data = butterfat))
# Jersey Guensey are not signif diff, we saw this before

plot(lmod, which = 1)
plot(lmod, which = 2) # this isn't so great
plot(resid(lmod)~Breed, data = butterfat) # looks like uneq variance
# we need one resid from each pair because they are mirror + -
# there are 48 entries
butterfat$resids <- sqrt(abs(resid(lmod)))
vmod <- lm(resids ~ Breed + Age, data = butterfat)
anova(vmod) # signif dif var for breed
lmod <- lm(Butterfat ~ Breed, data = butterfat)
summary(lmod)
TukeyHSD(aov(Butterfat ~ Breed, data = butterfat))
# not signif, borderline

data(barley)
plot(yield ~ variety, data = barley)
plot(yield ~ year, data = barley)
plot(yield ~ site, data = barley)
summary(aov(yield ~ variety * site * year, data = barley))
with(barley, interaction.plot(year, site, yield)) # could Morris by Year be reversed?
with(barley, interaction.plot(site, year, yield)) # could Morris by Year be reversed?
with(barley, interaction.plot(year, variety, yield))
with(barley, interaction.plot(variety, year, yield))
with(barley, interaction.plot(site, variety, yield))
with(barley, interaction.plot(variety, site, yield))

lmod <- lm(yield ~ year * site * variety, data = barley)
summary(lmod)
qqnorm(coef(lmod)[-1], pch=".")
qqline(coef(lmod)[-1])
n <- length(coef(lmod)[-1])
qq <- qnorm(seq(1, 118, length.out = 119)/119)
text(labels = names(sort(coef(lmod)[-1]))
     , qq, sort(coef(lmod)[-1]))
halfnorm(coef(lmod)[-1], labs=names(coef(lmod)[-1])
         , xlim = c(0.0, 4.0))
lmod <- lm(yield ~ year * site * variety, data = barley)
summary(lmod)

ggplot(data = barley
       , aes(x = site, y = yield
             , shape = year
             , colour = year)) +
  geom_point(position = position_jitter(width = 0.1)) +
  stat_summary(fun.y = "mean", geom = "line"
               , aes(group = year
                     , linetype = year)) +
  theme(legend.position = "top", legend.direction = "horizontal")

ggplot(data = barley
       , aes(x = variety, y = yield
             , shape = year
             , colour = year)) +
  geom_point(position = position_jitter(width = 0.1)) +
  stat_summary(fun.y = "mean", geom = "line"
               , aes(group = year
                     , linetype = year)) +
  theme(legend.position = "top", legend.direction = "horizontal")


m31 <- with(barley, barley[year == "1931" & site == "Morris", "yield"])
m32 <- with(barley, barley[year == "1932" & site == "Morris", "yield"])
barley[barley$year == "1931" & barley$site == "Morris", "yield"] <- m32
barley[barley$year == "1932" & barley$site == "Morris", "yield"] <- m31

ggplot(data = barley
       , aes(x = site, y = yield
             , shape = year
             , colour = year)) +
  geom_point(position = position_jitter(width = 0.1)) +
  stat_summary(fun.y = "mean", geom = "line"
               , aes(group = year
                     , linetype = year)) +
  theme(legend.position = "top", legend.direction = "horizontal")
ggplot(data = barley
       , aes(x = variety, y = yield
             , shape = year
             , colour = year)) +
  geom_point(position = position_jitter(width = 0.1)) +
  stat_summary(fun.y = "mean", geom = "line"
               , aes(group = year
                     , linetype = year)) +
  theme(legend.position = "top", legend.direction = "horizontal")


plot(yield ~ variety, data = barley)
plot(yield ~ year, data = barley)
plot(yield ~ site, data = barley)
summary(aov(yield ~ variety + site + year, data = barley))
with(barley, interaction.plot(year, site, yield))
with(barley, interaction.plot(site, year, yield))
with(barley, interaction.plot(year, variety, yield))
with(barley, interaction.plot(variety, year, yield))
with(barley, interaction.plot(site, variety, yield))
with(barley, interaction.plot(variety, site, yield))

lmod <- lm(yield ~ year + site * variety, data = barley)
summary(lmod)
qqnorm(coef(lmod)[-1], pch=names(coef(lmod)[-1]))
halfnorm(coef(lmod)[-1], labs=names(coef(lmod)[-1]))
anova(lmod)
lmod <- lm(yield ~ year + site + variety, data = barley)
anova(lmod) # only main effects in the end

data("sono")
names(sono)
plot(Intensity ~ Molarity, data = sono)
plot(Intensity ~ Solute, data = sono)
plot(Intensity ~ pH, data = sono)
plot(Intensity ~ Gas, data = sono)
plot(Intensity ~ Water, data = sono)
plot(Intensity ~ Horn, data = sono)
plot(Intensity ~ Flask, data = sono)
plot(log(Intensity) ~ Molarity, data = sono)
plot(log(Intensity) ~ Solute, data = sono)
plot(log(Intensity) ~ pH, data = sono)
plot(log(Intensity) ~ Flask, data = sono)
with(sono, interaction.plot(Water, Gas, log(Intensity))) 
with(sono, interaction.plot(Gas, Water, log(Intensity)))
with(sono, interaction.plot(Horn, Flask, log(Intensity))) 
with(sono, interaction.plot(Flask, Horn, log(Intensity))) 

lmod <- lm(log(Intensity)~., data = sono)
summary(lmod)
model.matrix(lmod) # all ups are 1, downs are 0
# a qqnorm may show signif effects as outliers
qqnorm(coef(lmod)[-1], pch=names(coef(lmod)[-1]))
# looks like S, F, p, M
# so not the following three
plot(log(Intensity) ~ Gas, data = sono)
plot(log(Intensity) ~ Water, data = sono)
plot(log(Intensity) ~ Horn, data = sono)

halfnorm(coef(lmod)[-1], nlab = 4, labs=names(coef(lmod)[-1]))
# so these
with(sono, interaction.plot(Molarity, Solute, log(Intensity))) 
with(sono, interaction.plot(Solute, Molarity, log(Intensity)))
with(sono, interaction.plot(pH, Molarity, log(Intensity))) 
with(sono, interaction.plot(Molarity, pH, log(Intensity))) 
with(sono, interaction.plot(Flask, Solute, log(Intensity))) 
with(sono, interaction.plot(Solute, Flask, log(Intensity)))
with(sono, interaction.plot(Flask, Molarity, log(Intensity))) 
with(sono, interaction.plot(Molarity, Flask, log(Intensity))) 
with(sono, interaction.plot(pH, Solute, log(Intensity))) 
with(sono, interaction.plot(Solute, pH, log(Intensity)))
with(sono, interaction.plot(pH, Flask, log(Intensity))) 
with(sono, interaction.plot(Flask, pH, log(Intensity))) 

lmod <- lm(Intensity ~ Flask * Molarity * Solute * pH, data = sono)
anova(lmod) # all these are signif, including interactions
lmodg <- update(lmod, .~. + Gas, data = sono)
anova(lmod, lmodg) # additional term not signif
lmodw <- update(lmod, .~. + Water, data = sono)
anova(lmod, lmodw) # additional term not signif
lmodh <- update(lmod, .~. + Horn, data = sono)
anova(lmod, lmodh) # additional term not signif

data(rats)
plot(time ~ poison, data = rats)
plot(time ~ treat, data = rats)

lmod <- lm(time ~ poison + treat, data = rats)
plot(lmod, which = 1) # hetskad
(bc <- boxcox(lmod))
bc$x[which.max(bc$y)]
lmod <- lm(I(time^-0.75) ~ poison + treat, data = rats)
plot(lmod, which = 1)
anova(lmod)
lmodi <- lm(I(time^-0.75) ~ poison * treat, data = rats)
anova(lmod, lmodi) # no point adding interact
summary(lmodi) # no point adding interact
anova(lmod)
summary(aov(lmod))
TukeyHSD(aov(lmod)) # only D-B is not signif
plot(I(time^-0.75) ~ treat, data = rats)

data("peanut")
nrow(peanut) # enough for all 2-way
lmod <- lm(solubility ~., data = peanut)
summary(lmod)
model.matrix(lmod)
lmod <- lm(solubility ~ (press + temp + moist + flow + size)^2, data = peanut)
summary(lmod)
qqnorm(coef(lmod)[-1], pch = ".")
qqline(coef(lmod)[-1])
qq <- qnorm(seq(1, length(coef(lmod)[-1]) - 1, length.out = length(coef(lmod)[-1]))/length(coef(lmod)[-1]))
text(labels = names(sort(coef(lmod)[-1]))
     , qq, sort(coef(lmod)[-1])
     , cex = 1)

data("hsb")
nrow(hsb)
lmodi <- lm(math ~ (gender + race + schtyp + prog + ses)^2, data = hsb)
summary(lmodi) # 41 params including intercept
model.matrix(lmodi)[, -1]
ncol(model.matrix(lmodi)[, -1])
# 2 * gender, 2 * schtyp, 4 * race, 3 * prog, 3 * ses
anova(lmodi)
# gender, schtyp and ses can go and there are no two ways.
drop1(lmodi, ~ (gender + race + schtyp + prog + ses)^2, test = "F")
lmod <- lm(math ~ gender + race + schtyp + prog + ses, data = hsb)
summary(lmod) # 41 params including intercept
anova(lmod, lmodi)
TukeyHSD(aov(lmod)) # whites-asians and hisp-afr are not signif diff, voc-gen not signif

qqnorm(resid(lmod)) # looks OK
qqline(resid(lmod))
plot(jitter(fitted(lmod)), resid(lmod))

lmods <- lm(math ~ race + prog, data = hsb)
summary(lmods)
qqnorm(resid(lmods)) # looks OK
qqline(resid(lmods))
plot(jitter(fitted(lmod)), resid(lmod)) # possible hetsked?
