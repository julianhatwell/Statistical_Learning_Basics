library(faraway)
library(ggplot2)
data("oatvar")
View(oatvar)
xtabs(yield ~ variety + block, data = oatvar)
plot(yield ~ variety, data = oatvar)
plot(yield ~ block, data = oatvar)
with(oatvar, interaction.plot(variety, block, yield))
with(oatvar, interaction.plot(block, variety, yield))
ggplot(data = oatvar
       , aes(x = variety, y = yield, group = block, linetype = block, colour = block)) +
  geom_line() +
  theme(legend.position = "top", legend.direction = "horizontal")
ggplot(data = oatvar
       , aes(x = block, y = yield, group = variety, linetype = variety, colour = variety)) +
  geom_line() +
  theme(legend.position = "top", legend.direction = "horizontal")

lmod <- lm(yield ~ block + variety, data = oatvar)
anova(lmod) # the denom in the F-test is the full model
anova(lm(yield ~ variety + block, data = oatvar)) # same results because of orthogonal design

anova(lm(yield ~ block, data = oatvar)) # the denom is the block only model
# this is not the right thing to do in a blocking model

# what if a run was lost
anova(lm(yield ~ block + variety, subset = -1, data = oatvar))
anova(lm(yield ~ variety + block, subset = -1, data = oatvar)) # not the same because no longer orthogonal
# prefer the first because the blocking factor can't be changed and is already included in the second test

drop1(lm(yield ~ block + variety, subset = -1, data = oatvar), test = "F")
# diagnostic
plot(lmod, which = 1)
plot(lmod, which = 2)

# Tukeys non-additivity test for interaction
varietycoefs <- c(0, coef(lmod)[6:12])
blockcoefs <- c(0, coef(lmod)[2:5])
ab <- rep(varietycoefs, each = 5) * rep(blockcoefs, 8)
ab
h <- update(lmod, .~. + ab)
anova(h)

# compare to complete randomized
lmcrd <- lm(yield ~ variety, data = oatvar)
summary(lmcrd)
summary(lmcrd)$sigma
summary(lmod)$sigma
(summary(lmcrd)$sigma / summary(lmod)$sigma)^2 # conclusion, CRD requires 66% more obs to achieve same precision

data("abrasion")
xtabs(wear ~ run + position, data = abrasion)
matrix(abrasion$material, 4, 4, byrow = TRUE) # this is the latin square
ggplot(data = abrasion, aes(x = run
                            , y = wear
                            , shape = position
                            , group = material)) +
  geom_point(size = 2) + geom_line(aes(linetype = material, colour = material))
ggplot(data = abrasion, aes(x = position
                            , y = wear
                            , shape = run
                            , group = material)) +
  geom_point(size = 2) + geom_line(aes(linetype = material, colour = material))
# differences at all variables, no obvious outliers or unequal var

lmod <- lm(wear ~ material + run + position, data = abrasion)
drop1(lmod, test="F")
plot(lmod, which = 1)
plot(lmod, which = 2)
summary(lmod)
# tukey pairwise intervals
qtukey(0.95, 4, 6) * 5.53/sqrt(2) # four materials, degfree, st.err, sqrt(2)???
scoefs <- c(0, coef(lmod)[2:4])
# table of the differences
outer(scoefs, scoefs, "-") # D-B and D-C are not significant
# max wear choose B but what if D were much cheaper?

# compare to completely randomized
lmodr <- lm(wear ~ material, data = abrasion)
(summary(lmodr)$sig/summary(lmod)$sig)^2

data("rabbit")
View(rabbit)
xtabs(gain ~ treat + block, rabbit)
ggplot(rabbit, aes(y=gain, x=block, shape=treat, colour=treat)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "top", legend.direction = "horizontal")
ggplot(rabbit, aes(y=gain, x=treat, colour = block)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "top", legend.direction = "horizontal")

lmod <- lm(gain ~ treat + block, rabbit)
drop1(lmod, test="F") # don't use ANOVA for incomplete block designs
plot(lmod, which = 1)
plot(lmod, which = 2)
# must construct tukey's CI manually
summary(lmod)
# the se for pairwise coparison of treatments is 2.24182
qtukey(0.95, 6, 15) * 2.24182/sqrt(2) # 6 treatments, 15 df
# which pairs are bigger than...
tcoefs <- c(0, coef(lmod)[2:6])
abs(outer(tcoefs, tcoefs, "-")) > qtukey(0.95, 6, 15) * 2.24182/sqrt(2)
# only e-f is signif diff.
lmodt <- lm(gain ~ treat, rabbit)
(summary(lmodt)$sig / summary(lmod)$sigma)^2
# the CRD would have needed 3 times as many samples to get the same precision

# exercises
data("alfalfa")
View(alfalfa)
xtabs(yield ~ shade + irrigation, data = alfalfa)
matrix(alfalfa$inoculum, 5, 5, byrow = TRUE) # this is the latin square
ggplot(data = alfalfa, aes(x = shade
                            , y = yield
                            , shape = irrigation
                            , group = inoculum)) +
  geom_point(size = 2) + geom_line(aes(linetype = inoculum, colour = inoculum))
ggplot(data = alfalfa, aes(x = irrigation
                            , y = yield
                            , shape = shade
                            , group = inoculum)) +
  geom_point(size = 2) + geom_line(aes(linetype = inoculum, colour = inoculum))
# differences at all variables, no obvious outliers or unequal var

lmod <- lm(yield ~ inoculum + shade + irrigation, data = alfalfa)
drop1(lmod, test="F")
plot(lmod, which = 1)
plot(lmod, which = 2)
summary(lmod)
# tukey pairwise intervals
qtukey(0.95, 5, 12) * 1.108/sqrt(2) # four materials, degfree, st.err, sqrt(2) is just the  formula
scoefs <- c(0, coef(lmod)[2:5])
# table of the differences
abs(outer(scoefs, scoefs, "-")) > qtukey(0.95, 5, 12) * 1.108/sqrt(2)

# compare to completely randomized
lmodr <- lm(yield ~ inoculum, data = alfalfa)
summary(lmodr)
(summary(lmodr)$sig/summary(lmod)$sig)^2
# interpret as irrigation wasn't a significant effect
# but you have to keep it in because it is a blocking factor
# E is significantly worse than all the others, but the rest are about the same
# blocking was 2.29 times more efficient.

data("eggprod")
View(eggprod)
xtabs(eggs ~ treat + block, data = eggprod)
plot(eggs ~ treat, data = eggprod)
plot(eggs ~ block, data = eggprod)
with(eggprod, interaction.plot(treat, block, eggs))
with(eggprod, interaction.plot(block, treat, eggs))
ggplot(data = eggprod
       , aes(x = block, y = eggs, group = treat, linetype = treat, colour = treat)) +
  geom_line() +
  theme(legend.position = "top", legend.direction = "horizontal")
ggplot(data = eggprod
       , aes(x = treat, y = eggs, group = block, linetype = block, colour = block)) +
  geom_line() +
  theme(legend.position = "top", legend.direction = "horizontal")

lmod <- lm(eggs ~ treat + block, data = eggprod)
anova(lmod) # the denom in the F-test is the full model
anova(lm(eggs ~ block + treat, data = eggprod)) # same results because of orthogonal design

anova(lm(eggs ~ block, data = eggprod)) # the denom is the block only model
# this is not the right thing to do in a blocking model

# diagnostic
plot(lmod, which = 1)
plot(lmod, which = 2)

summary(lmod)
# Tukeys non-additivity test for interaction
treatcoefs <- c(0, coef(lmod)[2:3])
blockcoefs <- c(0, coef(lmod)[4:6])
ab <- rep(treatcoefs, each = 4) * rep(blockcoefs, 3)
ab
h <- update(lmod, .~. + ab)
anova(h) # no evidence of interaction

summary(lmod) # treatO significantly worse than other two
# compare to complete randomized
lmcrd <- lm(eggs ~ treat, data = eggprod)
summary(lmcrd)
summary(lmcrd)$sigma
summary(lmod)$sigma
(summary(lmcrd)$sigma / summary(lmod)$sigma)^2 # conclusion, CRD requires 33% more obs to achieve same precision

data("morley")
View(morley)
xtabs(Speed ~ Run + Expt, data = morley)
with(morley, interaction.plot(Expt, Run, Speed))
with(morley, interaction.plot(Run, Expt, Speed))
lmod <- lm(Speed ~ Run + Expt, data = morley)
anova(lmod) # the denom in the F-test is the full model
summary(lmod)
lmcrd <- lm(Speed ~ Run, data = morley)
summary(lmcrd)
summary(lmcrd)$sigma
summary(lmod)$sigma
(summary(lmcrd)$sigma / summary(lmod)$sigma)^2 # not useful

data("OrchardSprays")
View(OrchardSprays)
xtabs(decrease ~ rowpos + colpos, data = OrchardSprays)
matrix(OrchardSprays$treatment, 8, 8, byrow = TRUE) # this is the latin square
ggplot(data = OrchardSprays, aes(x = factor(rowpos)
                           , y = decrease
                           , shape = factor(colpos)
                           , group = treatment)) +
  geom_point(size = 2) + geom_line(aes(linetype = treatment, colour = treatment))
ggplot(data = OrchardSprays, aes(x = factor(colpos)
                                 , y = decrease
                                 , shape = factor(rowpos)
                                 , group = treatment)) +
  geom_point(size = 2) + geom_line(aes(linetype = treatment, colour = treatment))
ggplot(data = OrchardSprays, aes(x = treatment
                                 , y = decrease)) +
  geom_boxplot()
# differences at all variables, no obvious outliers or unequal var

lmod <- lm(decrease ~ treatment + factor(rowpos) + factor(colpos), data = OrchardSprays)
drop1(lmod, test="F")
plot(lmod, which = 1)
plot(lmod, which = 2)
summary(lmod)
# tukey pairwise intervals
qtukey(0.95, 8, 12) * 9.757/sqrt(2) # four materials, degfree, st.err, sqrt(2) is just the  formula
scoefs <- c(0, coef(lmod)[2:8])
# table of the differences
abs(outer(scoefs, scoefs, "-")) > qtukey(0.95, 5, 12) * 9.757/sqrt(2)

# compare to completely randomized
lmodr <- lm(decrease ~ treatment, data = OrchardSprays)
summary(lmodr)
(summary(lmodr)$sig/summary(lmod)$sig)^2
# interpret as irrigation wasn't a significant effect
# but you have to keep it in because it is a blocking factor
# E is significantly worse than all the others, but the rest are about the same
# blocking was 2.29 times more efficient.
OrchardSprays$lohi <- ifelse(OrchardSprays$treatment %in% LETTERS[1:4], 0, 1)
lmods <- lm(decrease ~ lohi + factor(rowpos) + factor(colpos), data = OrchardSprays)
summary(lmods)
(summary(lmods)$sig/summary(lmod)$sig)^2
lmodl <- lm(decrease ~ as.numeric(treatment) + factor(rowpos) + factor(colpos), data = OrchardSprays)
summary(lmodl)
(summary(lmodl)$sig/summary(lmod)$sig)^2

data("resceram")
View(resceram)
xtabs(noise ~ shape + plate, data = resceram)

ggplot(resceram, aes(y=noise, x=plate, shape=shape, colour=shape)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "top", legend.direction = "horizontal")
ggplot(resceram, aes(y=noise, x=shape, shape=plate, colour=plate)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "top", legend.direction = "horizontal")

lmod <- lm(noise ~ shape + plate, resceram)
drop1(lmod, test="F") # don't use ANOVA for incomplete block designs
plot(lmod, which = 1)
plot(lmod, which = 2)
# must construct tukey's CI manually
summary(lmod)
# the se for pairwise coparison of treatments is 2.24182
qtukey(0.95, 4, 5) * 0.10135/sqrt(2)
# which pairs are bigger than...
tcoefs <- c(0, coef(lmod)[2:4])
abs(outer(tcoefs, tcoefs, "-")) > qtukey(0.95, 4, 5) * 0.10135/sqrt(2)
# only e-f is signif diff.
lmodt <- lm(noise ~ shape, data = resceram)
(summary(lmodt)$sig / summary(lmod)$sigma)^2
# the CRD would have needed 3 times as many samples to get the same precision

data("penicillin")
View(penicillin)
xtabs(yield ~ treat + blend, data = penicillin)

plot(yield ~ treat, data = penicillin)
plot(yield ~ blend, data = penicillin)
with(penicillin, interaction.plot(treat, blend, yield))
with(penicillin, interaction.plot(blend, treat, yield))
ggplot(data = penicillin
       , aes(x = blend, y = yield, group = treat, linetype = treat, colour = treat)) +
  geom_line() +
  theme(legend.position = "top", legend.direction = "horizontal")
ggplot(data = penicillin
       , aes(x = treat, y = yield, group = blend, linetype = blend, colour = blend)) +
  geom_line() +
  theme(legend.position = "top", legend.direction = "horizontal")

lmod <- lm(yield ~ treat + blend, penicillin)
drop1(lmod, test="F") # don't use ANOVA for incomplete block designs
plot(lmod, which = 1) # hetersked (var decreases as fitted increases?)
plot(lmod, which = 2)
# must construct tukey's CI manually
summary(lmod)
qtukey(0.95, 4, 12) * 2.745/sqrt(2)
# which pairs are bigger than...11
tcoefs <- c(0, coef(lmod)[2:4])
abs(outer(tcoefs, tcoefs, "-")) > qtukey(0.95, 4, 12) * 2.745/sqrt(2)
# only e-f is signif diff.
lmodt <- lm(yield ~ treat, data = penicillin)
(summary(lmodt)$sig / summary(lmod)$sigma)^2
