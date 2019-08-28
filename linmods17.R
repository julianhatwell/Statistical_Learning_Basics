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
