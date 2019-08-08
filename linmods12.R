library(faraway)
library(ggplot2)
library(corrplot)
library(lattice)
library(lmtest)
library(leaps)
library(nlme)
library(glmnet)

# consider the ecological fallacy. group corralation might not hold for the individual level.
data("eco")
plot(income~usborn, data=eco
     , xlab = "Proportion of US Born"
     , ylab = "Mean Annual Income")
lmod <- lm(income~usborn, data=eco)
abline(lmod)
plot(income~usborn, data=eco
     , xlab = "Proportion of US Born"
     , ylab = "Mean Annual Income"
     , xlim = c(0,1)
     , ylim = c(15000, 70000))
abline(lmod)
sumary(lmod) # inference is wrong because immigrants are naturally attracted to wealther states

data("chredlin")
head(chredlin)
summary(chredlin)
plot(chredlin[, -7])

ggplot(data = chredlin
       , aes(x = race, y = involact)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(data = chredlin
       , aes(x = fire, y = involact)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(data = chredlin
       , aes(x = theft, y = involact)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(data = chredlin
       , aes(x = age, y = involact)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(data = chredlin
       , aes(x = income, y = involact)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(data = chredlin
       , aes(x = side, y = involact)) +
  geom_point(position = position_jitter(width = 0.2
                                        , height = 0)) +
  stat_summary(geom = "errorbar", colour = "red")

# there is a clear correlation - insurance companies tend not to insure high race areas
sumary(lm(involact ~ race, data = chredlin))

# but are there other factors?
ggplot(data = chredlin
       , aes(x = race, y = fire)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(data = chredlin
       , aes(x = race, y = theft)) +
  geom_point() +
  stat_smooth(method = "lm")

# a full model
lmod <- lm(involact ~ fire + race + theft + age + log(income)
           , data = chredlin)
sumary(lmod)
plot(lmod, which = 1) # streak is from zero valued, otherwise OK
plot(lmod, which = 2)

# termplots are useful to seek potential transformations
termplot(lmod, partial.resid = TRUE, terms=1:2)
termplot(lmod, partial.resid = TRUE, terms=3)
termplot(lmod, partial.resid = TRUE, terms=4)

# should we adjust the response - are the conclusions robust?
listcombo <- unlist(
  sapply(0:4
         , function(x) {
           combn(4, x, simplify = FALSE)
           })
  , recursive = FALSE)

predterms <- lapply(listcombo, function(x) {
  paste(c("race", c("fire", "theft", "age", "log(income)")[x])
        , collapse = "+")})
coefm <- matrix(NA, 16, 2)
for (i in 1:16) {
  lmi <- lm(as.formula(paste("involact ~ ", predterms[[i]]))
            , data = chredlin)
  coefm[i, ] <- sumary(lmi)$coef[2, c(1,4)]
}
rownames(coefm) <- predterms
colnames(coefm) <- c("beta", "pvalue")
round(coefm, 4) # we are looking at how stable the race variable was
# significance is not related to choice of other preds

# now look at how stable is was under removal of points
diags <- data.frame(lm.influence(lmod)$coef)
ggplot(data = diags
       , aes(x = row.names(diags)
             , y = race)) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("ZIP") +
  geom_hline(yintercept = 0) +
  geom_linerange(aes(ymax = 0, ymin = race))

# same but for both theft and fire
ggplot(data = diags
       , aes(x = fire
             , y = theft)) +
  geom_text(label = row.names(diags))
ggplot(data = diags
       , aes(x = age
             , y = log.income.)) +
  geom_text(label = row.names(diags))
plot(lmod, 5) # none are extreme outliers

chredlin[c("60607", "60610"), ] # the two zip codes that stand out, but aren't outliers
match(c("60607", "60610"), row.names(chredlin))
lmode <- lm(involact ~ fire + race + theft + age + log(income)
            , data = chredlin[
  -match(c("60607", "60610"), row.names(chredlin))
  , ])
sumary(lmode)

lmod <- lm(involact ~ fire + race + theft + age + log(income)
           , data = chredlin
           , subset = side == "n")
sumary(lmod)
lmod <- lm(involact ~ fire + race + theft + age + log(income)
           , data = chredlin
           , subset = side == "s")
sumary(lmod)

# exercises
data("globwarm")
View(globwarm)

# issues - time series, autocorrelation?
# step one - split the data into years where we have temp
# and years where we don't
gw_known <- globwarm[!is.na(globwarm$nhtemp), ]
gw_unknown <- globwarm[is.na(globwarm$nhtemp), ]
# convenient to have this as a factor
gw_grouper <- factor(is.na(globwarm$nhtemp))

# step two - compare distributions, all, known, unknown
summary(globwarm)
summary(gw_known)
summary(gw_unknown)
# everything looks like it's centred and scaled.
corrplot(cor(gw_known[, -1]))
corrplot(cor(gw_unknown[, -1]))
# strong correlations between wusa, jasper, tornet, urals, mong - and year

ggplot(data = gw_known
       , aes(x = wusa, y = jasper)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(data = gw_known
       , aes(x = tornetrask, y = jasper)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(data = gw_known
       , aes(x = urals, y = jasper)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(data = gw_known
       , aes(x = mongolia, y = jasper)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(data = gw_unknown
       , aes(x = wusa, y = jasper)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(data = gw_unknown
       , aes(x = tornetrask, y = jasper)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(data = gw_unknown
       , aes(x = urals, y = jasper)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(data = gw_unknown
       , aes(x = mongolia, y = jasper)) +
  geom_point() +
  stat_smooth(method = "lm")

# comparing the distributions of the pre- and post-1850's data
densityplot(~wusa, groups = gw_grouper, data = globwarm)
densityplot(~jasper, groups = gw_grouper, data = globwarm)
densityplot(~westgreen, groups = gw_grouper, data = globwarm)
densityplot(~chesapeake, groups = gw_grouper, data = globwarm)
densityplot(~tornetrask, groups = gw_grouper, data = globwarm)
densityplot(~urals, groups = gw_grouper, data = globwarm)
densityplot(~mongolia, groups = gw_grouper, data = globwarm)
densityplot(~tasman, groups = gw_grouper, data = globwarm)

# is there a pattern with year?
plot(nhtemp~year, data=gw_known) # we don't have months, so no seasonal

# build a full model with what we've got
fmla <- as.formula("nhtemp~wusa+jasper+westgreen+chesapeake+tornetrask+urals+mongolia+tasman+year")
fmla2 <- as.formula("nhtemp~wusa+jasper+westgreen+chesapeake+tornetrask+urals+mongolia+tasman")
lmod <- lm(fmla, data=gw_known)
sumary(lmod) # R2 is 0.66
coef(lmod) # mong moves opposite to others that are colinear. need to do something
plot(lmod, which = 1)
plot(lmod, which = 2)
plot(lmod, which = 5) # diag plots look ok, but...
lmod2 <- lm(fmla2, data=gw_known)
sumary(lmod2) # R2 is 0.48, colinear vars have flipped
coef(lmod2) # mong moves opposite to others that are colinear. need to do something
plot(lmod2, which = 1)
plot(lmod2, which = 2)
plot(lmod2, which = 3)
plot(lmod2, which = 5) # diag plots look ok

# should compare this process with and without year.
# year is not causal of temperature, it is just the time series marker
# auto-correlation can't be accounted for when we don't know the true temps going so far back

# true and resids versus predictors
plot(nhtemp~wusa, data=gw_known)
points(nhtemp~wusa, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2)

plot(resid(lmod)~wusa, data=gw_known)
plot(resid(lmod2)~wusa, data=gw_known)
points(resid(lmod2)~wusa, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2)

# partial residual plots
d <- residuals(lm(update(fmla, .~.-wusa)
                  , data = gw_known))
m <- residuals(lm(update(fmla, wusa~.-wusa)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "wusa resids")
abline(0, coef(lmod)["wusa"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 1)

d <- residuals(lm(update(fmla2, .~.-wusa)
                  , data = gw_known))
m <- residuals(lm(update(fmla2, wusa~.-wusa)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "wusa resids")
points(d~m, subset=gw_known$year >=1997, col="red")
abline(0, coef(lmod2)["wusa"])
termplot(lmod2
         , partial.resid = TRUE
         , terms = 1) # slight nonlinearity?

plot(nhtemp~jasper, data=gw_known)
points(nhtemp~jasper, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2)

plot(resid(lmod)~jasper, data=gw_known)
plot(resid(lmod2)~jasper, data=gw_known)
points(resid(lmod2)~jasper, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2)

var.test(resid(lmod2)[gw_known$jasper < 0.25]
         , resid(lmod2)[gw_known$jasper <= 0.25]) # no difference

# partial residual plots
d <- residuals(lm(update(fmla, .~.-jasper)
                  , data = gw_known))
m <- residuals(lm(update(fmla, jasper~.-jasper)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "jasper resids")
abline(0, coef(lmod)["jasper"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 2)

d <- residuals(lm(update(fmla2, .~.-jasper)
                  , data = gw_known))
m <- residuals(lm(update(fmla2, jasper~.-jasper)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "jasper resids")
points(d~m, subset=gw_known$year >=1997, col="red")
abline(0, coef(lmod2)["jasper"])
termplot(lmod2
         , partial.resid = TRUE
         , terms = 2)

plot(nhtemp~westgreen, data=gw_known)
points(nhtemp~westgreen, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2)

plot(resid(lmod)~westgreen, data=gw_known)
plot(resid(lmod2)~westgreen, data=gw_known)
points(resid(lmod2)~westgreen, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2)

# partial residual plots
d <- residuals(lm(update(fmla, .~.-westgreen)
                  , data = gw_known))
m <- residuals(lm(update(fmla, westgreen~.-westgreen)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "westgreen resids")
abline(0, coef(lmod)["westgreen"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 3)

d <- residuals(lm(update(fmla2, .~.-westgreen)
                  , data = gw_known))
m <- residuals(lm(update(fmla2, westgreen~.-westgreen)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "westgreen resids")
points(d~m, subset=gw_known$year >=1997, col="red")
abline(0, coef(lmod2)["westgreen"])
termplot(lmod2
         , partial.resid = TRUE
         , terms = 3)

plot(nhtemp~chesapeake, data=gw_known)
points(nhtemp~chesapeake, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2)

plot(resid(lmod)~chesapeake, data=gw_known)
plot(resid(lmod2)~chesapeake, data=gw_known)
points(resid(lmod2)~chesapeake, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2)
gw_known$year[which(gw_known$chesapeake > 1)] # look out for these years, seem unusual
var.test(resid(lmod2)[gw_known$chesapeake > 1]
         , resid(lmod2)[gw_known$chesapeake <= 1]) # no difference

# partial residual plots
d <- residuals(lm(update(fmla, .~.-chesapeake)
                  , data = gw_known))
m <- residuals(lm(update(fmla, chesapeake~.-chesapeake)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "chesapeake resids")
abline(0, coef(lmod)["chesapeake"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 4)

d <- residuals(lm(update(fmla2, .~.-chesapeake)
                  , data = gw_known))
m <- residuals(lm(update(fmla2, chesapeake~.-chesapeake)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "chesapeake resids")
points(d~m, subset=gw_known$year >=1997, col="red")
abline(0, coef(lmod2)["chesapeake"])
termplot(lmod2
         , partial.resid = TRUE
         , terms = 4)

plot(nhtemp~tornetrask, data=gw_known)
points(nhtemp~tornetrask, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2) # this looks like a decent linear

plot(resid(lmod)~tornetrask, data=gw_known)
plot(resid(lmod2)~tornetrask, data=gw_known)
points(resid(lmod2)~tornetrask, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2)

# partial residual plots
d <- residuals(lm(update(fmla, .~.-tornetrask)
                  , data = gw_known))
m <- residuals(lm(update(fmla, tornetrask~.-tornetrask)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "tornetrask resids")
abline(0, coef(lmod)["tornetrask"])
termplot(lmod
         , partial.resid = TRUE
         , terms =5)

d <- residuals(lm(update(fmla2, .~.-tornetrask)
                  , data = gw_known))
m <- residuals(lm(update(fmla2, tornetrask~.-tornetrask)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "tornetrask resids")
points(d~m, subset=gw_known$year >=1997, col="red")
abline(0, coef(lmod2)["tornetrask"])
termplot(lmod2
         , partial.resid = TRUE
         , terms = 5)

plot(nhtemp~urals, data=gw_known)
points(nhtemp~urals, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2) # something odd about the way urals splits

plot(resid(lmod)~urals, data=gw_known)
plot(resid(lmod2)~urals, data=gw_known)
points(resid(lmod2)~urals, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2) # something odd about the way urals split, but it's not affecting the resids
var.test(resid(lmod2)[gw_known$urals > 0.1]
         , resid(lmod2)[gw_known$ural <= 0.1]) # evidence of a difference
var.test(resid(lmod2)[gw_known$urals > 0.1 & gw_known$year < 1997]
         , resid(lmod2)[gw_known$ural <= 0.1 & gw_known$year < 1997]) # no difference - it's just those elevated later years

# partial residual plots
d <- residuals(lm(update(fmla, .~.-urals)
                  , data = gw_known))
m <- residuals(lm(update(fmla, urals~.-urals)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "urals resids")
abline(0, coef(lmod)["urals"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 6)

d <- residuals(lm(update(fmla2, .~.-urals)
                  , data = gw_known))
m <- residuals(lm(update(fmla2, urals~.-urals)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "urals resids")
points(d~m, subset=gw_known$year >=1997, col="red")
abline(0, coef(lmod2)["urals"])
termplot(lmod2
         , partial.resid = TRUE
         , terms = 6)

plot(nhtemp~mongolia, data=gw_known)
points(nhtemp~mongolia, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2) # something odd about the way this split, but it's not affecting the resids

plot(resid(lmod)~mongolia, data=gw_known)
plot(resid(lmod2)~mongolia, data=gw_known)
points(resid(lmod2)~mongolia, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2) # something odd about the way urals split, but it's not affecting the resids

# partial residual plots
d <- residuals(lm(update(fmla, .~.-mongolia)
                  , data = gw_known))
m <- residuals(lm(update(fmla, mongolia~.-mongolia)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "mongolia resids")
abline(0, coef(lmod)["mongolia"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 7)

d <- residuals(lm(update(fmla2, .~.-mongolia)
                  , data = gw_known))
m <- residuals(lm(update(fmla2, mongolia~.-mongolia)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "mongolia resids")
points(d~m, subset=gw_known$year >=1997, col="red")
abline(0, coef(lmod2)["mongolia"])
termplot(lmod2
         , partial.resid = TRUE
         , terms = 7)

plot(nhtemp~tasman, data=gw_known)
points(nhtemp~tasman, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2) # there's a linear here

plot(resid(lmod)~tasman, data=gw_known)
plot(resid(lmod2)~tasman, data=gw_known)
points(resid(lmod2)~tasman, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2)

# partial residual plots
d <- residuals(lm(update(fmla, .~.-tasman)
                  , data = gw_known))
m <- residuals(lm(update(fmla, tasman~.-tasman)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "tasman resids")
abline(0, coef(lmod)["tasman"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 8)

d <- residuals(lm(update(fmla2, .~.-tasman)
                  , data = gw_known))
m <- residuals(lm(update(fmla2, tasman~.-tasman)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "tasman resids")
points(d~m, subset=gw_known$year >=1997, col="red")
abline(0, coef(lmod2)["tasman"])
termplot(lmod2
         , partial.resid = TRUE
         , terms = 8)

# repeat for good measure
plot(nhtemp~year, data=gw_known)
points(nhtemp~year, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2)

plot(resid(lmod)~year, data=gw_known)
plot(resid(lmod2)~year, data=gw_known)
abline(h=0)
points(resid(lmod2)~year, data=gw_known
       , subset = gw_known$year >= 1997
       , col = 2)

# partial residual plots
d <- residuals(lm(update(fmla, .~.-year)
                  , data = gw_known))
m <- residuals(lm(update(fmla, year~.-year)
                  , data = gw_known))
plot(d~m
     , xlab = "nhtemp resids"
     , ylab = "year resids")
abline(0, coef(lmod)["year"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 9)

n <- nrow(gw_known)
plot(tail(resid(lmod2), n-1)~head(resid(lmod2), n-1))
abline(h=0, v=0, col = grey(0.5), lty = 2)
sumary(lm(tail(resid(lmod2), n-1)~head(resid(lmod2), n-1)))
cor(tail(resid(lmod2), n-1), head(resid(lmod2), n-1))
dwtest(lmod2) # lots of different ways of seeing the autocorrel

plot(tail(resid(lmod2), n-2)~head(resid(lmod2), n-2))
abline(h=0, v=0, col = grey(0.5), lty = 2)
sumary(lm(tail(resid(lmod2), n-2)~head(resid(lmod2), n-2)))
cor(tail(resid(lmod2), n-2), head(resid(lmod2), n-2))

plot(tail(resid(lmod2), n-3)~head(resid(lmod2), n-3))
abline(h=0, v=0, col = grey(0.5), lty = 2)
sumary(lm(tail(resid(lmod2), n-3)~head(resid(lmod2), n-3)))
cor(tail(resid(lmod2), n-3), head(resid(lmod2), n-3))

plot(tail(resid(lmod2), n-4)~head(resid(lmod2), n-4))
abline(h=0, v=0, col = grey(0.5), lty = 2)
sumary(lm(tail(resid(lmod2), n-4)~head(resid(lmod2), n-4)))
cor(tail(resid(lmod2), n-4), head(resid(lmod2), n-4))

plot(tail(resid(lmod2), n-5)~head(resid(lmod2), n-5))
abline(h=0, v=0, col = grey(0.5), lty = 2)
sumary(lm(tail(resid(lmod2), n-5)~head(resid(lmod2), n-5)))
cor(tail(resid(lmod2), n-5), head(resid(lmod2), n-5))

# serious auto-correlation

densityplot(~resid(lmod))
shapiro.test(resid(lmod))
densityplot(~resid(lmod2))
shapiro.test(resid(lmod2)) # it's tempting to leave out those last 4 years
densityplot(~resid(lmod2)[gw_known$year < 1997])
shapiro.test(resid(lmod2)[gw_known$year < 1997]) # it's basically 1998

hatv <- hatvalues(lmod)
hatv2 <- hatvalues(lmod2)
sum(hatv)
sum(hatv2)
halfnorm(hatv, labs = gw_known$year, ylab = "leverages")
halfnorm(hatv2, labs = gw_known$year, ylab = "leverages")
qqnorm(rstandard(lmod));abline(0,1)
qqnorm(rstandard(lmod2));abline(0,1)
which.max(abs(rstudent(lmod)))
max(abs(rstudent(lmod)))
n <- nrow(gw_known)
p <- length(names(coef(lmod))) + 1
qt(0.05/(n*2), n-p) # bonferroni critical value
# 1998 is not a true outlier

cook <- cooks.distance(lmod)
cook2 <- cooks.distance(lmod2)
halfnorm(cook, labs = gw_known$year, ylab = "cooks")
halfnorm(cook2, labs = gw_known$year, ylab = "cooks")
which.max(cook)
which.max(cook2)

plot(dfbeta(lmod)[,2]
     , ylab = names(coef(lmod)[2]))
abline(h=0)
plot(dfbeta(lmod)[,3]
     , ylab = names(coef(lmod)[3]))
abline(h=0)
plot(dfbeta(lmod)[,4]
     , ylab = names(coef(lmod)[4]))
abline(h=0)
plot(dfbeta(lmod)[,5]
     , ylab = names(coef(lmod)[5]))
abline(h=0)
plot(dfbeta(lmod)[,6]
     , ylab = names(coef(lmod)[6]))
abline(h=0)
plot(dfbeta(lmod)[,7]
     , ylab = names(coef(lmod)[7]))
abline(h=0)
plot(dfbeta(lmod)[,8]
     , ylab = names(coef(lmod)[8]))
abline(h=0)
plot(dfbeta(lmod)[,9]
     , ylab = names(coef(lmod)[9]))
abline(h=0)
plot(dfbeta(lmod)[,10]
     , ylab = names(coef(lmod)[10]))
abline(h=0)

# lmod2
plot(dfbeta(lmod2)[,2]
     , ylab = names(coef(lmod2)[2]))
abline(h=0)
plot(dfbeta(lmod2)[,3]
     , ylab = names(coef(lmod2)[3]))
abline(h=0)
plot(dfbeta(lmod2)[,4]
     , ylab = names(coef(lmod2)[4]))
abline(h=0)
plot(dfbeta(lmod2)[,5]
     , ylab = names(coef(lmod2)[5]))
abline(h=0)
plot(dfbeta(lmod2)[,6]
     , ylab = names(coef(lmod2)[6]))
abline(h=0)
plot(dfbeta(lmod2)[,7]
     , ylab = names(coef(lmod2)[7]))
abline(h=0)
plot(dfbeta(lmod2)[,8]
     , ylab = names(coef(lmod2)[8]))
abline(h=0)
plot(dfbeta(lmod2)[,9]
     , ylab = names(coef(lmod2)[9]))
abline(h=0)

# things to try:
# model selection step-wise
# pca
# gls with autocorr
# shrinkage
# broken stick on urals?
# how to deal with year and autocorrelation?
# as a prediction model, should we cross-validate? How to split with autocorrelation?

# all predictors are temperature proxies, they are all auto-correlated
# therefore try building a model that embeds them all, then to selection/shrinkage

lmod_step <- step(lmod)
sumary(lmod_step)
lmod2_step <- step(lmod2)
sumary(lmod2_step) # neither of these has lost any R2

lmod_regsub <- regsubsets(fmla, data = gw_known)
plot(lmod_regsub)
summary(lmod_regsub)
which.min(summary(lmod_regsub)$bic)
coef(lmod_regsub, which.min(summary(lmod_regsub)$bic))
lmod2_regsub <- regsubsets(fmla2, data = gw_known)
plot(lmod2_regsub)
summary(lmod_regsub)
which.min(summary(lmod2_regsub)$bic)
coef(lmod2_regsub, which.min(summary(lmod2_regsub)$bic))

gw_known_prc <- prcomp(gw_known[,-c(1, 10)]) # can't include year
gw_known_prc$rotation
summary(gw_known_prc)
plot(gw_known_prc)
plot(gw_known_prc$x[, 1:2]) # this doesn't solve any of the problems

glmod_corar1 <- gls(fmla2, data = gw_known
                    , correlation = corAR1(form = ~year))
glmod_corcar1 <- gls(fmla2, data = gw_known
                    , correlation = corCAR1(form = ~year))
glmod_corarma <- gls(fmla2, data = gw_known
                     , correlation = corARMA(form = ~ 1 | year, p=1, q=1))
summary(glmod_corar1)
summary(glmod_corcar1)
summary(glmod_corarma)
plot(predict(glmod_corarma, newdata = gw_unknown))

glmod_corarma <- gls(fmla2, data = gw_known[c(folds[[1]], folds[[2]]), ]
                     , correlation = corARMA(form = ~ 1 | year, p=1, q=1))
rmse(gw_known$nhtemp[folds[[3]]]
     , predict(glmod_corarma, gw_known[folds[[3]], ]))

glmod_corarma <- gls(fmla2, data = gw_known[c(folds[[1]], folds[[3]]), ]
                     , correlation = corARMA(form = ~ 1 | year, p=1, q=1))
rmse(gw_known$nhtemp[folds[[2]]]
     , predict(glmod_corarma, gw_known[folds[[2]], ]))

glmod_corarma <- gls(fmla2, data = gw_known[c(folds[[3]], folds[[2]]), ]
                     , correlation = corARMA(form = ~ 1 | year, p=1, q=1))
rmse(gw_known$nhtemp[folds[[1]]]
     , predict(glmod_corarma, gw_known[folds[[1]], ]))
# even with the arma model, the ability to predict a large nearby range is compromised

wusa <- embed(globwarm$wusa, 5)
colnames(wusa) <- paste0("wusa", 1:5)
jasper <- embed(globwarm$jasper, 5)
colnames(jasper) <- paste0("jasper", 1:5)
westgreen <- embed(globwarm$westgreen, 5)
colnames(westgreen) <- paste0("westgreen", 1:5)
chesapeake <- embed(globwarm$chesapeake, 5)
colnames(chesapeake) <- paste0("chesapeake", 1:5)
tornetrask <- embed(globwarm$tornetrask, 5)
colnames(tornetrask) <- paste0("tornetrask", 1:5)
urals <- embed(globwarm$urals, 5)
colnames(urals) <- paste0("urals", 1:5)
mongolia <- embed(globwarm$mongolia, 5)
colnames(mongolia) <- paste0("mongolia", 1:5)
tasman <- embed(globwarm$tasman, 5)
colnames(tasman) <- paste0("tasman", 1:5)

gw_by5 <- data.frame(nhtemp = globwarm$nhtemp[1:997]
                , wusa
                , jasper
                , westgreen
                , chesapeake
                , tornetrask
                , urals
                , mongolia
                , tasman
                , year = globwarm$year[1:997])
gw_by5_known <- gw_by5[!is.na(gw_by5$nhtemp), ]
gw_by5_unknown <- gw_by5[is.na(gw_by5$nhtemp), ]

x <- model.matrix(nhtemp ~ .-1 -year
                  , data=gw_by5_known) 
newx <- model.matrix(nhtemp ~ .-1 -year
                     , data=gw_by5_known)
y <- gw_by5_known$nhtemp
lasso_mod <- glmnet(x,y, alpha = 1)
plot(lasso_mod,xvar="lambda",label=TRUE)
cv.lasso <- cv.glmnet(x,y)
plot(cv.lasso)
lmb <- cv.lasso$lambda[which.min(cv.lasso$cvm)]
lasso_final <- glmnet(x,y, alpha = 1, lambda = lmb)
coef(lasso_final)
plot(predict(lasso_final
             , newx=newx))
# build this model cross validating into those three sections

