library(faraway)
library(lmtest)
library(ggplot2)
data("savings")
lmod <- lm(sr ~ pop15 + pop75 + dpi + ddpi
           , data = savings)
plot(fitted(lmod), residuals(lmod)
     , xlab = "Fitted"
     , ylab = "Residuals")
abline(h=0)
plot(fitted(lmod), sqrt(abs(residuals(lmod)))
     , xlab = "Fitted"
     , ylab = expression(sqrt(hat(epsilon))))
# check for constant variance
sumary(lm(sqrt(abs(residuals(lmod))) ~ fitted(lmod)))

# simulations to see how resid plots might look
par(mfrow=c(2,2))
n <- 50
m <- 4
for(i in 1:m) {
  x <- runif(n)
  plot(x, rnorm(n), pch=".")
}
for(i in 1:m) {
  x <- runif(n)
  plot(x, x*rnorm(n), pch=".")
}
for(i in 1:m) {
  x <- runif(n)
  plot(x, sqrt(x)*rnorm(n), pch=".")
}
for(i in 1:m) {
  x <- runif(n)
  plot(x, cos(x*pi/2.5)+rnorm(n, sd=0.1), pch=".")
}
par(mfrow=c(1,1))

# plot resids against predictors
plot(savings$pop75, residuals(lmod)
     , xlab = "Population over 75"
     , ylab = "Residuals")
abline(h=0)
plot(savings$pop15, residuals(lmod)
     , xlab = "Population under 15"
     , ylab = "Residuals")
abline(h=0)
# is the variance the same
# in the two under 15 groups?

# the var.test is an F test
# DoF from the two groups
var.test(residuals(lmod)[savings$pop15 > 35]
         , residuals(lmod)[savings$pop15 < 35])

data("gala")
lmod <- lm(Species ~ Area + Elevation +
             Scruz + Nearest + Adjacent
           , data = gala)
plot(fitted(lmod), residuals(lmod)
     , xlab = "Fitted"
     , ylab = "Residuals")
abline(h=0)
plot(fitted(lmod), sqrt(abs(residuals(lmod)))
     , xlab = "Fitted"
     , ylab = expression(sqrt(hat(epsilon))))
# check for constant variance
sumary(lm(sqrt(abs(residuals(lmod))) ~ fitted(lmod)))

lmod <- lm(sqrt(Species) ~ Area + Elevation +
             Scruz + Nearest + Adjacent
           , data = gala)
plot(fitted(lmod), residuals(lmod)
     , xlab = "Fitted"
     , ylab = "Residuals")
abline(h=0)
plot(fitted(lmod), sqrt(abs(residuals(lmod)))
     , xlab = "Fitted"
     , ylab = expression(sqrt(hat(epsilon))))
# check for constant variance
sumary(lm(sqrt(abs(residuals(lmod))) ~ fitted(lmod)))

# checking normality
lmod <- lm(sr ~ pop15 + pop75 + dpi + ddpi
           , data = savings)
qqnorm(residuals(lmod)
       , ylab="Residuals"
       , main="")
qqline(residuals(lmod))
# book says hist and box not suitable
hist(residuals(lmod)
       , ylab="Residuals"
       , main="")
# the problem is binning

# test for normality
# null is normal
shapiro.test(residuals(lmod))

# simulations
par(mfrow=c(2,2))
n <- 50
m <- 4
for(i in 1:m) {
  x <- rnorm(n)
  qqnorm(x)
  qqline(x)
}
for(i in 1:m) {
  x <- exp(rnorm(n))
  qqnorm(x)
  qqline(x)
}
for(i in 1:m) {
  x <- rcauchy(n)
  qqnorm(x)
  qqline(x)
}
for(i in 1:m) {
  x <- runif(n)
  qqnorm(x)
  qqline(x)
}

par(mfrow=c(1,1))

# problem with autocorrelation
data("globwarm", package = "faraway")
lmod <- lm(nhtemp ~ wusa + jasper +
             westgreen + chesapeake +
             tornetrask + urals +
             mongolia + tasman
           , data = globwarm)
plot(residuals(lmod) ~ year, na.omit(globwarm)
     , ylab = "residuals")
abline(h=0)

# plot subsequent pairs of resids
# obvious there is a correlation
n <- length(residuals(lmod))
plot(
  tail(residuals(lmod), n-1) ~ head(residuals(lmod), n-1)
  , xlab = expression(hat(epsilon)[i])
  , ylab = expression(hat(epsilon)[i + 1])
  )
abline(h=0, v=0, col=grey(0.75))

sumary(lm(tail(residuals(lmod), n-1) ~
            head(residuals(lmod), n-1), -1))

# lmtest package
dwtest(nhtemp ~ wusa + jasper +
             westgreen + chesapeake +
             tornetrask + urals +
             mongolia + tasman
           , data = globwarm)

lmod <- lm(sr ~ pop15 + pop75 + dpi + ddpi
           , data = savings)
hatv <- hatvalues(lmod)
head(hatv)
sum(hatv) # = number of preds

# half normal plot for diagnosing leverage
countries <- rownames(savings)
halfnorm(hatv, labs=countries
         , ylab = "leverages")
qqnorm(rstandard(lmod))
abline(0,1)

# outliers
set.seed(123)
testdata <- data.frame(x=1:10, y=1:10+rnorm(10))
lmod <- lm(y~x, testdata)

p1 <- c(5.5, 12)
lmod1 <- lm(y~x, rbind(testdata, p1))
plot(y~x, rbind(testdata, p1))
points(p1[1], p1[2], pch=4, cex=2)
abline(lmod)
abline(lmod1, lty=2)

p2 <- c(15, 15.1)
lmod2 <- lm(y~x, rbind(testdata, p2))
plot(y~x, rbind(testdata, p2))
points(p2[1], p2[2], pch=4, cex=2)
abline(lmod)
abline(lmod2, lty=2)

p3 <- c(15, 5.1)
lmod3 <- lm(y~x, rbind(testdata, p3))
plot(y~x, rbind(testdata, p3))
points(p3[1], p3[2], pch=4, cex=2)
abline(lmod)
abline(lmod3, lty=2)

lmod <- lm(sr ~ pop15 + pop75 + dpi + ddpi
           , data = savings)
stud <- rstudent(lmod)
stud[which.max(abs(stud))]

n <- nrow(savings)
p <- length(names(coef(lmod))) + 1
qt(0.05/(n*2), n-p) # bonferroni critical value


data(star, package = "faraway")
plot(star$temp, star$light
     , xlab = "log(Temperature"
     , ylab = "log(Light Intensity")
lmod <- lm(light~temp, star)
abline(lmod)
range(rstudent(lmod))

lmod1 <- lm(light~temp, data=star, subset=temp > 3.6)
abline(lmod1, lty=2)

lmod <- lm(sr ~ pop15 + pop75 + dpi + ddpi
           , data = savings)
cook <- cooks.distance(lmod)
halfnorm(cook, 3
         , labs = countries
         , ylab = "Cook's Distances")

lmod1 <- lm(sr ~ pop15 + pop75 + dpi + ddpi
           , data = savings
           , subset = cook < max(cook))
sumary(lmod)
sumary(lmod1)

plot(dfbeta(lmod)[,2]
     , ylab="Changes in pop15 Coef")
abline(h=0)

lmod1 <- lm(sr ~ pop15 + pop75 + dpi + ddpi
            , data = savings
            , subset = countries != "Japan")


d <- residuals(lm(sr ~ pop75 + dpi + ddpi
            , data = savings))
m <- residuals(lm(pop15 ~ pop75 + dpi + ddpi
                  , data = savings))

plot(d, m
     , xlab = "pop15 resids"
     , ylab = "savings resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["pop15"])
plot(savings$pop15, d)
abline(0, coef(lmod)["pop15"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 1)
lmod1 <- lm(sr ~ pop15 + pop75 + dpi + ddpi
            , data = savings
            , subset = pop15 >35)
lmod2 <- lm(sr ~ pop15 + pop75 + dpi + ddpi
            , data = savings
            , subset = pop15 <35)
sumary(lmod1)
sumary(lmod2)


savings$status <- ifelse(savings$pop15 > 35
                        , "young", "old")
ggplot(savings, aes(x=ddpi, y=sr
                    , shape=status)) + 
  geom_point()

ggplot(savings, aes(x=ddpi, y=sr)) + 
  geom_point() +
  facet_grid(~status) +
  stat_smooth(method="lm")


# exercises
data("sat")
fmla <- as.formula("total ~ expend + salary + ratio + takers")
lmod <- lm(fmla
           , data = sat)
# plot lmod
plot(fitted(lmod), residuals(lmod)
     , xlab = "Fitted"
     , ylab = "Residuals")
abline(h=0)
plot(fitted(lmod), sqrt(abs(residuals(lmod)))
     , xlab = "Fitted"
     , ylab = expression(sqrt(hat(epsilon))))
# check for constant variance
sumary(lm(sqrt(abs(residuals(lmod))) ~ fitted(lmod)))

# check for normality
qqnorm(residuals(lmod)
       , ylab="Residuals"
       , main="")
qqline(residuals(lmod))

qqnorm(rstandard(lmod))
abline(0,1)

# test for normality
# null is normal
shapiro.test(residuals(lmod))

# plot subsequent pairs of resids
# obvious there is a correlation
n <- length(residuals(lmod))
plot(
  tail(residuals(lmod), n-1) ~ head(residuals(lmod), n-1)
  , xlab = expression(hat(epsilon)[i])
  , ylab = expression(hat(epsilon)[i + 1])
)
abline(h=0, v=0, col=grey(0.75))

sumary(lm(tail(residuals(lmod), n-1) ~
            head(residuals(lmod), n-1), -1))

# lmtest package
dwtest(fmla
       , data = sat)

# leverage
hatv <- hatvalues(lmod)
tail(sort(hatv))
2 * sum(hatv) / nrow(sat) # larger than this is high leverage

# half normal plot for diagnosing leverage
states <- rownames(sat)
halfnorm(hatv, labs=states
         , ylab = "leverages")

# outliers
stud <- rstudent(lmod)
stud[which.max(abs(stud))]
n <- nrow(sat)
p <- length(names(coef(lmod))) + 1
qt(0.05/(n*2), n-p) # bonferroni critical value
range(rstudent(lmod))
cook <- cooks.distance(lmod)
halfnorm(cook, 3
         , labs = states
         , ylab = "Cook's Distances")

lmod1 <- lm(fmla
            , data = sat
            , subset = cook < max(cook))
sumary(lmod)
sumary(lmod1)

plot(dfbeta(lmod)[,4]
     , ylab="Changes in Coef")
abline(h=0)

# partial residual plots
d <- residuals(lm(update(fmla, .~.- takers)
                  , data = sat))
m <- residuals(lm(update(fmla, takers~.-takers)
                  , data = sat))

plot(m, d
     , xlab = "takers resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["takers"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 1)

# partial residual plots
d <- residuals(lm(update(fmla, .~.- expend)
                  , data = sat))
m <- residuals(lm(update(fmla, expend~.-expend)
                  , data = sat))

plot(m, d
     , xlab = "expend resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["expend"])

termplot(lmod
         , partial.resid = TRUE
         , terms = 1)

# partial residual plots
d <- residuals(lm(update(fmla, .~.- salary)
                  , data = sat))
m <- residuals(lm(update(fmla, salary~.-salary)
                  , data = sat))

plot(d~m
     , xlab = "salary resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["salary"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 1)

# partial residual plots
d <- residuals(lm(update(fmla, .~.- ratio)
                  , data = sat))
m <- residuals(lm(update(fmla, ratio~.-ratio)
                  , data = sat))

plot(d~m
     , xlab = "ratio resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["ratio"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 1)


# exercises
data("teengamb")
fmla <- as.formula("gamble~sex+status+income+verbal")
lmod <- lm(fmla
           , data = teengamb)
sumary(lmod)

# plot lmod
plot(fitted(lmod), residuals(lmod)
     , xlab = "Fitted"
     , ylab = "Residuals")
abline(h=0)
plot(fitted(lmod), sqrt(abs(residuals(lmod)))
     , xlab = "Fitted"
     , ylab = expression(sqrt(hat(epsilon))))
# check for constant variance
sumary(lm(sqrt(abs(residuals(lmod))) ~ fitted(lmod)))

# check for normality
qqnorm(residuals(lmod)
       , ylab="Residuals"
       , main="")
qqline(residuals(lmod))

qqnorm(rstandard(lmod))
abline(0,1)

# test for normality
# null is normal
shapiro.test(residuals(lmod))

# plot subsequent pairs of resids
# obvious there is a correlation
n <- length(residuals(lmod))
plot(
  tail(residuals(lmod), n-1) ~ head(residuals(lmod), n-1)
  , xlab = expression(hat(epsilon)[i])
  , ylab = expression(hat(epsilon)[i + 1])
)
abline(h=0, v=0, col=grey(0.75))

sumary(lm(tail(residuals(lmod), n-1) ~
            head(residuals(lmod), n-1), -1))

# lmtest package
dwtest(fmla
       , data = teengamb)

# leverage
hatv <- hatvalues(lmod)
tail(sort(hatv))
2 * sum(hatv) / nrow(sat) # larger than this is high leverage

# half normal plot for diagnosing leverage
teens <- rownames(teengamb)
halfnorm(hatv, labs=teens
         , ylab = "leverages")

# outliers
stud <- rstudent(lmod)
stud[which.max(abs(stud))]
n <- nrow(teengamb)
p <- length(names(coef(lmod))) + 1
qt(0.05/(n*2), n-p) # bonferroni critical value
range(rstudent(lmod))
cook <- cooks.distance(lmod)
halfnorm(cook, 2
         , labs = teens
         , ylab = "Cook's Distances")

lmod1 <- lm(fmla
            , data = teengamb
            , subset = cook < max(cook))
sumary(lmod)
sumary(lmod1)

plot(dfbeta(lmod)[,2]
     , ylab="Changes in Coef")
abline(h=0)

# partial residual plots
d <- residuals(lm(update(fmla, .~.-sex)
                  , data = teengamb))
m <- residuals(lm(update(fmla, sex~.-sex)
                  , data = teengamb))

plot(m, d
     , xlab = "takers resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["sex"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 1)

# partial residual plots
d <- residuals(lm(update(fmla, .~.- status)
                  , data = teengamb))
m <- residuals(lm(update(fmla, status~.-status)
                  , data = teengamb))

plot(m, d
     , xlab = "expend resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["status"])

termplot(lmod
         , partial.resid = TRUE
         , terms = 2)

# partial residual plots
d <- residuals(lm(update(fmla, .~.- income)
                  , data = teengamb))
m <- residuals(lm(update(fmla, income~.-income)
                  , data = teengamb))

plot(d~m
     , xlab = "salary resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["income"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 3)

# partial residual plots
d <- residuals(lm(update(fmla, .~.- verbal)
                  , data = teengamb))
m <- residuals(lm(update(fmla, ratio~.-verbal)
                  , data = teengamb))

plot(d~m
     , xlab = "ratio resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["verbal"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 4)

# exercises
data("prostate")
names(prostate)
fmla <- as.formula("lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45")
lmod <- lm(fmla
           , data = prostate)
sumary(lmod)

# plot lmod
plot(fitted(lmod), residuals(lmod)
     , xlab = "Fitted"
     , ylab = "Residuals")
abline(h=0)
plot(fitted(lmod), sqrt(abs(residuals(lmod)))
     , xlab = "Fitted"
     , ylab = expression(sqrt(hat(epsilon))))
# check for constant variance
sumary(lm(sqrt(abs(residuals(lmod))) ~ fitted(lmod)))

# check for normality
qqnorm(residuals(lmod)
       , ylab="Residuals"
       , main="")
qqline(residuals(lmod))

qqnorm(rstandard(lmod))
abline(0,1)

# test for normality
# null is normal
shapiro.test(residuals(lmod))

# plot subsequent pairs of resids
# obvious there is a correlation
n <- length(residuals(lmod))
plot(
  tail(residuals(lmod), n-1) ~ head(residuals(lmod), n-1)
  , xlab = expression(hat(epsilon)[i])
  , ylab = expression(hat(epsilon)[i + 1])
)
abline(h=0, v=0, col=grey(0.75))

sumary(lm(tail(residuals(lmod), n-1) ~
            head(residuals(lmod), n-1), -1))

# lmtest package
dwtest(fmla
       , data = prostate)

# leverage
hatv <- hatvalues(lmod)
tail(sort(hatv))
2 * sum(hatv) / nrow(sat) # larger than this is high leverage

# half normal plot for diagnosing leverage
pros <- rownames(prostate)
halfnorm(hatv, labs=pros
         , ylab = "leverages")

# outliers
stud <- rstudent(lmod)
stud[which.max(abs(stud))]
n <- nrow(prostate)
p <- length(names(coef(lmod))) + 1
qt(0.05/(n*2), n-p) # bonferroni critical value
range(rstudent(lmod))
cook <- cooks.distance(lmod)
halfnorm(cook, 2
         , labs = teens
         , ylab = "Cook's Distances")
plot(lmod, which = 5)

# exercises
data("swiss")
names(swiss)
fmla <- as.formula("Fertility~Agriculture+Examination+Education+Catholic+Infant.Mortality")
lmod <- lm(fmla
           , data = swiss)
sumary(lmod)

# plot lmod
plot(fitted(lmod), residuals(lmod)
     , xlab = "Fitted"
     , ylab = "Residuals")
abline(h=0)
plot(fitted(lmod), sqrt(abs(residuals(lmod)))
     , xlab = "Fitted"
     , ylab = expression(sqrt(hat(epsilon))))
# check for constant variance
sumary(lm(sqrt(abs(residuals(lmod))) ~ fitted(lmod)))

# check for normality
qqnorm(residuals(lmod)
       , ylab="Residuals"
       , main="")
qqline(residuals(lmod))

qqnorm(rstandard(lmod))
abline(0,1)

# test for normality
# null is normal
shapiro.test(residuals(lmod))

# plot subsequent pairs of resids
# obvious there is a correlation
n <- length(residuals(lmod))
plot(
  tail(residuals(lmod), n-1) ~ head(residuals(lmod), n-1)
  , xlab = expression(hat(epsilon)[i])
  , ylab = expression(hat(epsilon)[i + 1])
)
abline(h=0, v=0, col=grey(0.75))

sumary(lm(tail(residuals(lmod), n-1) ~
            head(residuals(lmod), n-1), -1))

# lmtest package
dwtest(total ~ expend + salary + ratio + takers
       , data = sat)

# leverage
hatv <- hatvalues(lmod)
tail(sort(hatv))
2 * sum(hatv) / nrow(sat) # larger than this is high leverage

# half normal plot for diagnosing leverage
sweez <- rownames(swiss)
halfnorm(hatv, labs=sweez
         , ylab = "leverages")

# outliers
stud <- rstudent(lmod)
stud[which.max(abs(stud))]
n <- nrow(swiss)
p <- length(names(coef(lmod))) + 1
qt(0.05/(n*2), n-p) # bonferroni critical value
range(rstudent(lmod))
cook <- cooks.distance(lmod)
halfnorm(cook, 2
         , labs = sweez
         , ylab = "Cook's Distances")

lmod1 <- lm(fmla
            , data = swiss
            , subset = cook < max(cook))
sumary(lmod)
sumary(lmod1)

plot(dfbeta(lmod)[,2]
     , ylab="Changes in Coef")
abline(h=0)

# exercises
data("cheddar")
names(cheddar)
fmla <- as.formula("taste~Acetic+H2S+Lactic")
lmod <- lm(fmla
           , data = cheddar)
sumary(lmod)

# plot lmod
plot(fitted(lmod), residuals(lmod)
     , xlab = "Fitted"
     , ylab = "Residuals")
abline(h=0)
plot(fitted(lmod), sqrt(abs(residuals(lmod)))
     , xlab = "Fitted"
     , ylab = expression(sqrt(hat(epsilon))))
# check for constant variance
sumary(lm(sqrt(abs(residuals(lmod))) ~ fitted(lmod)))

# check for normality
qqnorm(residuals(lmod)
       , ylab="Residuals"
       , main="")
qqline(residuals(lmod))

qqnorm(rstandard(lmod))
abline(0,1)

# test for normality
# null is normal
shapiro.test(residuals(lmod))

# plot subsequent pairs of resids
# obvious there is a correlation
n <- length(residuals(lmod))
plot(
  tail(residuals(lmod), n-1) ~ head(residuals(lmod), n-1)
  , xlab = expression(hat(epsilon)[i])
  , ylab = expression(hat(epsilon)[i + 1])
)
abline(h=0, v=0, col=grey(0.75))

sumary(lm(tail(residuals(lmod), n-1) ~
            head(residuals(lmod), n-1), -1))

# lmtest package
dwtest(fmla
       , data = cheddar)

# leverage
hatv <- hatvalues(lmod)
tail(sort(hatv))
2 * sum(hatv) / nrow(sat) # larger than this is high leverage

# half normal plot for diagnosing leverage
chees <- rownames(cheddar)
halfnorm(hatv, labs=chees
         , ylab = "leverages")

# outliers
stud <- rstudent(lmod)
stud[which.max(abs(stud))]
n <- nrow(cheddar)
p <- length(names(coef(lmod))) + 1
qt(0.05/(n*2), n-p) # bonferroni critical value
range(rstudent(lmod))
cook <- cooks.distance(lmod)
halfnorm(cook, 2
         , labs = teens
         , ylab = "Cook's Distances")
plot(lmod, which = 5)

plot(dfbeta(lmod)[,4]
     , ylab="Changes in Coef")
abline(h=0)

# partial residual plots
d <- residuals(lm(update(fmla, .~.-Acetic)
                  , data = cheddar))
m <- residuals(lm(update(fmla, Acetic~.-Acetic)
                  , data = cheddar))

plot(m, d
     , xlab = "takers resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["Acetic"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 1)

d <- residuals(lm(update(fmla, .~.-H2S)
                  , data = cheddar))
m <- residuals(lm(update(fmla, H2S~.-H2S)
                  , data = cheddar))

plot(m, d
     , xlab = "takers resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["H2S"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 2)

d <- residuals(lm(update(fmla, .~.-Lactic)
                  , data = cheddar))
m <- residuals(lm(update(fmla, Lactic~.-Lactic)
                  , data = cheddar))

plot(m, d
     , xlab = "takers resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["Lactic"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 2)

# exercises
data("happy")
names(happy)
fmla <- as.formula("happy~money+sex+love+work")
lmod <- lm(fmla
           , data = happy)
sumary(lmod)

# plot lmod
plot(fitted(lmod), residuals(lmod)
     , xlab = "Fitted"
     , ylab = "Residuals")
abline(h=0)
plot(fitted(lmod), sqrt(abs(residuals(lmod)))
     , xlab = "Fitted"
     , ylab = expression(sqrt(hat(epsilon))))
# check for constant variance
sumary(lm(sqrt(abs(residuals(lmod))) ~ fitted(lmod)))

# check for normality
qqnorm(residuals(lmod)
       , ylab="Residuals"
       , main="")
qqline(residuals(lmod))

qqnorm(rstandard(lmod))
abline(0,1)

# test for normality
# null is normal
shapiro.test(residuals(lmod))

# plot subsequent pairs of resids
# obvious there is a correlation
n <- length(residuals(lmod))
plot(
  tail(residuals(lmod), n-1) ~ head(residuals(lmod), n-1)
  , xlab = expression(hat(epsilon)[i])
  , ylab = expression(hat(epsilon)[i + 1])
)
abline(h=0, v=0, col=grey(0.75))

sumary(lm(tail(residuals(lmod), n-1) ~
            head(residuals(lmod), n-1), -1))

# lmtest package
dwtest(fmla
       , data = happy)

# leverage
hatv <- hatvalues(lmod)
tail(sort(hatv))
2 * sum(hatv) / nrow(sat) # larger than this is high leverage

# half normal plot for diagnosing leverage
smiles <- rownames(happy)
halfnorm(hatv, labs=smiles
         , ylab = "leverages")

# outliers
stud <- rstudent(lmod)
stud[which.max(abs(stud))]
n <- nrow(happy)
p <- length(names(coef(lmod))) + 1
qt(0.05/(n*2), n-p) # bonferroni critical value

range(rstudent(lmod))
cook <- cooks.distance(lmod)
halfnorm(cook, 2
         , labs = teens
         , ylab = "Cook's Distances")
plot(lmod, which = 5)

lmod1 <- lm(fmla
            , data = happy
            , subset = cook < max(cook))
sumary(lmod)
sumary(lmod1)

plot(dfbeta(lmod)[,2]
     , ylab="Changes in Coef")
abline(h=0)

plot(dfbeta(lmod)[,3]
     , ylab="Changes in Coef")
abline(h=0)

plot(dfbeta(lmod)[,4]
     , ylab="Changes in Coef")
abline(h=0)

# partial residual plots
d <- residuals(lm(update(fmla, .~.-money)
                  , data = happy))
m <- residuals(lm(update(fmla, money~.-money)
                  , data = happy))

plot(m, d
     , xlab = "takers resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["money"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 1)

d <- residuals(lm(update(fmla, .~.-love)
                  , data = happy))
m <- residuals(lm(update(fmla, love~.-love)
                  , data = happy))

plot(m, d
     , xlab = "takers resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["love"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 3)

d <- residuals(lm(update(fmla, .~.-sex)
                  , data = happy))
m <- residuals(lm(update(fmla, sex~.-sex)
                  , data = happy))

plot(m, d
     , xlab = "takers resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["sex"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 2)

d <- residuals(lm(update(fmla, .~.-work)
                  , data = happy))
m <- residuals(lm(update(fmla, work~.-work)
                  , data = happy))

plot(m, d
     , xlab = "takers resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["work"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 4)

# exercises
data("happy")
names(happy)
fmla <- as.formula("happy~money+sex+love+work")
lmod <- lm(fmla
           , data = happy)
sumary(lmod)

# plot lmod
plot(fitted(lmod), residuals(lmod)
     , xlab = "Fitted"
     , ylab = "Residuals")
abline(h=0)
plot(fitted(lmod), sqrt(abs(residuals(lmod)))
     , xlab = "Fitted"
     , ylab = expression(sqrt(hat(epsilon))))
# check for constant variance
sumary(lm(sqrt(abs(residuals(lmod))) ~ fitted(lmod)))

# check for normality
qqnorm(residuals(lmod)
       , ylab="Residuals"
       , main="")
qqline(residuals(lmod))

qqnorm(rstandard(lmod))
abline(0,1)

# test for normality
# null is normal
shapiro.test(residuals(lmod))

# plot subsequent pairs of resids
# obvious there is a correlation
n <- length(residuals(lmod))
plot(
  tail(residuals(lmod), n-1) ~ head(residuals(lmod), n-1)
  , xlab = expression(hat(epsilon)[i])
  , ylab = expression(hat(epsilon)[i + 1])
)
abline(h=0, v=0, col=grey(0.75))

sumary(lm(tail(residuals(lmod), n-1) ~
            head(residuals(lmod), n-1), -1))

# lmtest package
dwtest(fmla
       , data = happy)

# leverage
hatv <- hatvalues(lmod)
tail(sort(hatv))
2 * sum(hatv) / nrow(sat) # larger than this is high leverage

# half normal plot for diagnosing leverage
smiles <- rownames(happy)
halfnorm(hatv, labs=smiles
         , ylab = "leverages")

# outliers
stud <- rstudent(lmod)
stud[which.max(abs(stud))]
n <- nrow(happy)
p <- length(names(coef(lmod))) + 1
qt(0.05/(n*2), n-p) # bonferroni critical value

range(rstudent(lmod))
cook <- cooks.distance(lmod)
halfnorm(cook, 2
         , labs = teens
         , ylab = "Cook's Distances")
plot(lmod, which = 5)

lmod1 <- lm(fmla
            , data = happy
            , subset = cook < max(cook))
sumary(lmod)
sumary(lmod1)

plot(dfbeta(lmod)[,2]
     , ylab="Changes in Coef")
abline(h=0)

plot(dfbeta(lmod)[,3]
     , ylab="Changes in Coef")
abline(h=0)

plot(dfbeta(lmod)[,4]
     , ylab="Changes in Coef")
abline(h=0)

# partial residual plots
d <- residuals(lm(update(fmla, .~.-money)
                  , data = happy))
m <- residuals(lm(update(fmla, money~.-money)
                  , data = happy))

plot(m, d
     , xlab = "takers resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["money"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 1)

d <- residuals(lm(update(fmla, .~.-love)
                  , data = happy))
m <- residuals(lm(update(fmla, love~.-love)
                  , data = happy))

plot(m, d
     , xlab = "takers resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["love"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 3)

d <- residuals(lm(update(fmla, .~.-sex)
                  , data = happy))
m <- residuals(lm(update(fmla, sex~.-sex)
                  , data = happy))

plot(m, d
     , xlab = "takers resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["sex"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 2)

d <- residuals(lm(update(fmla, .~.-work)
                  , data = happy))
m <- residuals(lm(update(fmla, work~.-work)
                  , data = happy))

plot(m, d
     , xlab = "takers resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["work"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 4)

# exercises
data("tvdoctor")
names(tvdoctor)
fmla <- as.formula("life~tv+doctor")
lmod <- lm(fmla
           , data = tvdoctor)
sumary(lmod)

# plot lmod
plot(fitted(lmod), residuals(lmod)
     , xlab = "Fitted"
     , ylab = "Residuals")
abline(h=0)
plot(fitted(lmod), sqrt(abs(residuals(lmod)))
     , xlab = "Fitted"
     , ylab = expression(sqrt(hat(epsilon))))
# check for constant variance
sumary(lm(sqrt(abs(residuals(lmod))) ~ fitted(lmod)))

# check for normality
qqnorm(residuals(lmod)
       , ylab="Residuals"
       , main="")
qqline(residuals(lmod))

qqnorm(rstandard(lmod))
abline(0,1)

# test for normality
# null is normal
shapiro.test(residuals(lmod))

# plot subsequent pairs of resids
# obvious there is a correlation
n <- length(residuals(lmod))
plot(
  tail(residuals(lmod), n-1) ~ head(residuals(lmod), n-1)
  , xlab = expression(hat(epsilon)[i])
  , ylab = expression(hat(epsilon)[i + 1])
)
abline(h=0, v=0, col=grey(0.75))

sumary(lm(tail(residuals(lmod), n-1) ~
            head(residuals(lmod), n-1), -1))

# lmtest package
dwtest(fmla
       , data = tvdoctor)

hatv <- hatvalues(lmod)
tail(sort(hatv))
2 * sum(hatv) / nrow(sat) # larger than this is high leverage

# half normal plot for diagnosing leverage
teevs <- rownames(tvdoctor)
halfnorm(hatv, labs=teevs
         , ylab = "leverages")

# outliers
stud <- rstudent(lmod)
stud[which.max(abs(stud))]
n <- nrow(tvdoctor)
p <- length(names(coef(lmod))) + 1
qt(0.05/(n*2), n-p) # bonferroni critical value

range(rstudent(lmod))
cook <- cooks.distance(lmod)
halfnorm(cook, 2
         , labs = teevs
         , ylab = "Cook's Distances")
plot(lmod, which = 5)

lmod1 <- lm(fmla
            , data = tvdoctor
            , subset = cook < max(cook))
sumary(lmod)
sumary(lmod1)

plot(dfbeta(lmod)[,2]
     , ylab="Changes in Coef")
abline(h=0)

plot(dfbeta(lmod)[,3]
     , ylab="Changes in Coef")
abline(h=0)

# partial residual plots
d <- residuals(lm(update(fmla, .~.-tv)
                  , data = happy))
m <- residuals(lm(update(fmla, tv~.-tv)
                  , data = happy))

plot(m, d
     , xlab = "takers resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["tv"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 1)

d <- residuals(lm(update(fmla, .~.-doctor)
                  , data = tvdoctor))
m <- residuals(lm(update(fmla, doctor~.-doctor)
                  , data = tvdoctor))

plot(m, d
     , xlab = "takers resids"
     , ylab = "total resids")
coef(lm(d~m))
coef(lmod)
abline(0, coef(lmod)["doctor"])
termplot(lmod
         , partial.resid = TRUE
         , terms = 2)


# exercises
data("divusa")
names(divusa)
fmla <- as.formula("divorce~unemployed+femlab+marriage+birth+military")
lmod <- lm(fmla
           , data = divusa)
sumary(lmod)

# plot lmod
plot(fitted(lmod), residuals(lmod)
     , xlab = "Fitted"
     , ylab = "Residuals")
abline(h=0)
plot(fitted(lmod), sqrt(abs(residuals(lmod)))
     , xlab = "Fitted"
     , ylab = expression(sqrt(hat(epsilon))))
# check for constant variance
sumary(lm(sqrt(abs(residuals(lmod))) ~ fitted(lmod)))

# check for normality
qqnorm(residuals(lmod)
       , ylab="Residuals"
       , main="")
qqline(residuals(lmod))

qqnorm(rstandard(lmod))
abline(0,1)

# test for normality
# null is normal
shapiro.test(residuals(lmod))

# plot subsequent pairs of resids
# obvious there is a correlation
n <- length(residuals(lmod))
plot(
  tail(residuals(lmod), n-1) ~ head(residuals(lmod), n-1)
  , xlab = expression(hat(epsilon)[i])
  , ylab = expression(hat(epsilon)[i + 1])
)
abline(h=0, v=0, col=grey(0.75))

sumary(lm(tail(residuals(lmod), n-1) ~
            head(residuals(lmod), n-1), -1))

# lmtest package
dwtest(fmla
       , data = divusa)

