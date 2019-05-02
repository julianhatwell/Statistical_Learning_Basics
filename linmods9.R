library(faraway)
library(MASS)
library(splines)
library(nlme)
library(mgcv)
library(lmtest)
data("savings")
lmod <- lm(sr~pop15+pop75+dpi+ddpi, data = savings)
boxcox(lmod, plotit = TRUE, interp = TRUE)
boxcox(lmod, plotit = TRUE, lambda = seq(0.5, 1.5, by=0.1))
data("gala")
lmod <- lm(Species~Area+Elevation+Nearest+Scruz+Adjacent
           , data = gala)
boxcox(lmod
       , plotit = TRUE
       , interp = TRUE
       , lambda = seq(-0.25, 0.75, by=0.05))

data("leafburn")
lmod <- lm(burntime~nitrogen+chlorine+potassium
           , data=leafburn)
logtrans(lmod, plotit = TRUE
         , interp = TRUE
         , alpha = seq(-min(leafburn$burntime) + 0.001
                       , 0
                       , by=0.01))

# piecewise (not so good)
lmod1 <- lm(sr~pop15+pop75+dpi+ddpi
            , data = savings
            , subset = pop15 < 35)
lmod2 <- lm(sr~pop15+pop75+dpi+ddpi
            , data = savings
            , subset = pop15 > 35)
plot(sr~pop15, data = savings
     , xlab = "pop under 15"
     , ylab = "savings rate")
abline(v=35, lty = 5)
segments(20, lmod1$coefficients[1] +
           lmod1$coefficients[2] * 20
         , 35, lmod1$coefficients[1] +
           lmod1$coefficients[2] * 35)
segments(48, lmod2$coefficients[1] +
           lmod2$coefficients[2] * 48
         , 35, lmod2$coefficients[1] +
           lmod2$coefficients[2] * 35)

# basis functions
lhs <- function(x) ifelse(x < 35, 35-x, 0)
rhs <- function(x) ifelse(x < 35, 0, x-35)
lmod <- lm(sr~lhs(pop15)+rhs(pop15), data=savings)
x <- 20:48
py <- lmod$coefficients[1] +
  lmod$coefficients[2] * lhs(x) +
  lmod$coefficients[3] * rhs(x)
lines(x, py, lty = 2)

# adding polynomials
sumary(lm(sr~ddpi, data=savings))
sumary(lm(sr~ddpi + I(ddpi^2), data=savings)) # signif so add cubic
sumary(lm(sr~ddpi + I(ddpi^2) + I(ddpi^3), data=savings))

# better with poly
sumary(lm(sr~poly(ddpi, 4), data=savings))

# multi poly
lmod <- lm(sr~polym(pop15, ddpi, degree = 2), data = savings)

pop15r <- seq(20, 50, length.out = 10)
ddpir <- seq(0, 20, length.out = 10)
pgrid <- expand.grid(pop15=pop15r, ddpi=ddpir)
pv <- predict(lmod, pgrid)
persp(pop15r, ddpir, matrix(pv, 10, 10)
      , theta = 45
      , ylab = "Growth"
      , zlab = "Savings Rate"
      , ticktype = "detailed"
      , shade = 0.25)

funky <- function(x) sin(2 * pi * x^3)^3
x <- seq(0, 1, 0.01)
y <- funky(x) + 0.1 * rnorm(101)
matplot(x, cbind(y, funky(x)), type = c("pl")
        , ylab = "y", pch = 20
        , lty = 1, col = 1)
g4 <- lm(y ~ poly(x, 4))
g12 <- lm(y ~ poly(x, 12))
matplot(x, cbind(y
                 , g4$fitted.values
                 , g12$fitted.values)
        , type = c("pll")
        , ylab = "y", pch = 20
        , lty = 1:2, col = 1)
knots <- c(0,0,0,0
           , 0.2, 0.4, 0.5, 0.6
           , 0.7, 0.8, 0.85, 0.9
           ,1,1,1,1)
bx <- splineDesign(knots, x)
lmodb <- lm(y~bx-1)
matplot(x, bx, type = "l")
matplot(x, cbind(y, funky(x), lmodb$fitted.values)
        , type = c("pll")
        , ylab = "y", pch = 20
        , lty = c(1, 1, 2), col = c(1, 1, 2))

ssf <- smooth.spline(x, y)
matplot(x, cbind(y, funky(x), ssf$y)
        , type = c("pll")
        , ylab = "y", pch = 20
        , lty = c(1, 1, 2), col = c(1, 1, 2))

gamod <- gam(sr~s(pop15)+s(pop75)+s(dpi)+s(ddpi)
            , data = savings)
plot(gamod)          

# exercises
data("aatemp")
plot(temp~year, data=aatemp)
cor(aatemp)
lmod <- lm(temp~year, data=aatemp) 
sumary(lmod)

n <- length(residuals(lmod))
plot(
  tail(residuals(lmod), n-1) ~ head(residuals(lmod), n-1)
  , xlab = expression(hat(epsilon)[i])
  , ylab = expression(hat(epsilon)[i + 1])
)
abline(h=0, v=0, col=grey(0.75))
cor(resid(lmod)[-1], resid(lmod)[-length(resid(lmod))])
plot(resid(lmod)[-1], resid(lmod)[-length(resid(lmod))])
summary(lm(resid(lmod)[-length(resid(lmod))]~resid(lmod)[-1]))
dwtest(lmod) # there is autocor

lmod <- lm(temp~year, data=aatemp) 
glmod <- gls(temp~year, data=aatemp
             , correlation = corAR1(form = ~year))
summary(glmod)

degmod <- lm(temp~year+I(year^2)+I(year^3)+
               I(year^4)+I(year^5)+I(year^6)+
               I(year^7)+I(year^8)+I(year^9)+
               I(year^10)
             , data = aatemp)
sumary(degmod)
degmod <- lm(temp~year+I(year^2)+I(year^3)+
               I(year^4)+I(year^5)+I(year^6)+
               I(year^7)+I(year^8)
             , data = aatemp)
sumary(degmod)
plot(temp~year, data=aatemp)
points(aatemp$year, degmod$fitted.values
      , type="l")
predict(degmod, newdata = data.frame(year = 2020))
bef1930 <- function(x) ifelse(x < 1930, 1930, x)
hockeymod <- lm(temp~bef1930(year), data=aatemp) 
sumary(hockeymod)
points(aatemp$year, hockeymod$fitted.values
       , type="l"
       , col=2
       , lty=2)
knots <- c(rep(1850, 3)
           , seq(1850, 2000, length.out = 6)
           ,rep(2000, 3))
bx <- splineDesign(knots, aatemp$year)
lmodb <- lm(temp~bx-1, data = aatemp)
points(aatemp$year, lmodb$fitted.values
       , type="l"
       , col=3
       , lty=3)

data("cornnit")
names(cornnit)
cornnit$nitrolog <- ifelse(cornnit$nitrogen == 0, 0, log(cornnit$nitrogen))
plot(yield~nitrogen, data = cornnit)
plot(yield~nitrolog, data = cornnit)

lmod1 <- lm(yield~nitrogen, data = cornnit)
lmod2 <- lm(yield~nitrolog, data = cornnit)
lmod3 <- lm(yield~poly(nitrogen, 3), data = cornnit)

sumary(lmod1)
sumary(lmod2)
sumary(lmod3) # also tried with 2. Log seems better by R2 and RSE

bctrans <- function(x, lamb) (x^lamb-1)/lamb

data("ozone")
lmod <- lm(O3~temp+humidity+ibh, data=ozone)
sumary(lmod)
boxcox(lmod, lambda = seq(0, 0.5, by=0.1)
        , plotit = TRUE
        , interp = TRUE
       ) # could use cube root or 4th root
lmod <- lm(bctrans(O3, 0.24)~temp+humidity+ibh, data=ozone)
sumary(lmod) # R2 = 0.71

data("pressure")
plot(pressure~temperature, data = pressure)
plot(log(pressure)~temperature, data = pressure)
boxcox(lmod, lambda = seq(0.05, 0.3, by=0.1)
       , plotit = TRUE
       , interp = TRUE
)

lmod <- lm(bctrans(pressure, 0.15)~temperature
           , data = pressure)
sumary(lmod) # R2 = 1!

data("trees")
names(trees)
plot(Volume~Girth, data = trees)
plot(Volume~Height, data = trees)
lmodg <- lm(Girth~Height, data = trees)
lmodh <- lm(Height~Girth, data = trees)
lmodg2 <- lm(Volume~Height, data = trees)
lmodh2 <- lm(Volume~Girth, data = trees)

plot(lmodg$residuals, lmodg2$residuals)
plot(lmodh$residuals, lmodh2$residuals)
plot(Volume~log(Height), data = trees) # any better

lmod <- lm(Volume~Height+Girth, data=trees)
sumary(lmod)
lmod2 <- lm(Volume~log(Height)+Girth, data=trees)
sumary(lmod2)
lmod3 <- lm(Volume~poly(Height, 2)+Girth, data=trees)
sumary(lmod3)
lmod4 <- lm(Volume~poly(Height, 3)+Girth, data=trees)
sumary(lmod4)
plot(lmod, which = 1)
plot(lmod1, which = 1)
boxcox(lmod, lambda = seq(0, 0.5, by=0.1)
       , plotit = TRUE
       , interp = TRUE
)
lmod5 <- lm(I(Volume^(1/3))~Height+Girth, data = trees)
sumary(lmod5)
plot(lmod5, which = 1)

data("odor")
names(odor)
lmod <- lm(odor ~ polym(temp, gas, pack, degree=2)
           , data = odor)
summary(lmod)
nrow(odor)
lmod <- lm(odor ~ temp + I(temp^2) +
             gas + I(gas^2) +
             pack + I(pack^2)
           , data = odor)
summary(lmod) 
# justified because only square terms were useful in prev
# many more deg free here
# don't need pack ^2

odor[which.min(predict(lmod, odor)), ]
View(odor)
newvals <- data.frame(temp = c(0,0), gas = c(0,0), pack= c(0,1))
predict(lmod, newvals) # Yes, I predicted Karright!!!

data("cheddar")
lmod <- lm(taste ~ Lactic+H2S+Acetic, data = cheddar)
sumary(lmod)
plot(lmod, which = 1)
boxcox(lmod, lambda = seq(0.4, 1.1, by=0.1)
       , plotit = TRUE
       , interp = TRUE
)
lmod <- lm(I(taste^0.65) ~ Lactic+H2S+Acetic, data = cheddar)
sumary(lmod) # no point

data("cars")
lmod <- lm(dist~speed, data=cars)
plot(dist~speed, data=cars)
abline(lmod)
lmod2 <- lm(dist~speed+I(speed^2), data=cars)
x <- 5:25
y <- predict(lmod2, newdata = data.frame(speed = x))
lines(x, y, col=2, lty = 2)
lmod3 <- lm(sqrt(dist) ~ speed, data=cars)
y <- predict(lmod3, newdata = data.frame(speed = x))^2
lines(x, y, col=3, lty = 3)
sp <- smooth.spline(cars$speed, cars$dist)            
lines(sp, col=4, lty=4)
