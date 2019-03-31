library(faraway)
library(ggplot2)
data("cars")
plot(dist ~ speed, data = cars)
lmod <- lm(dist ~ speed, data = cars)
sumary(lmod)
abline(lmod)

# error in the predictors
lmod1 <- lm(dist ~ I(speed+rnorm(50)), data = cars)
coef(lmod1)
lmod2 <- lm(dist ~ I(speed+2*rnorm(50)), data = cars)
coef(lmod1)
lmod5 <- lm(dist ~ I(speed+5*rnorm(50)), data = cars)
coef(lmod1)
coef(lmod2)
coef(lmod5)
abline(lmod1, lty=2)
abline(lmod2, lty=3)
abline(lmod5, lty=4)

# add variances at different levels, rep 1000 each
vv <- rep(1:5/10, each=1000)
slopes <- numeric(5000)
for(i in seq_along(slopes)) {
  slopes[i] <- lm(dist ~ I(speed+sqrt(vv[i])*rnorm(50)), data = cars)$coef[2]
}

betas <- (c(coef(lmod)[2]
        , colMeans(matrix(slopes
                          , nrow = 1000))))
variances <- c(0,1:5/10) + 0.5
plot(variances, betas
     , xlim = c(0,1)
     , ylim=c(3.86, 4)
     )
gv <- lm(betas~variances)
coef(gv)
points(0, gv$coef[1], pch=3)

# scaling
data("savings")
scsav <- data.frame(scale(savings))
lmod <- lm(sr ~., scsav)
sumary(lmod)

edf <- data.frame(coef(lmod), confint(lmod))
names(edf) <- c("est", "lwr", "upr")
p <- ggplot(aes(y=est
                , ymin = lwr
                , ymax = upr
                , x = row.names(edf))
            , data = edf)
p + geom_pointrange() +
  coord_flip() +
  geom_hline(yintercept=0, col=gray(0.75)) +
  xlab("predictor") +
  theme_bw()

