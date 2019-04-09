library(faraway)
library(ggplot2)
library(corrplot)
data("cars")
plot(dist ~ speed, data = cars)
lmod <- lm(dist ~ speed, data = cars)
sumary(lmod)
abline(lmod)

# error in the predictors
lmod1 <- lm(dist ~ I(speed+rnorm(50)), data = cars)
lmod2 <- lm(dist ~ I(speed+2*rnorm(50)), data = cars)
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

# there are two clusters
savings$age <- ifelse(savings$pop15 > 35, 0, 1)
# binary pred has sd of 0.5
# scale other vars accordingly
savings$dpis <- (savings$dpi-mean(savings$dpi)) /
                  (2*sd(savings$dpi))
savings$ddpis <- (savings$ddpi-mean(savings$ddpi)) /
  (2*sd(savings$ddpi))
lmod <- lm(sr ~ age + dpis + ddpis, data = savings)
sumary(lmod)
# each coef is now on a change of 2 standard devs


# colinarity
data("seatpos", package="faraway")
lmod <- lm(hipcenter~.,data = seatpos)
sumary(lmod) # large R2 but no signif preds
round(cor(seatpos[,-9]),2)
corrplot(round(cor(seatpos[,-9]),2))
x <- model.matrix(lmod)[,-1]
egg <- eigen(t(x) %*% x)
sqrt(egg$val[1]/egg$val)

# variance inflation, first var regressed on all others
1/(1-sumary(lm(x[,1] ~ x[,-1]))$r.squared)
# easier with 
vif(x)

# exercises
data("faithful")
lmod <- lm(eruptions~waiting, data=faithful)
sumary(lmod)

# add variances at different levels, rep 1000 each
vv <- rep(1:5/10, each=1000)
slopes <- numeric(5000)
for(i in seq_along(slopes)) {
  slopes[i] <- lm(eruptions ~ I(waiting+sqrt(vv[i])*rnorm(272)), data = faithful)$coef[2]
  
  # for part 2 - doesn't have any effect on the coef
  # slopes[i] <- lm(I(eruptions+sqrt(vv[i])*rnorm(272)) ~ waiting, data = faithful)$coef[2]
}

betas <- (c(coef(lmod)[2]
            , colMeans(matrix(slopes
                              , nrow = 1000))))
variances <- c(0,1:5/10) + 0.5
plot(variances, betas
     , xlim = c(0, 1)
     , ylim=c(0.075,0.076)
)
gv <- lm(betas~variances)
coef(gv)
points(0, gv$coef[1], pch=3)

data("divusa")
lmod <- lm(divorce~unemployed+femlab+marriage+birth+military,data=divusa)
sumary(lmod)
corrplot(cor(divusa[,-1])) # we didn't use year
x <- model.matrix(lmod)[,-1]
egg <- eigen(t(x) %*% x)
egg$val
vif(x)
# remove insignif preds
lmod <- lm(divorce~unemployed+femlab+marriage+birth,data=divusa)
sumary(lmod)
lmod <- lm(divorce~femlab+marriage+birth,data=divusa)
sumary(lmod)
x <- model.matrix(lmod)[,-1]
egg <- eigen(t(x) %*% x)
egg$val
vif(x) # all remaining vifs are reduced a bit


data("longley")
lmod <- lm(Employed~.,data=longley)
sumary(lmod)
corrplot(cor(longley))
x <- model.matrix(lmod)[,-1]
egg <- eigen(t(x) %*% x)
egg$val
vif(x)
sqrt(vif(x))

data("prostate")
lmod <- lm(lpsa~.,data=prostate)
sumary(lmod)
corrplot(cor(prostate))
x <- model.matrix(lmod)[,-1]
egg <- eigen(t(x) %*% x)
egg$val
vif(x)
sqrt(vif(x))
sumary(update(lmod,.~.-lcp-gleason))
sumary(update(lmod,.~.-lcp-gleason-age-pgg45))
sumary(update(lmod,.~.-lcp-gleason-age-pgg45-lbph))

data("cheddar")
lmod <- lm(taste~.,data=cheddar)
sumary(lmod)
summary(lmod)$coef[4,4]

pees <- numeric(1000)
for(i in 1:1000) {
  pees[i] <- summary(lm(taste~Acetic+H2S+I(Lactic+rnorm(30
                                                        , mean=0
                                                        , sd=0.01))
                        , data=cheddar))$coef[4,4]  
}
mean(pees)

for(i in 1:1000) {
  pees[i] <- summary(lm(taste~Acetic+H2S+I(Lactic+rnorm(30
                                    , mean=0
                                    , sd=0.1))
                        , data=cheddar))$coef[4,4]  
}
mean(pees)

lmod <- lm(happy~.,data=happy)
sumary(lmod)
?happy
happy.sc <- happy
happy.sc$happy <- happy.sc$happy/20
happy.sc$money <- happy.sc$money/2000
happy.sc$love <- happy.sc$love/6
happy.sc$work <- happy.sc$work/10
lmod <- lm(happy~.,data=happy.sc)

data("fat")
lmod <- lm(brozek~age + weight + height + neck + 
             chest + abdom + hip + thigh + knee +
             ankle + biceps + forearm + wrist
           , data = fat)
sumary(lmod)
corrplot(cor(fat[, c("brozek", "age", "weight","height"
                     , "neck", "chest", "abdom"
                     , "hip", "thigh", "knee"
                     , "ankle", "biceps"
                     , "forearm", "wrist")]))
x <- model.matrix(lmod)[,-1]
egg <- eigen(t(x) %*% x)
egg$val
vif(x)
sqrt(vif(x))

fat.nong <- fat[-c(39, 42), ]
lmod <- lm(brozek~age + weight + height + neck + 
             chest + abdom + hip + thigh + knee +
             ankle + biceps + forearm + wrist
           , data = fat.nong)
sumary(lmod)
corrplot(cor(fat.nong[, c("brozek", "age", "weight","height"
                     , "neck", "chest", "abdom"
                     , "hip", "thigh", "knee"
                     , "ankle", "biceps"
                     , "forearm", "wrist")]))
x <- model.matrix(lmod)[,-1]
egg <- eigen(t(x) %*% x)
egg$val
vif(x)
sqrt(vif(x))

lmod <- lm(brozek~age + weight + height
           , data = fat)
sumary(lmod)
corrplot(cor(fat[, c("brozek", "age", "weight","height")]))
x <- model.matrix(lmod)[,-1]
egg <- eigen(t(x) %*% x)
egg$val
vif(x)
sqrt(vif(x))

nd <- data.frame(t(apply(fat[, c("age"
                    , "weight"
                    ,"height")]
            , 2
            , median)))
names(nd) <- c("age"
               , "weight"
               ,"height")

predict(lmod
        , newdata = nd
        , interval = "prediction")
predict(lmod
        , newdata = data.frame(age = 40
                               , weight=200
                               , height=73)
        , interval = "prediction")
predict(lmod
        , newdata = data.frame(age = 40
                               , weight=130
                               , height=73)
        , interval = "prediction")
