library(vcd)
library(vcdExtra)
library(effects)
library(ggplot2)
library(gpairs)
library(countreg)
library(GGally)
library(MASS)
library(sandwich)
library(AER)
library(VGAM)
library(lmtest)
library(pscl)
library(mgcv)
library(rsm)
library(colorspace)
library(heplots)
theme_set(theme_bw())  # set default ggplot theme

data("PhdPubs", package = "vcdExtra")
with(PhdPubs, c(mean = mean(articles), var = var(articles),
                ratio = var(articles) / mean(articles)))
art.fac <- factor(PhdPubs$articles, levels = 0 : 19)  # include zero frequencies
art.tab <- table(art.fac)
art.tab

barplot(art.tab, xlab = "Number of articles", ylab = "Frequency",
        col = "lightblue")
abline(v = mean(PhdPubs$articles), col = "red", lwd = 3)
ci <- mean(PhdPubs$articles) + c(-1, 1) * sd(PhdPubs$articles)
lines(x = ci, y = c(-4, -4), col = "red", lwd = 3, xpd = TRUE)

barplot(art.tab + 1, ylab = "log(Frequency+1)",
        xlab = "Number of articles", col = "lightblue", log = "y")
abline(v = mean(PhdPubs$articles), col = "red", lwd = 3)
lines(x = ci, y = c(0.8, 0.8), col = "red", lwd = 3, xpd = TRUE)

boxplot(articles + 1 ~ married, data = PhdPubs
        , log = "y", varwidth = TRUE
        , ylab = "log(articles + 1)"
        , xlab = "married", cex.lab = 1.25)
plot(jitter(articles + 1) ~ mentor, data = PhdPubs
     , log = "y", ylab="log(articles + 1)", cex.lab = 1.25)
lines(lowess(PhdPubs$mentor, PhdPubs$articles + 1), col = "blue", 
      lwd = 3)

ggplot(PhdPubs, aes(mentor, articles + 1)) +
  geom_jitter(position = position_jitter(h = 0.05)) +
  stat_smooth(method = "lm", color = "red", fill = "violetred", size = 1.25, se = TRUE) +
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.1) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "log(articles + 1)", x = "Mentor publications")

PhdPubs <- within(PhdPubs, {
  female <- factor(female)	
  married <- factor(married)
})
phd.pois <- glm(articles ~ ., data = PhdPubs
                , family = poisson)
summary(phd.pois)
round(cbind(beta = coef(phd.pois),
            expbeta = exp(coef(phd.pois)),
            pct = 100 * (exp(coef(phd.pois)) - 1)), 3)
# that's a nice way to show the increase or decrease

plot(allEffects(phd.pois), band.colors = "blue", lwd = 3,
     ylab = "Number of articles", main = "")

plot(allEffects(phd.pois), band.colors = "blue"
     , ylim = c(0, log(10)))

data("CrabSatellites", package = "countreg")
gpairs(CrabSatellites[, 5 : 1],
       diag.pars = list(fontsize = 16))
ggpairs(CrabSatellites[, 5 : 1], upper = list(continuous = "points", combo = "box"), 
        lower = list(continuous = "points", combo = "box"))
str(CrabSatellites)

op <- par(mar=c(4,4,1,1)+.1)
plot(jitter(satellites) ~ width, data = CrabSatellites
     , ylab = "Number of satellites (jittered)"
     , xlab = "Carapace width"
     , cex.lab = 1.25)
with(CrabSatellites, lines(lowess(width, satellites)
                           , col = "red", lwd = 2))

plot(jitter(satellites) ~ weight, data = CrabSatellites
     , ylab = "Number of satellites (jittered)"
     , xlab = "Weight"
     , cex.lab = 1.25)
with(CrabSatellites, lines(lowess(weight, satellites)
                           , col = "red", lwd = 2))
par(op)

op <- par(mar=c(4,4,1,1)+.1)
plot(satellites ~ cutfac(width), data = CrabSatellites
     , ylab = "Number of satellites"
     , xlab = "Carapace width (deciles)")
plot(satellites ~ cutfac(weight), data = CrabSatellites
     , ylab = "Number of satellites"
     , xlab = "Weight (deciles)")
par(op)

crabs.pois <- glm(satellites ~ ., data = CrabSatellites, 
                  family = poisson)
summary(crabs.pois)
plot(allEffects(crabs.pois), main = "")

CrabSatellites1 <- transform(CrabSatellites
                             , color = as.numeric(color))
crabs.pois1 <- glm(satellites ~ weight + color
                   , data = CrabSatellites1
                   , family = poisson)
summary(crabs.pois1)
LRstats(crabs.pois, crabs.pois1)
with(phd.pois, deviance / df.residual)
sum(residuals(phd.pois
              , type = "pearson")^2) / phd.pois$df.residual
# this is to estimate the over-dispersion factor

phd.qpois <- glm(articles ~ .
                 , data = PhdPubs
                 , family = quasipoisson)
(phi <- summary(phd.qpois)$dispersion)
# or it can be done through fitting the quasi poisson model

crabs.nbin <- glm.nb(satellites ~ weight + color, 
                     data = CrabSatellites1)
crabs.nbin$theta # to estimate theta dispersion parameter from the data
# it's so close to 1, it could be fixed
crabs.nbin1 <- glm(satellites ~ weight + color
                   , data = CrabSatellites1,
                   family = negative.binomial(1))

# fittin mean/variance curves
phd.nbin  <- glm.nb(articles ~ ., data = PhdPubs)
fit.pois <- fitted(phd.pois, type = "response")
fit.nbin <- fitted(phd.nbin, type = "response")

## cutq
cutq <- function(x, q = 10) {
  quantile <- cut(x, breaks = quantile(x, probs = (0 : q) / q),
                  include.lowest = TRUE, labels = 1 : q)
  quantile
}
group <- cutq(fit.nbin, q = 20)
qdat <- aggregate(PhdPubs$articles,
                  list(group),
                  FUN = function(x) c(mean = mean(x), var = var(x)))
qdat <- data.frame(qdat$x)
qdat <- qdat[order(qdat$mean),]

phi <- summary(phd.qpois)$dispersion
qdat$qvar <- phi * qdat$mean
qdat$nbvar <- qdat$mean + (qdat$mean^2) / phd.nbin$theta
head(qdat)

with(qdat, {
  plot(var ~ mean, xlab = "Mean number of articles", ylab = "Variance",
       pch = 16, cex = 1.2, cex.lab = 1.2)
  abline(h = mean(PhdPubs$articles), col = gray(.40), lty = "dotted")
  lines(mean, qvar, col = "red", lwd = 2)
  lines(mean, nbvar, col = "blue", lwd = 2)
  lines(lowess(mean, var), lwd = 2, lty = "dashed")
  text(3, mean(PhdPubs$articles), "Poisson", col = gray(.40))
  text(3, 5, "quasi-Poisson", col = "red")
  text(3, 6.7, "negbin", col = "blue")
  text(3, 8.5, "lowess")
})

# sandwich functions to compute standard errors with over disperson
phd.SE <- sqrt(cbind(
  pois = diag(vcov(phd.pois)),
  sand = diag(sandwich(phd.pois)),
  qpois = diag(vcov(phd.qpois)),
  nbin = diag(vcov(phd.nbin))))
round(phd.SE, 4)

dispersiontest(phd.pois)
dispersiontest(phd.pois, 2)

countreg::rootogram(phd.pois, max = 12, 
                    main = "PhDPubs: Poisson")
countreg::rootogram(phd.nbin, max = 12, 
                    main = "PhDPubs: Negative-Binomial")

countreg::rootogram(crabs.pois, max = 15, 
                    main = "CrabSatellites: Poisson")
countreg::rootogram(crabs.nbin, max = 15, 
                    main = "CrabSatellites: Negative-Binomial")

set.seed(1234)
data1 <- rzipois(200, 3, 0)
data2 <- rzipois(200, 3, .3)

tdata1 <- table(data1)
barplot(tdata1, xlab = "Count", ylab = "Frequency",
        main = "Poisson(3)")
tdata2 <- table(data2)
barplot(tdata2, xlab = "Count", ylab = "Frequency",
        main = expression("ZI Poisson(3, " * pi * "= .3)"))
detach(package:VGAM)

CrabSatellites <- transform(CrabSatellites,
                            color = as.numeric(color))
op <- par(cex.lab=1.2, mfrow = c(1, 2))
plot(factor(satellites == 0) ~ weight, data = CrabSatellites,
     breaks = quantile(weight, probs = seq(0,1,.2)), ylevels = 2:1,
     ylab = "No satellites")
plot(factor(satellites == 0) ~ color, data = CrabSatellites,
     breaks = quantile(color, probs = seq(0,1,.33)),  ylevels = 2:1,
     ylab = "No satellites")
par(op)

op <- par(cex.lab = 1.2, mfrow = c(1, 2))
cdplot(factor(satellites == 0) ~ weight
       , data = CrabSatellites,
       ylevels = 2:1, ylab = "No satellites")
cdplot(factor(satellites == 0) ~ color
       , data = CrabSatellites,
       ylevels = 2:1, ylab = "No satellites")
par(op)

data("CodParasites", package = "countreg")
summary(CodParasites[, c(1 : 4, 7)])

gpairs(CodParasites[,c(1:4, 7)],
       diag.pars = list(fontsize = 16),
       mosaic.pars = list(gp = shading_Friendly))

ggpairs(CodParasites[,c(1:4, 7)])

cp.tab <- xtabs(~ area + year + factor(is.na(prevalence) |
                                         prevalence == "yes"),
                data = CodParasites)
dimnames(cp.tab)[3] <- list(c("No", "Yes"))
names(dimnames(cp.tab))[3] <- "prevalence"

doubledecker(prevalence ~ area + year, data = cp.tab,
             margins = c(1, 5, 3, 1))
doubledecker(prevalence ~ area + year, data = cp.tab,
             gp = shading_hcl
             , expected = ~ year:area + prevalence,
             margins = c(1, 5, 3, 1))

ggplot(CodParasites, aes(x = length
                         , y = as.numeric(prevalence) - 1)) +
  geom_jitter(position = position_jitter(height = .05)
              , alpha = 0.25) +
  geom_rug(position = "jitter", sides = "b") +
  stat_smooth(method = "loess", color = "red", 
              fill = "red", size = 1.5) +
  labs(y = "prevalence")

# plot only positive values of intensity
CPpos <- subset(CodParasites, intensity > 0)
ggplot(CPpos, aes(x = year, y = intensity)) +
  geom_boxplot(outlier.size = 3, notch = TRUE
               , aes(fill = year),
               alpha = 0.2) +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.25) +
  facet_grid(. ~ area) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200)) +
  theme(legend.position = "none") +
  labs(y = "intensity (log scale)")

# this shows no sign of non linearity
ggplot(CPpos, aes(x = length, y = intensity)) +
  geom_jitter(position = position_jitter(height = .1), alpha = 0.25) +
  geom_rug(position = "jitter", sides = "b") +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200)) +
  stat_smooth(method = "loess", color = "red", fill = "red", size = 2) +
  stat_smooth(method = "lm", size = 1.5)

cp_p  <- glm(intensity ~ length + area * year,
             data = CodParasites, family = poisson)
cp_nb <- glm.nb(intensity ~ length + area * year,
                data = CodParasites)

cp_hp  <- hurdle(intensity ~ length + area * year,
                 data = CodParasites, dist = "poisson")
cp_hnb <- hurdle(intensity ~ length + area * year,
                 data = CodParasites, dist = "negbin")
cp_zip <- zeroinfl(intensity ~ length + area * year,
                   data = CodParasites, dist = "poisson")
cp_znb <- zeroinfl(intensity ~ length + area * year,
                   data = CodParasites, dist = "negbin")

op <- par(mfrow = c(3, 2))
countreg::rootogram(cp_p, max = 50, main = "Poisson")
countreg::rootogram(cp_nb, max = 50, main = "Negative Binomial")
countreg::rootogram(cp_hp, max = 50, main = "Hurdle Poisson")
countreg::rootogram(cp_hnb, max = 50, main = "Hurdle Negative Binomial")
countreg::rootogram(cp_zip, max = 50, main = "Zero-inflated Poisson")
countreg::rootogram(cp_znb, max = 50, 
                    main = "Zero-inflated Negative Binomial")
par(op)

# have to use LRStats as the only measures can compare are AIC and BIC
LRstats(cp_p, cp_nb, cp_hp, cp_hnb, cp_zip, cp_znb, sortby = "BIC")


lrtest(cp_hp, cp_hnb)

vuong(cp_nb, cp_hnb)     # nb vs. hurdle nb
vuong(cp_hnb, cp_znb)    # hurdle nb vs znb
summary(cp_hnb)
# length not significant in the zero model, can remove it
cp_hnb1 <- hurdle(intensity ~ length + area * year | area * year,
                  data = CodParasites, dist = "negbin")

lrtest(cp_hnb, cp_hnb1) # no difference
vuong(cp_hnb, cp_hnb1) # new model is better
detach(package:pscl)

eff.nb <- allEffects(cp_nb)
plot(eff.nb[1], type = "response", ylim = c(0,30),
     main  ="NB model: length effect")

plot(eff.nb[2], type = "response", ylim = c(0,30),
     multiline = TRUE, ci.style = "bars",
     key.args = list(x = .05, y = .95, columns = 1),
     colors = c("black", "red", "blue") ,
     symbols = 15 : 17, cex = 2,
     main = "NB model: area*year effect")

cp_zero  <- glm(prevalence ~ length + area * year,
                data = CodParasites, family = binomial)
cp_nzero <- glm.nb(intensity ~ length + area * year,
                   data = CodParasites, subset = intensity > 0)

eff.zero <- allEffects(cp_zero)
plot(eff.zero[1], ylim=c(-2.5, 2.5),
     main="Hurdle zero model: length effect")

plot(eff.zero[2],  ylim=c(-2.5, 2.5),
     multiline=TRUE,
     key.args=list(x=.05, y=.95, columns=1),
     colors=c("black", "red", "blue"),
     symbols=15:17, cex=2,
     main="Hurdle zero model: area*year effect")

data("NMES1988", package = "AER")
nmes <- NMES1988[, c(1, 6:8, 13, 15, 18)]
plot(table(nmes$visits),
     xlab = "Physician office visits", ylab = "Frequency")
plot(log(table(nmes$visits)),
     xlab = "Physician office visits", ylab = "log(Frequency)")

with(nmes, c(mean = mean(visits),
             var = var(visits),
             ratio = var(visits) / mean(visits)))

op <-par(mfrow=c(1, 3), cex.lab=1.4)
plot(log(visits + 1) ~ cutfac(chronic), data = nmes,
     ylab = "Physician office visits (log scale)",
     xlab = "Number of chronic conditions", main = "chronic")
plot(log(visits + 1) ~ health, data = nmes, varwidth = TRUE,
     ylab = "Physician office visits (log scale)",
     xlab = "Self-perceived health status", main = "health")
plot(log(visits + 1) ~ cutfac(hospital, c(0:2, 8)), data = nmes,
     ylab = "Physician office visits (log scale)",
     xlab = "Number of hospital stays", main = "hospital")
par(op)

ggplot(nmes, aes(x = school, y = visits + 1)) +
  geom_jitter(alpha = 0.25) +
  stat_smooth(method = "loess", color = "red", fill = "red", 
              size = 1.5, alpha = 0.3) +
  labs(x = "Number of years of education", 
       y = "log(Physician office visits + 1)") +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100))

nmes.pois <-    glm(visits ~ ., data = nmes, family = poisson)
nmes.nbin <- glm.nb(visits ~ ., data = nmes)
lrtest(nmes.pois, nmes.nbin) # basic nbin is better
summary(nmes.nbin)
add1(nmes.nbin, . ~ .^2, test = "Chisq")
# add some of the signif terms
nmes.nbin2 <- update(nmes.nbin,
                     . ~ . + (health + chronic + hospital)^2
                     + health : school)
lrtest(nmes.nbin, nmes.nbin2)
plot(allEffects(nmes.nbin), ylab = "Office visits")
eff_nbin2 <- allEffects(nmes.nbin2,
                        xlevels = list(hospital = c(0 : 3, 6, 8), 
                                       chronic = c(0:3, 6, 8), 
                                       school = seq(0, 20, 5)))
names(eff_nbin2)
plot(eff_nbin2, "health:chronic", layout = c(3, 1),
     ylab = "Office visits", colors = "blue")

plot(eff_nbin2, 
     "health:chronic", multiline = TRUE, ci.style = "bands",
     ylab = "Office visits", xlab  ="# Chronic conditions",
     key.args = list(x = 0.05, y = .80, corner = c(0, 0), columns = 1))

plot(eff_nbin2, 
     "hospital:health", multiline = TRUE, ci.style = "bands",
     ylab = "Office visits", xlab = "Hospital stays",
     key.args = list(x = 0.05, y = .80, corner = c(0, 0), columns = 1))

plot(eff_nbin2, "hospital:chronic", multiline = TRUE, ci.style = "bands",
     ylab = "Office visits", xlab = "Hospital stays",
     key.args = list(x = 0.05, y = .70, corner = c(0, 0), columns = 1))

plot(eff_nbin2, "health:school", multiline = TRUE,  ci.style = "bands",
     ylab = "Office visits", xlab = "Years of education",
     key.args = list(x = 0.65, y = .1, corner = c(0, 0), columns = 1))

nmes.nbin3 <- update(nmes.nbin2, . ~ . + I(chronic^2) + I(hospital^2))
# same as this
## nmes.nbin3 <- glm.nb(visits ~ poly(hospital, 2) + poly(chronic, 2) +
##                      insurance + school + gender +
##                      (health + chronic + hospital)^2 + health : school,
## 		     data = nmes)

ret <- anova(nmes.nbin, nmes.nbin2, nmes.nbin3)
ret$Model <- c("nmes.nbin", "nmes.nbin2", "nmes.nbin3")
ret
LRstats(nmes.nbin, nmes.nbin2, nmes.nbin3)

eff_nbin3 <- allEffects(nmes.nbin3,
                        xlevels = list(hospital = c(0 : 3, 6, 8), 
                                       chronic = c (0 : 3, 6, 8), 
                                       school = seq(0, 20, 5)))
plot(eff_nbin3, "health : chronic", layout = c(3, 1))

nmes.gamnb <- gam(visits ~ s(hospital, k = 3) + s(chronic, k = 3) +
                    insurance + school + gender +
                    (health + chronic + hospital)^2 + 
                    health : school,
                  data = nmes, family = nb())

persp(nmes.gamnb, hospital ~ chronic, zlab = "log Office visits",
      col = rainbow_hcl(30), contour = list(col = "colors", lwd = 2),
      at = list(school = 10, health = "average"), theta = -60)

persp(nmes.gamnb, school ~ chronic, zlab = "log Office visits",
      col = rainbow_hcl(30), 
      contour = list(col = "colors", lwd = 2, z = "top"),
      at = list(hospital = 0.3, health = "average"), theta = -60)

op <- par(mfrow=c(2,2), mar=c(4,4,2,1)+.1, cex.lab=1.2)
plot(phd.nbin)
par(op)
library(car)
residualPlot(phd.nbin, type = "rstandard", col.smooth = "red", id.n = 3)
residualPlot(phd.nbin, type = "rstandard",
             groups = PhdPubs$articles, key = FALSE, linear = FALSE, 
             smoother = NULL)

residualPlot(phd.nbin, "mentor", type = "rstudent",
             quadratic = TRUE, col.smooth = "red", col.quad = "blue", 
             id.n = 3)
residualPlot(phd.nbin, "phdprestige", type = "rstudent",
             quadratic = TRUE, col.smooth = "red", col.quad = "blue", 
             id.n = 3)

influencePlot(phd.nbin)
outlierTest(phd.nbin)

qqPlot(rstudent(phd.nbin), id.n = 3,
       xlab = "Normal quantiles", ylab = "Studentized residuals")

observed <- sort(abs(rstudent(phd.nbin)))
n <- length(observed)
expected <- qnorm((1:n + n - 1/8) / (2*n + 1/2))
S <- 100
sims <- simulate(phd.nbin, nsim = S)
simdat <- cbind(PhdPubs, sims)
# calculate residuals for one simulated data set
resids <- function(y)
  rstudent(glm.nb(y ~ female + married + kid5 + phdprestige + mentor,
                  data=simdat, start=coef(phd.nbin)))
# do them all ...
simres <- matrix(0, nrow(simdat), S)
for(i in 1:S) {
	simres[,i] <- sort(abs(resids(simdat[,paste("sim", i, sep="_")])))
}
envelope <- 0.95
mean <- apply(simres, 1, mean)
lower <- apply(simres, 1, quantile, prob = (1 - envelope) / 2)
upper <- apply(simres, 1, quantile, prob = (1 + envelope) / 2)

plot(expected, observed,
     xlab = "Expected value of half-normal order statistic",
     ylab = "Absolute value of studentized residual")
lines(expected, mean, lty = 1, lwd = 2, col = "blue")
lines(expected, lower, lty = 2, lwd = 2, col = "red")
lines(expected, upper, lty = 2, lwd = 2, col = "red")
identify(expected, observed, labels = names(observed), n = 3)

res <- rstudent(phd.nbin)
plot(density(res), lwd = 2, col = "blue",
     main = "Density of studentized residuals")
rug(res)

# why the bimodality?
plot(jitter(log(PhdPubs$articles + 1), factor = 1.5), res,
     xlab = "log (articles + 1)", ylab = "Studentized residual")


data("NMES1988", package = "AER")
nmes2 <- NMES1988[, c(1 : 4, 6 : 8, 13, 15, 18)]
names(nmes2)[1 : 4]     # responses
names(nmes2)[-(1 : 4)]  # predictors

clog <- function(x) log(x + 1)
nmes.mlm <- lm(clog(cbind(visits, nvisits, ovisits, novisits)) ~ .,
               data = nmes2)


vlabels <- c("Physician\noffice visits", 
             "Non-physician\n office visits",
             "Physician\nhospital visits", 
             "Non-physician\nhospital visits")
pairs(nmes.mlm, factor.means = "health", 
      fill = TRUE, var.labels = vlabels)

vars <- colnames(nmes2)[1 : 4]
nmes.long <- reshape(nmes2,
                     varying = vars,
                     v.names = "visit",
                     timevar = "type",
                     times = vars,
                     direction = "long",
                     new.row.names = 1 : (4 * nrow(nmes2)))

nmes.long <- nmes.long[order(nmes.long$id),]
nmes.long <- transform(nmes.long,
                       practitioner = ifelse(type %in% c("visits", "ovisits"),
                                             "physician", "nonphysician"),
                       place = ifelse(type %in% c("visits", "nvisits"), "office", "hospital"),
                       hospf = cutfac(hospital, c(0 : 2, 8)),
                       chronicf = cutfac(chronic))

xtabs(visit ~ practitioner + place, data = nmes.long)
fourfold(xtabs(visit ~ practitioner + place + health, data = nmes.long),
         mfrow=c(1,3))
loddsratio(xtabs(visit ~ practitioner + place + health, 
                 data = nmes.long))
tab <- xtabs(visit ~ practitioner + place + gender + 
               insurance + chronicf,
             data = nmes.long)
fourfold(tab, mfcol=c(4,4), varnames=FALSE)

lodds.df <- as.data.frame(loddsratio(tab))
ggplot(lodds.df, aes(x = chronicf, y = LOR,
                     ymin = LOR - 1.96 * ASE, ymax = LOR + 1.96 * ASE,
                     group = insurance, color = insurance)) +
  geom_line(size = 1.2) + geom_point(size = 3) +
  geom_linerange(size = 1.2) +
  geom_errorbar(width = 0.2) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_hline(yintercept = mean(lodds.df$LOR), linetype = "dotdash") +
  facet_grid(. ~ gender, labeller = label_both) +
  labs(x = "Number of chronic conditions",
       y = "log odds ratio (physician|place)") +
  theme_bw() + theme(legend.position = c(0.1, 0.9)) 

lodds.mod <- lm(LOR ~ (gender + insurance + chronicf)^2,
                weights = 1 / ASE^2, data = lodds.df)
anova(lodds.mod)

nmes2.nbin <- vglm(cbind(visits, nvisits, ovisits, novisits) ~ .,
                   data = nmes2, family = negbinomial)
coef(nmes2.nbin, matrix = TRUE)[,c(1, 2)]
# theta for visits
exp(coef(nmes2.nbin, matrix = TRUE)[1, 2])
coef(nmes2.nbin, matrix = TRUE)[,c(1, 3, 5, 7)]

clist <- constraints(nmes2.nbin, type = "term")
clist$hospital[c(1, 3, 5, 7),]

## ----nmes5-clist2--------------------------------------------------------
clist2 <- clist
clist2$hospital <- cbind(rowSums(clist$hospital))
clist2$chronic  <- cbind(rowSums(clist$chronic))
clist2$hospital[c(1, 3, 5, 7), 1, drop = FALSE]

nmes2.nbin2 <- vglm(cbind(visits, nvisits, ovisits, novisits) ~ ., 
                    data = nmes2, constraints = clist2,
                    family = negbinomial(zero = NULL))

coef(nmes2.nbin2, matrix = TRUE)[,c(1, 3, 5, 7)]
lrtest(nmes2.nbin, nmes2.nbin2)
lh <- paste("hospital:", 1 : 3, " = ", "hospital:", 2 : 4, sep="")
lh
if (packageVersion("car") < "2.1.1") {
  df.residual.vglm <- function(object, ...) object@df.residual
  vcov.vglm <- function(object, ...) vcovvlm(object, ...)
  coef.vglm <- function(object, ...) coefvlm(object, ...)
}

car::linearHypothesis(nmes2.nbin, lh)

