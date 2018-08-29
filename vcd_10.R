library(vcd)
library(vcdExtra)
library(corrplot)
library(gnm)
library(logmult)
library(car)
library(VGAM)
library(effects)
library(lmtest)

data("Mental", package = "vcdExtra")
(mental.tab <- xtabs(Freq ~ mental + ses, data=Mental))
(LMT <- loddsratio(mental.tab))

corrplot(as.matrix(LMT), method = "square", is.corr = FALSE,
        tl.col = "black", tl.srt = 0, tl.offset = 1)

mean(LMT$coefficients)

# baseline independence model
indep <- glm(Freq ~ mental + ses, data = Mental, family = poisson)
LRstats(indep) # poor fit

long.labels <- list(set_varnames = c(mental="Mental Health Status",
                                     ses="Parent SES"))
mosaic(indep,
       gp=shading_Friendly,
       residuals_type="rstandard",
       labeling_args = long.labels,
       labeling=labeling_residuals, suppress=1,
       main="Mental health data: Independence")

Cscore <- as.numeric(Mental$ses)
Rscore <- as.numeric(Mental$mental)

# adding the row and column scores to the model
linlin <- update(indep, . ~ . + Rscore:Cscore)
roweff <- update(indep, . ~ . + mental:Cscore)
coleff <- update(indep, . ~ . + Rscore:ses)
rowcol <- update(indep, . ~ . + Rscore:ses + mental:Cscore)

LRstats(indep, linlin, roweff, coleff, rowcol)
# note the C and R when solo cannot be nested
anova(indep, linlin, roweff, test = "Chisq")
anova(indep, linlin, coleff, test = "Chisq")

# interpret linlin association parameter
coef(linlin)[["Rscore:Cscore"]]
exp(coef(linlin)[["Rscore:Cscore"]])

# This corresponds to a local odds ratio,
# This single number describes the association succinctly: 
# each step down the socioeconomic scale 
# increases the odds of being classified 
# one step poorer in mental health by 9.5%.

# plot the fitted odds ratios to view the model structure
linlin.fit <- matrix(fitted(linlin), 4, 6, 
                     dimnames=dimnames(mental.tab))
round(as.matrix(loddsratio(linlin.fit)), 3)

roweff.fit <- matrix(fitted(roweff), 4, 6, 
                     dimnames=dimnames(mental.tab))
round(as.matrix(loddsratio(roweff.fit)), 3)

coleff.fit <- matrix(fitted(coleff), 4, 6, 
                     dimnames = dimnames(mental.tab))
round(as.matrix(loddsratio(coleff.fit)), 3)

plot(t(loddsratio(linlin.fit)), confidence = FALSE, 
     legend_pos="bottomright", ylim = c(-.2, .3),
     main = "log odds ratios for ses and mental, L*L model")
plot(t(loddsratio(roweff.fit)), confidence = FALSE, 
     legend_pos="bottomright", ylim = c(-.2, .3),
     main = "log odds ratios for ses and mental, R model")
plot(t(loddsratio(coleff.fit)), confidence = FALSE, 
     legend_pos = "bottomright", ylim = c(-.2, .3),
     main="log odds ratios for ses and mental, C model")
rowcol.fit <- matrix(fitted(rowcol), 4, 6, dimnames = dimnames(mental.tab))
plot(t(loddsratio(rowcol.fit)), confidence = FALSE, 
     legend_pos = "bottomright", ylim = c(-.2, .3),
     main = "log odds ratios for ses and mental, R+C model")

# set up for nonlin
contrasts(Mental$mental) <- contr.treatment
contrasts(Mental$ses) <- contr.treatment

# this is just to formulate indep with gnm. Should be the same as before
indep <- gnm(Freq ~ mental + ses, data = Mental, family = poisson)
# now can update with Mult and instances(Mult(a,b), n)
RC1 <- update(indep, . ~ . + Mult(mental, ses), verbose = FALSE)
RC2 <- update(indep, . ~ . + instances(Mult(mental, ses), 2), 
              verbose = FALSE)

LRstats(indep, linlin, roweff, coleff, RC1, RC2)
anova(linlin, RC1, RC2, test = "Chisq")

rowProbs <- with(Mental, tapply(Freq, mental, sum) / sum(Freq))
colProbs <- with(Mental, tapply(Freq, ses, sum) / sum(Freq))
mu <- getContrasts(RC1, pickCoef(RC1, "[.]mental"),
                   ref = rowProbs, scaleWeights = rowProbs)
nu <- getContrasts(RC1, pickCoef(RC1, "[.]ses"),
                   ref = colProbs, scaleWeights = colProbs)

(alpha <- mu$qvframe)
(beta  <- nu$qvframe)
scores <- rbind(alpha, beta)
scores <- cbind(scores,
                factor = c(rep("mental", 4), rep("ses", 6)) )
rownames(scores) <- c(levels(Mental$mental), levels(Mental$ses))
scores$lower <- scores[,1] - scores[,2]
scores$upper <- scores[,1] + scores[,2]
scores

op <- par(mar=c(5, 4, 1, 1) + .1)
with(scores, {
  dotchart(Estimate, groups = factor, labels = rownames(scores),
           cex = 1.2, pch = 16, xlab = "RC1 Score",
           xlim = c(min(lower), max(upper)))
  arrows(lower, c(8 + (1 : 4), 1 : 6), upper, c(8 + (1 : 4), 1 : 6),
         col = "red", angle = 90, length = .05, code = 3, lwd = 2)
})
par(op)

rc1 <- rc(mental.tab, verbose = FALSE, weighting = "marginal",
          se = "jackknife")
rc2 <- rc(mental.tab, verbose = FALSE, weighting = "marginal", nd = 2,
          se = "jackknife")

coords  <- plot(rc2, conf.ellipses = 0.68, cex = 1.5,
                rev.axes = c(TRUE, FALSE))

scores <- rbind(coords$row, coords$col)
lines(scores[1 : 4,], col = "blue", lwd = 2)
lines(scores[-(1 : 4),], col = "red", lwd = 2)

# symmetry models
data("VisualAcuity", package="vcd")
women <- subset(VisualAcuity, gender=="female", select=-gender)

indep <- glm(Freq ~ right + left,  data = women, family = poisson)
quasi <- update(indep, . ~ . + Diag(right, left))

symm <- glm(Freq ~ Symm(right, left), data = women, family = poisson)
qsymm <- update(symm, . ~ right + left + .)

LRstats(indep, quasi, symm, qsymm)

labs <- c("High", "2", "3", "Low")
largs <- list(set_varnames = c(right = "Right eye grade",
                               left = "Left eye grade"),
              set_labels=list(right = labs, left = labs))
mosaic(quasi, ~ right + left, residuals_type = "rstandard",
       gp = shading_Friendly,
       labeling_args = largs,
       main = "Quasi-Independence (women)")
# It can be seen in the left panel that
# the non-diagonal associations are largely symmetric, 
# and also that when they differ, 
# visual acuity in the two eyes is most likely to differ
# by only one eye grade.
mosaic(symm, ~ right + left, residuals_type = "rstandard",
       gp = shading_Friendly,
       labeling_args = largs,
       main = "Quasi-Symmetry (women)")
mosaic(qsymm, ~ right + left, residuals_type = "rstandard",
       gp = shading_Friendly,
       labeling_args = largs,
       main = "Quasi-Symmetry (women)")
anova(symm, qsymm, test = "Chisq")

data("Hauser79", package = "vcdExtra")
structable(~ Father + Son, data = Hauser79)

hauser.tab <- xtabs(Freq ~ Father + Son, data = Hauser79)
(lor.hauser <- loddsratio(hauser.tab))

plot(lor.hauser, confidence = FALSE, legend_pos = "topleft", 
     xlab = "Father's status comparisons")
m <- mean(lor.hauser$coefficients)        # mean LOR
grid.lines(x = unit(c(0, 1), "npc"),
           y = unit(c(m, m), "native"))

plot(t(lor.hauser), confidence = FALSE, legend_pos = "topleft", 
     xlab = "Father's status comparisons")
m <- mean(lor.hauser$coefficients)        # mean LOR
grid.lines(x = unit(c(0, 1), "npc"),
           y = unit(c(m, m), "native"))

hauser.indep <- gnm(Freq ~ Father + Son, data = Hauser79, 
                    family = poisson)
hauser.quasi <-  update(hauser.indep, ~ . + Diag(Father, Son))
LRstats(hauser.indep, hauser.quasi)

mosaic(hauser.indep, ~ Father + Son, main = "Independence model",
       gp = shading_Friendly)
mosaic(hauser.quasi, ~ Father + Son, main = "Quasi-independence model",
       gp = shading_Friendly)

hauser.qsymm <-  update(hauser.indep,
                        ~ . + Diag(Father, Son) + Symm(Father, Son))
LRstats(hauser.qsymm)
mosaic(hauser.qsymm, ~ Father + Son, main = "Quasi-symmetry model",
       gp = shading_Friendly, residuals_type = "rstandard")

levels <- matrix(c(
  2,  4,  5,  5,  5,
  3,  4,  5,  5,  5,
  5,  5,  5,  5,  5,
  5,  5,  5,  4,  4,
  5,  5,  5,  4,  1
), 
5, 5, byrow = TRUE)
hauser.topo <- update(hauser.indep, 
                      ~ . + Topo(Father, Son, spec = levels))
LRstats(hauser.topo)
as.vector((coef(hauser.topo)[pickCoef(hauser.topo, "Topo")]))
LRstats(hauser.indep, hauser.quasi, hauser.qsymm, hauser.topo)
# q sym still better

mosaic(hauser.topo, ~ Father + Son, main = "Topographic model",
       gp = shading_Friendly)

Fscore <- as.numeric(Hauser79$Father)   # numeric scores
Sscore <- as.numeric(Hauser79$Son)      # numeric scores
# uniform association
hauser.UA <- update(hauser.indep, ~ . + Fscore * Sscore)
# row effects model
hauser.roweff <- update(hauser.indep, ~ . + Father * Sscore)
# col effects 
hauser.coleff <- update(hauser.indep, ~ . + Son * Fscore)
# RC model
hauser.RC <- update(hauser.indep, 
                    ~ . + Mult(Father, Son), verbose = FALSE)
LRstats(hauser.indep, hauser.UA, hauser.roweff, hauser.RC)

hauser.UAdiag <- update(hauser.UA, ~ . + Diag(Father, Son))
anova(hauser.UA, hauser.UAdiag, test = "Chisq")
coef(hauser.UAdiag)[["Fscore:Sscore"]]
# For comparisons not involving the diagonal cells, 
# each step down the scale of occupational categories 
# for the father multiplies the odds that the son 
# will also be in one lower category by exp(0.158) = 1.172, 
# an increase of 17%. 

hauser.CR <- update(hauser.indep, ~ . + Crossings(Father, Son))
hauser.CRdiag <- update(hauser.CR, ~ . + Diag(Father, Son))
LRstats(hauser.CR, hauser.CRdiag)

nu <- coef(hauser.CRdiag)[pickCoef(hauser.CRdiag, "Crossings")]
names(nu) <- gsub("Crossings(Father, Son)C", "nu", names(nu), 
                  fixed = TRUE)
nu

mosaic(hauser.CRdiag, ~ Father + Son,
       gp = shading_Friendly, residuals_type = "rstandard",
       main = "Crossings() + Diag()")

modlist <- glmlist(hauser.indep, hauser.roweff, hauser.UA, 
                   hauser.UAdiag, hauser.quasi, hauser.qsymm, 
                   hauser.topo, hauser.RC, hauser.CR, hauser.CRdiag)
LRstats(modlist, sortby = "BIC")
sumry <- LRstats(modlist)
mods <- substring(rownames(sumry), 8)
with(sumry, {
  plot(Df, BIC, cex = 1.3, pch = 19,
       xlab = "Degrees of freedom", ylab = "BIC (log scale)",
       log = "y", cex.lab = 1.2)
  pos <- ifelse(mods == "UAdiag", 1, 3)
  text(Df, BIC + 55, mods, pos = pos, col = "red", xpd = TRUE, cex = 1.2)
})


# Fitting with Kway
## Freq ~ 1
## Freq ~ right + left + gender
## Freq ~ (right + left + gender)^2
## Freq ~ (right + left + gender)^3
vis.kway <- Kway(Freq ~ right + left + gender, data = VisualAcuity)
LRstats(vis.kway)
vis.indep <- glm(Freq ~ right + left + gender,  data = VisualAcuity,
                 family = poisson)
vis.quasi <- update(vis.indep, . ~ . + Diag(right, left))
vis.qsymm <- update(vis.indep, . ~ . + Diag(right, left) 
                    + Symm(right, left))
LRstats(vis.indep, vis.quasi, vis.qsymm)
mosaic(vis.qsymm, ~ gender + right + left, condvars = "gender",
       residuals_type = "rstandard", gp = shading_Friendly,
       labeling_args = largs, rep = FALSE,
       main = "Homogeneous quasi-symmetry")
# opposite resid patterns in men and women

vis.hetdiag <- update(vis.indep, . ~ . + gender * Diag(right, left) +
                        Symm(right, left))
vis.hetqsymm <- update(vis.indep, . ~ . + gender * Diag(right, left) +
                         gender * Symm(right, left))
LRstats(vis.qsymm, vis.hetdiag, vis.hetqsymm)

mosaic(vis.hetqsymm, ~ gender + right + left, condvars="gender",
       residuals_type = "rstandard", gp = shading_Friendly,
       labeling_args = largs, rep = FALSE,
       main="Heterogeneous quasi-symmetry")


data("CoalMiners", package = "vcd")
coalminers <- data.frame(t(matrix(aperm(CoalMiners, c(2, 1, 3)),
                                  4, 9)))
colnames(coalminers) <- c("BW", "Bw", "bW", "bw")
coalminers$age <- c(22, 27, 32, 37, 42, 47, 52, 57, 62)
coalminers

logitsCM <- blogits(coalminers[, 1 : 4], add = 0.5)
colnames(logitsCM)[1:2] <- c("logitB", "logitW")
logitsCM

col <- c("blue", "red", "black")
pch <- c(15, 17, 16)
age <- coalminers$age

op <- par(mar = c(4, 4, 1, 4)+.2)
matplot(age, logitsCM, type = "p",
        col = col, pch = pch, cex = 1.2, cex.lab = 1.25,
        xlab = "Age", ylab = "Log Odds or Odds Ratio")
abline(lm(logitsCM[,1] ~ age), col = col[1], lwd = 2)
abline(lm(logitsCM[,2] ~ age), col = col[2], lwd = 2)
abline(lm(logitsCM[,3] ~ age), col = col[3], lwd = 2)

# right probability axis
probs <- c(.01, .05, .10, .25, .5)
axis(4, at = qlogis(probs), labels = probs)
mtext("Probability", side = 4, cex = 1.2, at = -2, line = 2.5)
# curve labels
text(age[2], logitsCM[2, 1] + .5, "Breathlessness", 
     col = col[1], pos = NULL, cex = 1.2)
text(age[2], logitsCM[2, 2] + .5, "Wheeze", 
     col = col[2], pos = NULL, cex = 1.2)
text(age[2], logitsCM[2, 3] - .5, "log OR\n(B|W)/(B|w)", 
     col = col[3], pos = 1, cex = 1.2)
par(op)

CM <- as.data.frame(CoalMiners)
colnames(CM)[1:2] <- c("B", "W")
head(CM)

cm.glm0 <- glm(Freq ~ B + W + Age, data = CM, family = poisson)
cm.glm1 <- glm(Freq ~ B * W + Age, data = CM, family = poisson)
LRstats(cm.glm0, cm.glm1)

vnames <- list(set_varnames = c(B = "Breathlessness", W = "Wheeze"))
lnames <- list(B=c("B", "b"), W = c("W", "w"))
mosaic(cm.glm1, ~ Age + B + W,
       labeling_args = vnames, set_labels = lnames)

cm.glm2 <- glm(Freq ~ B * W + (B + W) * Age, data = CM, family = poisson)
# same as Freq ~ (B + W + Age)^2 
LRstats(cm.glm1, cm.glm2)
Anova(cm.glm2)
CM$age <- rep(seq(22, 62, 5), each = 4)
CM$ageOR <- (CM$B == "B") * (CM$W == "W") * CM$age
cm.glm3 <- update(cm.glm2, . ~ . + ageOR)
LRstats(cm.glm0, cm.glm1, cm.glm2, cm.glm3)

anova(cm.glm2, cm.glm3, test = "Chisq")

coalminers <- transform(coalminers, agec = (age - 42) / 5)
coalminers$Age <- dimnames(CoalMiners)[[3]]
coalminers
#                      00  01  10  11
cm.vglm1 <- vglm(cbind(bw, bW, Bw, BW) ~ agec,
                 binom2.or(zero = NULL), data = coalminers)
cm.vglm1

(G2 <- deviance(cm.vglm1))
# test residual deviance
1-pchisq(deviance(cm.vglm1), cm.vglm1@df.residual)

coef(cm.vglm1, matrix = TRUE)
exp(coef(cm.vglm1, matrix = TRUE))
#Thus, the odds of a miner showing breathlessness
# are multiplied by 1.67, a 67% increase, 
# for each 5 years' increase in age; 
# similarly, the odds of wheeze are multiplied by 1.38,
# a 38% increase. The odds ratio for the association 
# between the two symptoms are multiplied by 0.88,
# a 12% decrease over each 5-year interval.

age <- coalminers$age
P <- fitted(cm.vglm1)
colnames(P) <- c("bw", "bW", "Bw", "BW")
head(P)
Y <- depvar(cm.vglm1)

col <- c("red", "blue", "red", "blue")
pch <- c(1, 2, 16, 17)

op <- par(mar = c(5, 4, 1, 1) + .1)
matplot(age, P, type = "l",
  col = col,
  lwd = 2, cex = 1.2, cex.lab = 1.2,
  xlab = "Age", ylab = "Probability",
  xlim = c(20,65))
matpoints(age, Y, pch = pch, cex = 1.2, col = col)
# legend
text(64, P[9,]+ c(0,.01, -.01, 0), labels = colnames(P), col = col, cex = 1.2)
text(20, P[1,]+ c(0,.01, -.01, .01), labels = colnames(P), col = col, cex = 1.2)
par(op)

lP <- qlogis(P)
lY <- qlogis(Y)
logitsP <- blogits(P[, 4 : 1])
logitsY <- blogits(Y[, 4 : 1])

cm.vglm2 <- vglm(cbind(bw, bW, Bw, BW) ~ poly(agec, 2),
                 binom2.or(zero = NULL), data = coalminers)
(LR <- deviance(cm.vglm1) - deviance(cm.vglm2))
1 - pchisq(LR, cm.vglm1@df.residual - cm.vglm2@df.residual)

data("Toxaemia", package = "vcdExtra")
str(Toxaemia)
tox.tab <- xtabs(Freq ~ class + smoke + hyper + urea, Toxaemia)
ftable(tox.tab, row.vars = 1)

margin.table(tox.tab, 2 : 1)

mosaic(~ smoke + class, data = tox.tab, shade = TRUE,
       main = "Predictors", legend = FALSE)
mosaic(~ hyper + urea, data = tox.tab, shade = TRUE,
       main = "Responses", legend = FALSE)
cotabplot(~ hyper + urea | smoke, tox.tab, shade = TRUE,
          legend = FALSE, layout = c(1, 3))
cotabplot(~ hyper + urea | class, tox.tab, shade = TRUE,
          legend = FALSE, layout = c(1, 5))

fourfold(aperm(tox.tab), fontsize = 16)

(LOR <- loddsratio(urea ~ hyper | smoke + class, data = tox.tab))
plot(t(LOR), confidence = FALSE, legend_pos = "bottomright", 
     xlab = "Social class of mother")

tox.hyper <- glm(hyper == "High" ~ class * smoke, weights = Freq,
                 data = Toxaemia, family = binomial)
tox.urea <- glm(urea == "High" ~ class * smoke, weights = Freq,
                data = Toxaemia, family = binomial)

plot(allEffects(tox.hyper),
     ylab = "Probability (hypertension)",
     xlab = "Social class of mother",
     main = "Hypertension: class*smoke effect plot",
     colors = c("blue", "black", "red"),
     lwd=3,  multiline = TRUE,
     key.args = list(x = 0.05, y = 0.2, cex = 1.2, columns = 1)
)
plot(allEffects(tox.hyper),
     ylab = "Probability (hypertension)",
     xlab = "Social class of mother",
     main = "Hypertension: class*smoke effect plot",
     colors = c("blue", "black", "red"),
     lwd=3,  #multiline = TRUE,
     key.args = list(x = 0.05, y = 0.2, cex = 1.2, columns = 1)
)

plot(allEffects(tox.urea),
     ylab = "Probability (Urea)",
     xlab = "Social class of mother",
     main = "Urea: class*smoke effect plot",
     colors = c("blue", "black", "red"),
     lwd=3,  multiline = TRUE,
     key.args = list(x = 0.65, y = 0.2, cex = 1.2, columns = 1)
)

plot(allEffects(tox.urea),
     ylab = "Probability (Urea)",
     xlab = "Social class of mother",
     main = "Urea: class*smoke effect plot",
     colors = c("blue", "black", "red"),
     lwd=3, # multiline = TRUE,
     key.args = list(x = 0.65, y = 0.2, cex = 1.2, columns = 1)
)

tox.tab <- xtabs(Freq~class + smoke + hyper + urea, Toxaemia)
toxaemia <- t(matrix(aperm(tox.tab), 4, 15))
colnames(toxaemia) <- c("hu", "hU", "Hu", "HU")
rowlabs <- expand.grid(smoke = c("0", "1-19", "20+"), 
                       class = factor(1:5))
toxaemia <- cbind(toxaemia, rowlabs)
head(toxaemia)

tox.vglm1 <- vglm(cbind(hu, hU, Hu, Hu) ~ class + smoke,
                  binom2.or(zero = 3), data = toxaemia)
coef(tox.vglm1, matrix=TRUE)

# null model
tox.glm0 <- glm(Freq ~ class*smoke + hyper + urea,
                data = Toxaemia, family = poisson)
# baseline model: no association between predictors and responses
tox.glm1 <- glm(Freq ~ class*smoke + hyper*urea,
                data = Toxaemia, family = poisson)

tox.glm2 <- update(tox.glm1, . ~ . + smoke*hyper + class*urea)
tox.glm3 <- glm(Freq ~ (class + smoke + hyper + urea)^2,
                data=Toxaemia, family=poisson)

tox.glm4 <- glm(Freq ~ class*smoke*hyper + hyper*urea + class*urea,
                data=Toxaemia, family=poisson)
tox.glm5 <- update(tox.glm4, . ~ . + smoke*urea)
tox.glm6 <- update(tox.glm4, . ~ . + class*smoke*urea)
tox.glm7 <- update(tox.glm6, . ~ . + smoke*hyper*urea)
tox.glm8 <- glm(Freq ~ (class + smoke + hyper + urea)^3,
                data = Toxaemia, family = poisson)
tox.glm9 <- glm(Freq ~ (class + smoke + hyper + urea)^4,
                data = Toxaemia, family = poisson)

lmtest::lrtest(tox.glm1, tox.glm2, tox.glm3, tox.glm4, tox.glm5)

# reshape to 15 x 4 table of frequencies
tox.tab <- xtabs(Freq ~ class + smoke + hyper + urea, Toxaemia)
toxaemia <- t(matrix(aperm(tox.tab), 4, 15))
colnames(toxaemia) <- c("hu", "hU", "Hu", "HU")
rowlabs <- expand.grid(smoke = c("0", "1-19", "20+"), 
                       class = factor(1:5))
toxaemia <- cbind(toxaemia, rowlabs)
# observed logits and log odds ratios
logitsTox <- blogits(toxaemia[,4:1], add=0.5)
colnames(logitsTox)[1:2] <- c("logitH", "logitU")
logitsTox <- cbind(logitsTox, rowlabs)
head(logitsTox)

# fitted frequencies, as a 15 x 4 table
Fit <- t(matrix(predict(tox.glm2, type = "response"), 4, 15))
colnames(Fit) <- c("HU", "Hu", "hU", "hu")
Fit <- cbind(Fit, rowlabs)
logitsFit <- blogits(Fit[, 1 : 4], add=0.5)
colnames(logitsFit)[1 : 2] <- c("logitH", "logitU")
logitsFit <- cbind(logitsFit, rowlabs)
matrix(logitsFit$logOR, 3, 5,
       dimnames = list(smoke = c("0", "1-19", "20+"), class = 1 : 5))

ggplot(logitsTox, aes(x = as.numeric(class), y = logitH,
                      color = smoke)) +
  theme_bw() +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("blue", "black", "red")) +
  ylab("log odds (Hypertension)") +
  xlab("Social class of mother") +
  ggtitle("Hypertension") +
  theme(axis.title = element_text(size = 16)) +
  geom_point(data = logitsTox,
             aes(x = as.numeric(class), y = logitH, color = smoke),
             size = 3) +
  theme(legend.position = c(0.85, .6))

ggplot(logitsFit, aes(x = as.numeric(class), y = logitH,
                      color = smoke)) +
  theme_bw() +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("blue", "black", "red")) +
  ylab("log odds (Hypertension)") +
  xlab("Social class of mother") +
  ggtitle("Hypertension") +
  theme(axis.title = element_text(size = 16)) +
  geom_point(data = logitsTox,
             aes(x = as.numeric(class), y = logitH, color = smoke),
             size = 3) +
  theme(legend.position = c(0.85, .6))