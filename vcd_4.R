library(vcd)
library(vcdExtra)
library(extracat)
library(MASS)
hec <- margin.table(HairEyeColor, 2:1)
tile(hec)
fluctile(hec)

data("Mental", package = "vcdExtra")
mental <- xtabs(Freq ~ ses + mental, data = Mental)
spineplot(mental)
fluctile(mental)

Berkeley <- margin.table(UCBAdmissions, 2:1)
library(gmodels)
CrossTable(Berkeley, prop.chisq = FALSE, prop.c = FALSE, 
           format = "SPSS")

p <- c(0.05, .1, .25, .50, .75, .9, .95)
odds <- p / (1 - p)
logodds <- log(odds)
logits <- data.frame(p, odds, logodds)
logits

data("UCBAdmissions")
UCB <- margin.table(UCBAdmissions, 1:2)
(LOR <- loddsratio(UCB))
(OR <- loddsratio(UCB, log = FALSE))
summary(LOR)
confint(LOR)
confint(OR)
fisher.test(UCB)

data("Arthritis", package = "vcd")
Art <- xtabs(~ Treatment + Improved, data = Arthritis)
Art
round(100 * prop.table(Art, margin = 1), 2)
assocstats(Art)

Art2 <- xtabs(~ Treatment + Improved + Sex, data = Arthritis)
Art2
assocstats(Art2)
CMHtest(Art2)
# the test was more significant for female 
# because of the greater number of female in the data
apply(Art2, 3, sum)

data("Mental", package = "vcdExtra")
mental <- xtabs(Freq ~ ses + mental, data = Mental)
assocstats(mental)    # standard chisq tests
CMHtest(mental)       # CMH tests

# general association
cmhdemo1 <- read.table(header=TRUE, sep="", text="
                       b1  b2   b3  b4  b5
                       a1    0  15   25  15   0
                       a2    5  20    5  20   5
                       a3   20   5    5   5  20
                       ")
cmhdemo1 <- as.matrix(cmhdemo1)

# linear association
cmhdemo2 <- read.table(header=TRUE, sep="", text="
                       b1  b2   b3  b4  b5
                       a1    2   5    8   8   8
                       a2    2   8    8   8   5
                       a3    5   8    8   8   2
                       a4    8   8    8   5   2
                       ")

cmhdemo2 <- as.matrix(cmhdemo2)

CMHtest(cmhdemo1)
CMHtest(cmhdemo2)

sieve(cmhdemo1, shade=TRUE, main="General association",
  gp = shading_sieve(interpolate = 0, lty = c("solid", "longdash")))
sieve(cmhdemo2, shade=TRUE, main="Linear association",
  gp = shading_sieve(interpolate = 0, lty = c("solid", "longdash")))

# for 2x2xk tables, determine if the odds ratios differ across the strata
woolf_test(UCBAdmissions)
woolf_test(Art2)

# the loglinear model agrees with the woolf test on the art data
loglm(~ (Treatment + Improved + Sex)^2, data = Art2)
loglm(~ (Gender + Admit + Dept)^2, data = UCBAdmissions)

fourfold(Berkeley, std = "ind.max")   # unstandardized
fourfold(Berkeley, margin = 1)        # equating gender
fourfold(Berkeley)  # standardize both margins

summary(loddsratio(Berkeley))
exp(.6103 + c(-1, 1) * qnorm(.975) * 0.06398)
confint(loddsratio(Berkeley, log = FALSE))

UCB <- aperm(UCBAdmissions, c(2, 1, 3))
fourfold(UCB, mfrow = c(2, 3))

data("CoalMiners", package = "vcd")
CM <- CoalMiners[, , 2 : 9]
structable(. ~ Age, data = CM)
fourfold(CM, mfcol = c(2, 4))

loddsratio(CM)
loddsratio(CM, log = FALSE)

lor_CM <- loddsratio(CM)
plot(lor_CM, bars=FALSE, baseline=FALSE, whiskers=.2)

lor_CM_df <- as.data.frame(lor_CM)
age <- seq(25, 60, by = 5) + 2
lmod <- lm(LOR ~ poly(age, 2), weights = 1 / ASE^2, data = lor_CM_df)
grid.lines(seq_along(age), fitted(lmod), 
           gp = gpar(col = "red", lwd = 2), default.units = "native")

summary(lmod)

haireye <- margin.table(HairEyeColor, 1:2)
expected = independence_table(haireye)
round(expected, 1)

sieve(haireye, shade=TRUE, sievetype="expected",
     main="Expected frequencies")
sieve(haireye, shade=TRUE,
     main="Observed frequencies")

sieve(haireye, sievetype = "expected", shade = TRUE,
      main="Expected frequencies",
      labeling = labeling_values, value_type = "expected",
      gp_text = gpar(fontface = 2), gp = shading_sieve(interpolate = 0, line_col="darkgrey",eps=Inf,lty="dashed"))

sieve(haireye, shade = TRUE, main="Observed frequencies",
      labeling = labeling_values, value_type = "observed",
      gp_text = gpar(fontface = 2))

data("VisualAcuity", package = "vcd")
VA <- xtabs(Freq ~ right + left + gender, data = VisualAcuity)
dimnames(VA)[1:2] <- list(c("high", 2, 3, "low"))
names(dimnames(VA))[1:2] <- paste(c("Right", "Left"), "eye grade")
structable(aperm(VA))

sieve(VA[, , "female"], shade = TRUE)

sieve(Freq ~ right + left | gender, data = VisualAcuity, 
      shade = TRUE, set_varnames = c(right = "Right eye grade", 
                                     left = "Left eye grade"))

cotabplot(VA, cond = "gender", panel = cotab_sieve, shade = TRUE)

sieve(UCBAdmissions, shade = TRUE, condvar = 'Gender')

sieve(~ Dept + Admit + Gender, data = UCBAdmissions, 
      shade = TRUE, labeling = labeling_values, 
      gp_text = gpar(fontface = 2), abbreviate_labs = c(Gender = TRUE))

cotabplot(UCBAdmissions, cond = "Gender", panel = cotab_sieve, 
          shade = TRUE)

cotabplot(UCBAdmissions, cond = "Dept", panel = cotab_sieve, 
          shade = TRUE, labeling = labeling_values, 
          gp_text = gpar(fontface = "bold"))

UCB2 <- aperm(UCBAdmissions, c(3, 2, 1))
sieve(UCB2, shade = TRUE
      , expected = ~ Admit * Gender + Dept
      , split_vertical = c(FALSE, TRUE, TRUE))

assoc(~ Hair + Eye, data = HairEyeColor, shade = TRUE, gp_axis = gpar(lty = 5))
assoc(HairEyeColor, shade = TRUE, gp_axis = gpar(lty = 5))

data("SexualFun", package = "vcd")
SexualFun

MSPatients[, , "Winnipeg"]
MSPatients[, , "New Orleans"]
apply(MSPatients, 3, sum)      # show sample sizes

Kappa(SexualFun)
confint(Kappa(SexualFun))

oldpar <- par()
op <- par(mar=c(4,3,4,1)+.1)
agreementplot(SexualFun, main = "Unweighted", weights = 1)
agreementplot(SexualFun, main = "Weighted")
par(op)

B <- agreementplot(SexualFun)
unlist(B)[1 : 2]

op <- par(mar = c(4, 3, 4, 1) + .1)
data("Mammograms", package = "vcdExtra")
B <- agreementplot(Mammograms, main = "Mammogram ratings")
par(op)

unlist(B)[1 : 2]

cotabplot(MSPatients, cond = "Patients", panel = cotab_agreementplot,
          text_gp = gpar(fontsize = 18), xlab_rot=20)

agr1 <- agreementplot(MSPatients[, , "Winnipeg"])
agr2 <- agreementplot(MSPatients[, , "New Orleans"])
rbind(Winnipeg = unlist(agr1), NewOrleans = unlist(agr2))[, 1 : 2]

library(ggtern)
DATA <- data.frame(
A = c(40, 20, 10),
B = c(30, 60, 10),
C = c(30, 20, 80),
id = c("1", "2", "3"))
ggtern(data = DATA,
mapping = aes(x=C, y=A, z=B, colour = id)) +
geom_point(size=4) +
geom_text(vjust=-.5, size=8, aes(label=id)) +
theme_rgbw() +
theme(plot.margin=unit(c(0,0,0,0),"mm"))

library(ggtern)
DATA <- data.frame(
  A = c(40, 20, 10),
  B = c(30, 60, 10),
  C = c(30, 20, 80),
  id = c("1", "2", "3"))

aesthetic_mapping <- aes(x = C, y = A, z = B, colour = id)
ggtern(data = DATA, mapping = aesthetic_mapping) +
    geom_point(size = 4) +
    theme_rgbw()


data("Lifeboats", package = "vcd")
# label boats with more than 10% men
Lifeboats$id <- ifelse(Lifeboats$men / Lifeboats$total > .1,
                       as.character(Lifeboats$boat), "")

AES <- aes(x = women, y = men, z = crew, colour = side, shape = side,
           label = id)
ggtern(data = Lifeboats, mapping = AES) +
    geom_text() +
    geom_point(size=2) +
    geom_smooth_tern(method = "lm", alpha = 0.2)

data("Lifeboats", package="vcd")
# label boats with more than 10% men
Lifeboats$id <- ifelse(Lifeboats$men/Lifeboats$total > .1,
                       as.character(Lifeboats$boat), "")
ggtern(data = Lifeboats, 
       mapping = aes(x = women, y = men, z = crew, colour=side, shape=side, label=id)) +
  theme_rgbw() +
  geom_point(size=2) +
  labs(title = "Lifeboats on the Titanic") +
  labs(T="Women and children") +
  geom_smooth_tern(method="lm", size=1.5, alpha=.2, aes(fill=side)) +
  geom_text(vjust=1, color="black") +
  theme(legend.position=c(.85, .85)
        #, axis.tern.vshift=unit(5,"mm")
        )
ggplot(data = Lifeboats,
       aes(x=launch, y=total, colour=side,  label=boat)) +
  geom_smooth(method="lm", aes(fill=side), size=1.5) +
  geom_smooth(method="loess", aes(fill=side), se=FALSE, size=1.2) +
  geom_point() + ylim(c(0,100)) +
  geom_text(vjust=-.5, color="black") +
  labs(y="Total loaded", x="Launch time")