library(MASS)
library(vcd)
library(vcdExtra)
data("HairEyeColor", package = "datasets")
haireye <- margin.table(HairEyeColor, 1:2)
mosaic(haireye, pop = FALSE)
labeling_cells(text = haireye, gp_text = gpar(fontface = 1), clip = FALSE)(haireye)

mosaic(haireye, labeling = labeling_values) # same as above

(hair <- margin.table(haireye, 1))
prop.table(hair)

mosaic(hair, labeling = labeling_values)

expected <- rep(sum(hair) / 4, 4) # unlikely equiprobability model
names(expected) <- names(hair)
expected

(residuals <- (hair - expected) / sqrt(expected)) # Pearson residuals

mosaic(hair, expected = expected, labeling = labeling_values)

round(addmargins(prop.table(haireye, 1), 2), 3)

mosaic(haireye, shade=TRUE, suppress=0,
       labeling=labeling_residuals, gp_text=gpar(fontface=2))

mosaic(haireye, shade = TRUE, labeling = labeling_residuals)

# to manually compute the test of independence
exp <- independence_table(haireye)
resids <- (haireye - exp) / sqrt(exp)
round(resids, 2)

(chisq <- sum(resids ^ 2))
(df <- prod(dim(haireye) - 1))
pchisq(chisq, df, lower.tail = FALSE)

# or the easy way
chisq.test(haireye)
round(residuals(chisq.test(haireye)), 2) # can extract the residuals from this

# re-order the table
haireye2 <- as.table(haireye[, c("Brown", "Hazel", "Green", "Blue")])
mosaic(haireye2, shade = TRUE)

# color by eye color
fill_colors <- c("brown4", "#acba72", "green", "lightblue")
(fill_colors_mat <- t(matrix(rep(fill_colors, 4), ncol = 4)))
mosaic(haireye2, gp = gpar(fill = fill_colors_mat, col = 0))

# color by hair color
fill_colors <- c("black", "brown4", "orange", "yellow")
(fill_colors_mat <- t(matrix(rep(fill_colors, 4), ncol = 4)))
mosaic(haireye2, gp = gpar(fill = fill_colors_mat, col = 0))

# a built in diagonal shading scheme
mosaic(haireye2, gp = shading_Marimekko(haireye2))

# toeplitz designs
library(colorspace)
toeplitz(1 : 4)
fill_colors <- rainbow_hcl(8)[1 + toeplitz(1 : 4)]
mosaic(haireye2, gp = gpar(fill = fill_colors, col = 0))

mosaic(haireye2, gp = shading_diagonal(haireye2))

mosaic(haireye2, highlighting = "Eye", highlighting_fill = fill_colors)
mosaic(Eye ~ Hair, data = haireye2, highlighting_fill = fill_colors)

# more shading levels
mosaic(haireye2, shade = TRUE, gp_args = list(interpolate = 1 : 4))

# continuous shading
interp <- function(x) pmin(x / 6, 1)
mosaic(haireye2, shade = TRUE, gp_args = list(interpolate = interp))

mosaic(haireye2, gp = shading_Friendly, legend = legend_fixed)
set.seed(1234)
mosaic(haireye2, gp = shading_max)

art <- xtabs(~ Treatment + Improved, data = Arthritis,
             subset = Sex == "Female")
names(dimnames(art))[2] <- "Improvement"

# this shading, not so significant
mosaic(art, gp = shading_Friendly, margin = c(right = 1),
       labeling = labeling_residuals, suppress = 0, digits = 2)

# this chi sq test
chisq.test(art) # very significant
residuals(chisq.test(art))

# shading max calls coindep_test(art)
coindep_test(art) # generates 1000 random tables with same margins
# Finally, the 0.90 and 0.99 quantiles
# of the simulation distribution are used
# as shading levels, passed as the value
# of the interpolate argument.
set.seed(1243)
art_max <- coindep_test(art)
art_max$residuals
art_max$qdist(c(0.90, 0.99))

set.seed(1234)
mosaic(art, gp = shading_max, margin = c(right = 1))

data("UKSoccer", package = "vcd")
CMHtest(UKSoccer) # no significant association

set.seed(1234)
mosaic(UKSoccer, gp = shading_max, labeling = labeling_residuals,
       digits = 2) # one cell stands out for further attention

HEC <- HairEyeColor[, c("Brown", "Hazel", "Green", "Blue"),]
mosaic(HEC, rot_labels = c(right = -45)) 
# seems like there are slightly too many blue/blonde female

# fitting the log linear models
loglm(~ Hair + Eye, data = haireye) # strong lack of fit

# are hair and eye jointly independent of sex?
HE_S <- loglm(~ Hair * Eye + Sex, data = HairEyeColor)
HE_S
residuals(HE_S, type = "pearson") # not significant

HEC <- HairEyeColor[, c("Brown", "Hazel", "Green", "Blue"),]
mosaic(HEC, expected = ~ Hair * Eye + Sex,
       labeling = labeling_residuals,
       digits = 2, rot_labels = c(right = -45))

abbrev <- list(abbreviate = c(FALSE, FALSE, 1))
mosaic(HEC, expected = ~ Hair + Eye + Sex, labeling_args = abbrev,
       main = "Model: ~ Hair + Eye + Sex")
mosaic(HEC, expected = ~ Hair * Sex + Eye * Sex, labeling_args = abbrev,
       main="Model: ~ Hair*Sex + Eye*Sex")

# three types of independence:
mod1 <- loglm(~ Hair + Eye + Sex, data = HEC)       # mutual
mod2 <- loglm(~ Hair * Sex + Eye * Sex, data = HEC) # conditional
mod3 <- loglm(~ Hair * Eye + Sex, data = HEC)       # joint
LRstats(mod1, mod2, mod3)

anova(mod1)
anova(mod1, mod2, mod3, test = "chisq")

# independent model of [hair][eye][sex]
mosaic(HEC, expected = ~ Hair + Eye + Sex, legend = FALSE, labeling_args = abbrev, main = "Mutual")
# independent model of [hair][eye]
mosaic(~ Hair + Eye, data = HEC, shade = TRUE, legend = FALSE, main = "Marginal")
# joint model of [hair eye][sex]
mosaic(HEC, expected = ~ Hair * Eye + Sex, legend = FALSE, labeling_args = abbrev, main = "Joint")

# independence modeling
for(nf in 2 : 5) {
  print(loglin2string(joint(nf, factors = LETTERS[1:5])))
}
for(nf in 2 : 5) {
  print(loglin2string(conditional(nf, factors = LETTERS[1:5]), 
                      sep = ""))
}
for(nf in 2 : 5) {
  print(loglin2formula(conditional(nf, factors = LETTERS[1:5])))
}
# applied to a atable
loglin2formula(joint(3, table = HEC))
loglin2string(joint(3, table = HEC))

HEC.mods <- seq_loglm(HEC, type = "joint")
LRstats(HEC.mods)

data("PreSex", package = "vcd")
structable(Gender + PremaritalSex + ExtramaritalSex ~ MaritalStatus, 
           data = PreSex)

PreSex <- aperm(PreSex, 4 : 1)   # order variables G, P, E, M

# (Gender Pre)
mosaic(margin.table(PreSex, 1 : 2), shade = TRUE,
       main = "Gender and Premarital Sex")

## (Gender Pre)(Extra)
mosaic(margin.table(PreSex, 1 : 3),
       expected = ~ Gender * PremaritalSex + ExtramaritalSex,
       main = "Gender*Pre + ExtramaritalSex")

# odds ration for men and women is the same
loddsratio(margin.table(PreSex, 1 : 3), stratum = 1, log = FALSE)

## (Gender Pre Extra)(Marital)
mosaic(PreSex,
       expected = ~ Gender * PremaritalSex * ExtramaritalSex
       + MaritalStatus,
       main = "Gender*Pre*Extra + MaritalStatus")
## (GPE)(PEM)
mosaic(PreSex,
       expected = ~ Gender * PremaritalSex * ExtramaritalSex
       + MaritalStatus * PremaritalSex * ExtramaritalSex,
       main = "G*P*E + P*E*M")

data("Employment", package = "vcd")
structable(Employment)

# baseline model [A][BC]
loglm(~ EmploymentStatus + EmploymentLength * LayoffCause, 
      data = Employment)

mosaic(Employment, shade = TRUE,
       expected = ~ EmploymentStatus + EmploymentLength * LayoffCause,
       main = "EmploymentStatus + Length * Cause")

# conditional model [AC][BC]
loglm(~ EmploymentStatus * LayoffCause + EmploymentLength * LayoffCause,
      data = Employment)

mosaic(Employment, shade = TRUE, gp_args = list(interpolate = 1 : 4),
       expected = ~ EmploymentStatus * LayoffCause + 
         EmploymentLength * LayoffCause,
       main = "EmploymentStatus * Cause + Length * Cause")

# creating a list of formulas, note the literal margin in apply.
mods.list <-
  apply(Employment, "LayoffCause",
        function(x) loglm(~ EmploymentStatus + EmploymentLength, 
                          data = x))
mods.list

mosaic(Employment[,,"Closure"], shade = TRUE, 
       gp_args = list(interpolate = 1 : 4),
       margin = c(right = 1), main = "Layoff: Closure")
mosaic(Employment[,,"Replaced"], shade = TRUE, 
       gp_args = list(interpolate = 1 : 4),
       margin = c(right = 1), main = "Layoff: Replaced")

data("Punishment", package = "vcd")
str(Punishment, vec.len = 2)

pun <- xtabs(Freq ~ memory + attitude + age + education, 
             data = Punishment)
dimnames(pun) <- list(
  Memory = c("yes", "no"),
  Attitude = c("no", "moderate"),
  Age = c("15-24", "25-39", "40+"),
  Education = c("Elementary", "Secondary", "High"))

(mod.cond <- loglm(~ Memory * Age * Education + 
                     Attitude * Age * Education, data = pun))

set.seed(1071)
coindep_test(pun, margin = c("Age", "Education"),
             indepfun = function(x) sum(x ^ 2), aggfun = sum)

set.seed(1071)
pun_cotab <- cotab_coindep(pun, condvars = 3 : 4, type = "mosaic",
                           varnames = FALSE, margins = c(2, 1, 1, 2),
                           test = "sumchisq", interpolate = 1 : 2)
cotabplot(~ Memory + Attitude | Age + Education,
          data = pun, panel = pun_cotab)

mods.list <- apply(pun, c("Age", "Education"),
                   function(x) loglm(~ Memory + Attitude, data = x)$pearson)

mosaic(~ Memory + Attitude | Age + Education, data = pun,
       shade = TRUE, gp_args = list(interpolate = 1 : 4))

# mosaic matrix
pairs(Bartlett, gp = shading_Friendly2)

data("PreSex", package = "vcd")
pairs(PreSex, gp = shading_Friendly2, space = 0.25,
      gp_args = list(interpolate = 1 : 4), 
      diag_panel_args = list(offset_varnames = -0.5))

largs <- list(labeling = labeling_border(varnames = FALSE,
                                         labels = c(T, T, F, T), alternate_labels = FALSE))
dargs <- list(gp_varnames = gpar(fontsize = 20), offset_varnames = -1,
              labeling = labeling_border(alternate_labels = FALSE))
pairs(UCBAdmissions, shade = TRUE, space = 0.25,
      diag_panel_args = dargs,
      upper_panel_args = largs, lower_panel_args = largs)

pairs(UCBAdmissions, space = 0.2,
      lower_panel = pairs_mosaic(type = "joint"),
      upper_panel = pairs_mosaic(type = "total"))

pairs(UCBAdmissions, type = "conditional", space = 0.2)

library(gpairs)
data("Arthritis", package = "vcd")
gpairs(Arthritis[,c(5, 2, 3, 4)],
       diag.pars = list(fontsize = 20),
       mosaic.pars = list(gp = shading_Friendly,
                          gp_args = list(interpolate = 1 : 4)))

mosaic3d(Bartlett)

# a simulation to show visualisation of models
struc <- array(c(6, 10, 312, 44,
                 37, 31, 192, 76),
               dim = c(2, 2, 2),
               dimnames = list(Age = c("Young", "Old"),
                               Sex = c("F", "M"),
                               Disease = c("No", "Yes"))
)
struc <- as.table(struc)
structable(struc)

mosaic(struc, shade = TRUE)
mosaic(struc, type = "expected")

mutual <- loglm(~ Age + Sex + Disease, data = struc, fitted = TRUE)
fit <- as.table(fitted(mutual))
structable(fit)
pairs(fit, gp = shading_Friendly2, type = "total")
mosaic3d(fit)

joint <- loglm(~ Age * Sex + Disease, data = struc, fitted = TRUE)
fit <- as.table(fitted(joint))
structable(fit)
pairs(fit, gp = shading_Friendly2)

doubledecker(Admit ~ Dept + Gender, data = UCBAdmissions[2:1, , ])
doubledecker(Survived ~ Class + Age + Sex, Titanic)

# odds ratio diagrams
data("Punishment", package = "vcd")
pun_lor <- loddsratio(Freq ~ memory + attitude | age + education, 
                      data = Punishment)
pun_lor_df <- as.data.frame(pun_lor)

plot(pun_lor)
pun_lor_df <- transform(pun_lor_df, 
                        age = as.numeric(age), 
                        education = as.numeric(education))

pun_mod <- lm(LOR ~ age * education, data = pun_lor_df, 
              weights = 1 / ASE^2)
anova(pun_mod)

Titanic2 <- Titanic[, , 2:1, 2:1]
Titanic2["Crew", , "Child", ] <- NA
titanic_lor1 <- loddsratio(~ Survived + Age | Class + Sex, 
                           data = Titanic2)
titanic_lor1

titanic_lor2 <- loddsratio(~ Survived + Sex | Class + Age, 
                           data = Titanic2)
titanic_lor2

plot(titanic_lor1)
plot(titanic_lor2)

