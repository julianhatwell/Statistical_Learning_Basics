library(vcd)
library(vcdExtra)
library(car)
library(ggplot2)
library(MASS)

options("contrasts")
myTable <- UCBAdmissions
dimnames(myTable) <- list(A = c("Y", "N")
                          , B = c("M", "F")
                          , C = tolower(letters[1:6])
                          )

# these are all equivalent
loglin(myTable, margin = list(c(1, 2), c(1, 3), c(2, 3)))
loglm(~ (A + B + C)^2, data = myTable)
loglm(Freq ~ (A + B + C)^2, data = as.data.frame(myTable))
glm(Freq ~ (A + B + C)^2, data = as.data.frame(myTable)
    , family = poisson)

berkeley <- as.data.frame(UCBAdmissions)
berk.glm1 <- glm(Freq ~ Dept * (Gender + Admit), data = berkeley, 
                 family = "poisson")
fit <- fitted(berk.glm1)
hat <- hatvalues(berk.glm1)
stderr <- sqrt(1 - hat)

op <- par(mar = c(5,4,1,1)+.1)
plot(fit, stderr, cex = 5 * hat,
     ylab = "Std. Error of Residual", xlab = "Fitted Frequency",
     cex.lab = 1.2)
labs <- with(berkeley,
             paste(Dept, substr(Gender, 1, 1), ifelse(Admit == "Admitted", "+", "-"), sep = ""))
col <- ifelse(berkeley$Admit == "Admitted", "blue", "red")
text(fit, stderr, labs, col = col, cex = 1.2)
par(op)

# looking at the mutual independance model, though we know it can't be true
# and we're trying to investigate gender bias
data("UCBAdmissions")
berk.loglm0 <- loglm(~ Dept + Gender + Admit, data = UCBAdmissions,
                     param = TRUE, fitted = TRUE)
berk.loglm0
# using the fitted frequencies to view the table of expected values
structable(Dept ~ Admit + Gender, fitted(berk.loglm0))

# conditional independence in UCB admissions data
berk.loglm1 <- loglm(~ Dept * (Gender + Admit), data = UCBAdmissions)
berk.loglm1

# homogeneous association, [AD][AG][GD]
berk.loglm2 <-loglm(~ (Admit + Dept + Gender)^2, data = UCBAdmissions)
berk.loglm2

# model 2 is no improvement on model 1
anova(berk.loglm0, berk.loglm1, berk.loglm2, test = "Chisq")

# using glm - data frame in freq form
berk.glm1 <- glm(Freq ~ Dept * (Gender + Admit),
                 data = berkeley, family = "poisson")
berk.glm2 <- glm(Freq ~ (Dept + Gender + Admit)^2,
                 data = berkeley, family = "poisson")
# these models are equiv to loglm
anova(berk.glm1, berk.glm2, test = "Chisq")
# with glm object can get contribution of each term
anova(berk.glm1, test = "Chisq")

# diagnostic
mosaic(berk.glm1, shade = TRUE, 
       formula = ~ Dept + Admit + Gender, split = TRUE,
       residuals_type = "rstandard", # this works on the glm object specifically
       main = "Model: [AdmitDept][GenderDept]", 
       labeling = labeling_residuals, 
       abbreviate_labs = c(Gender = TRUE),
       keep_aspect_ratio = FALSE)

# addressing the association that is condition on dept A with a new dummy var
berkeley <- within(berkeley, 
                   dept1AG <- (Dept == "A") * 
                     (Gender == "Female") * 
                     (Admit == "Admitted"))
head(berkeley)
berk.glm3 <- glm(Freq ~ Dept * (Gender + Admit) + dept1AG, 
                 data = berkeley, family = "poisson")
LRstats(berk.glm3) # matches well the saturated model
anova(berk.glm1, berk.glm3, test = "Chisq") # huge improvement on deviance

coef(berk.glm3)[["dept1AG"]]
exp(coef(berk.glm3)[["dept1AG"]])

# well fitting model
mosaic(berk.glm3, shade = TRUE, 
       formula = ~ Dept + Admit + Gender, split = TRUE,
       residuals_type = "rstandard", 
       main = "Model: [DeptGender][DeptAdmit] + DeptA*[GA]",
       labeling = labeling_residuals, 
       abbreviate_labs = c(Gender = TRUE),
       keep_aspect_ratio = FALSE)

# Same ops with logistic
# log odds of admission - log(admit/reject)
(obs <- log(UCBAdmissions[1,,] / UCBAdmissions[2,,]))

berk.logit2 <- glm(Admit == "Admitted" ~ Dept + Gender,
                   data = berkeley, weights = Freq, family = "binomial")
summary(berk.logit2)

# special model for dept A
berkeley <- within(berkeley,
                   dept1AG <- (Dept == "A") * (Gender == "Female"))
berk.logit3 <- glm(Admit == "Admitted" ~ Dept + Gender + dept1AG,
                   data = berkeley, weights = Freq, family = "binomial")

Anova(berk.logit2)
Anova(berk.logit3)

# plotting logit model
pred2 <- cbind(berkeley[,1:3], fit = predict(berk.logit2))
pred2 <- cbind(subset(pred2, Admit == "Admitted"), obs = as.vector(obs))
head(pred2)

ggplot(pred2, aes(x = Dept, y = fit, group = Gender, color = Gender)) +
  geom_line(size = 1.2) +
  geom_point(aes(x = Dept, y = obs, group = Gender, color = Gender), size = 4) +
  ylab("Log odds (Admitted)") + theme_bw() +
  theme(legend.position = c(.8, .9)
        , legend.title = element_text(size = 14)
        , legend.text = element_text(size = 14)
        )

pred3 <- cbind(berkeley[,1:3], fit = predict(berk.logit3))
pred3 <- cbind(subset(pred3, Admit == "Admitted"), obs = as.vector(obs))
head(pred3)

ggplot(pred3, aes(x = Dept, y = fit, group = Gender, color = Gender)) +
  geom_line(size = 1.2) +
  geom_point(aes(x = Dept, y = obs, group = Gender, color = Gender), size = 4) +
  ylab("Log odds (Admitted)") + theme_bw() +
  theme(legend.position = c(.8, .9)
        , legend.title = element_text(size = 14)
        , legend.text = element_text(size = 14)
  )

# structural and sample zeros
Health <- expand.grid(concerns = c("sex", "menstrual",
                                   "healthy", "nothing"),
                      age      = c("12-15", "16-17"),
                      gender   = c("M", "F"))
Health$Freq <- c(4, 0, 42, 57, 2, 0, 7, 20,
                 9, 4, 19, 71, 7, 8, 10, 21)
# eliminating struct zeros from the model
health.glm0 <- glm(Freq ~ concerns + age + gender, data = Health,
                   subset = (Freq > 0), family = poisson)
health.glm1 <- glm(Freq ~ concerns + age * gender, data = Health,
                   subset = (Freq > 0), family = poisson)
# neither fits well
LRstats(health.glm0, health.glm1)

mosaic(health.glm1, ~ concerns + age + gender, 
       residuals_type = "rstandard", 
       rot_labels = 0, 
       just_labels = c(left = "right"),
       margin = c(left = 5))
# here we lose an important parameter
health.glm2 <- glm(Freq ~ concerns*gender + concerns*age, data = Health,
                   subset = (Freq > 0), family = poisson)
LRstats(health.glm2)
summary(health.glm2)


health.tab <- xtabs(Freq ~ concerns + age + gender, data = Health)
nonzeros <- ifelse(health.tab>0, 1, 0)
health.loglm0 <- loglm(~ concerns + age + gender,
                       data = health.tab, start = nonzeros)
health.loglm1 <- loglm(~ concerns + age * gender,
                       data = health.tab, start = nonzeros)
# df is wrong
health.loglm2 <- loglm(~ concerns*gender + concerns*age,
                       data = health.tab, start = nonzeros)
LRstats(health.loglm0, health.loglm1, health.loglm2)
