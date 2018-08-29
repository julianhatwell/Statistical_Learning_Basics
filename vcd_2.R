library(vcd)
library(vcdExtra)

ds <- datasets(package = c("vcd", "vcdExtra"))
str(ds)
with(ds, table(Package, class))

(art <- xtabs(~ Treatment + Improved
      , data = Arthritis
      , subset = Sex == "Female"))

mosaic(art, gp = shading_Friendly)
mosaic(art, gp = shading_max)

structable(~., data = BrokenMarriage)

data("Bundesliga")

## number of goals per game poisson distributed?
(ngoals1 <- xtabs(~ HomeGoals, data = Bundesliga, subset = Year == 1995))
plot(ngoals1)

(ngoals2 <- xtabs(~ AwayGoals, data = Bundesliga, subset = Year == 1995))
plot(ngoals2)

(ngoals3 <- table(apply(subset(Bundesliga, Year == 1995)[,3:4], 1, sum)))
plot(ngoals3)

(gf1 <- goodfit(ngoals1))
summary(gf1)
plot(gf1)

(gf2 <- goodfit(ngoals2))
summary(gf2)
plot(gf2)

(gf3 <- goodfit(ngoals3))
summary(gf3)
plot(gf3)

Ord_plot(ngoals1)
distplot(ngoals1)

learnAbout <- function(d) {
  data(list = d)
  str(get(d))
  help(topic = d) 
}

learnAbout("Abortion")
learnAbout("Caesar")
learnAbout("DaytonSurvey")

mod.GR <- glm(Freq ~ . + sex*race, data=DaytonSurvey, family=poisson)  # mutual independence + GR
mod.homog.assoc <- glm(Freq ~ .^2, data=DaytonSurvey, family=poisson)  # homogeneous association
Dayton.ACM <- aggregate(Freq ~ cigarette+alcohol+marijuana, data=DaytonSurvey, FUN=sum)

learnAbout("Hoyt")
structable(Status+Sex ~ Rank+Occupation, data=Hoyt) # needs wide display

Hoyt1 <- collapse.table(Hoyt, Status=c("College", rep("Non-College",3)))
plot(Hoyt1, shade=TRUE)

data("UCBAdmissions")
sum(UCBAdmissions)
margin.table(UCBAdmissions)
margin.table(UCBAdmissions, 1)
margin.table(UCBAdmissions, 2)
margin.table(UCBAdmissions, 3)
margin.table(UCBAdmissions, c(1, 3))["Admitted",]

# Construct a tabular display of department (rows)
# and gender (columns), showing the proportion
# of applicants in each cell
# who were admitted 
# relative to the total applicants in that cell.

prop.table(UCBAdmissions, 2)
myTab <- structable(Gender~Admit+Dept, UCBAdmissions)
prop.table(structable(Gender~Dept, UCBAdmissions))

tot.dept <- structable(Gender~Dept, margin.table(UCBAdmissions, c(2,3)))
app.dept <- structable(Gender~Admit+Dept, UCBAdmissions)["Admitted"]
prop.dept <- app.dept/tot.dept

# klaps down to fewer dims
UBB.tab2 <- as.matrix(structable(Dept~Admit+Gender, UCBAdmissions), sep = ":")

learnAbout("DanishWelfare")
with(DanishWelfare, sum(Freq))
# this works because they're in order
DanishWelfare$Alcohol <- ordered(DanishWelfare$Alcohol)
DanishWelfare$Income <- ordered(DanishWelfare$Income)

DanishWelfare.tab <- xtabs(Freq~., data = DanishWelfare)
margin.table(DanishWelfare.tab, 4)
# fewer urban vars
DanishWelfare.clps <- collapse.table(DanishWelfare.tab
  , Urban = c("City", "Not City"
              , "City", "City"
              , "Not City"))

ftable(Alcohol~Urban+Income+Status, DanishWelfare.clps)  
structable(Alcohol+Urban~Income+Status, DanishWelfare.clps)

learnAbout("UKSoccer")
margin.table(UKSoccer)
addmargins(UKSoccer)
mosaic(UKSoccer, gp = shading_max, main = "UK Soccer Scores")

totgoals <- matrix(nrow = 5, ncol = 2)

for (i in 1:2) {
  print(margin.table(UKSoccer, i))
  totgoals[,i] <- as.numeric(dimnames(UKSoccer)[[i]]) * margin.table(UKSoccer, i)
}

totgoals
sum(totgoals[,1])
sum(totgoals[,2])

library(lattice)
xyplot(totgoals[,1]+totgoals[,2]~0:4, type = "l")

plot(totgoals[,1], type = "l")
lines(totgoals[,2], type = "l", col = "blue")

learnAbout("Saxony")
learnAbout("Geissler")
gf <- goodfit(Saxony, type = "binomial")
gf
summary(gf)
plot(gf)

Saxony12<-subset(Geissler, size==12, select=c(boys, Freq))
rownames(Saxony12) <- NULL
xtabs(Freq~boys, Saxony12)

learnAbout("VisualAcuity")

xtabs(Freq~left+right+gender, VisualAcuity)

structable(~ gender + left + right, data = VisualAcuity)
sieve(Freq ~ left + right | gender, data = VisualAcuity, shade = TRUE)
cotabplot(Freq ~ left + right | gender, data = VisualAcuity,
          panel = cotab_agreementplot)

