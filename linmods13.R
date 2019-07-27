library(faraway)
library(Amelia)
library(dplyr)
data("chmiss")
summary(chmiss)
rowSums(is.na(chmiss))
image(is.na(chmiss), axes=FALSE, col=gray(1:0))
axis(2, at=0:5/5, labels=colnames(chmiss))
axis(1, at=0:46/46, labels=row.names(chmiss), las=2)

data("chredlin")
modfull <- lm(involact ~ . - side, data=chredlin)
sumary(modfull)
modmiss <- lm(involact ~ ., data=chmiss)
sumary(modmiss)

(cmeans <- colMeans(chmiss, na.rm = TRUE))
mchm <- chmiss
# skipping involact response
for (i in c(1:4, 6)) mchm[is.na(chmiss[, i]), i] <- cmeans[i]
imod <- lm(involact~., data = mchm)
sumary(imod)

lmodr <- lm(race~fire+theft+age+income, data=chmiss)
predict(lmodr, chmiss[is.na(chmiss$race),])

lmodr <- lm(logit(race/100)~fire+theft+age+income, data=chmiss)
ilogit(predict(lmodr, chmiss[is.na(chmiss$race),]))*100
chredlin$race[is.na(chmiss$race)]

set.seed(123)
chimp <- amelia(chmiss, m=25)
betas <- NULL
ses <- NULL
for(i in 1:chimp$m) {
  lmod <- lm(involact ~ race+fire+theft+age
             , chimp$imputations[[i]])
  betas <- rbind(betas, coef(lmod))
  ses <- rbind(ses, coef(summary(lmod))[,2])
}

(cr <- mi.meld(q=betas, se=ses))
cr$q.mi / cr$se.mi # t stats

# exercises
data("kanga")
summary(kanga)
image(is.na(kanga), axes=FALSE, col=gray(1:0))
axis(2, at=0:19/19, labels=colnames(kanga))
axis(1, at=0:147/147, labels=row.names(kanga), las=2)
kanga_o <- kanga
colSums(is.na(kanga_o))[which(colSums(is.na(kanga_o)) > 0)]
sum(rowSums(is.na(kanga_o))[which(rowSums(is.na(kanga_o)) > 0)])

kanga_o <- select(kanga, -palate.width)
colSums(is.na(kanga_o))[which(colSums(is.na(kanga_o)) > 0)]
sum(rowSums(is.na(kanga_o))[which(rowSums(is.na(kanga_o)) > 0)])

kanga_o <- select(kanga, -mandible.length, -palate.width)
colSums(is.na(kanga_o))[which(colSums(is.na(kanga_o)) > 0)]
sum(rowSums(is.na(kanga_o))[which(rowSums(is.na(kanga_o)) > 0)])

kanga_o <- select(kanga, -occipital.depth)
colSums(is.na(kanga_o))[which(colSums(is.na(kanga_o)) > 0)]
sum(rowSums(is.na(kanga_o))[which(rowSums(is.na(kanga_o)) > 0)])

kanga_o <- select(kanga, -occipital.depth, -palate.width)
colSums(is.na(kanga_o))[which(colSums(is.na(kanga_o)) > 0)]
sum(rowSums(is.na(kanga_o))[which(rowSums(is.na(kanga_o)) > 0)])

kanga_o <- select(kanga, -occipital.depth, -mandible.length, -palate.width)
colSums(is.na(kanga_o))[which(colSums(is.na(kanga_o)) > 0)]
sum(rowSums(is.na(kanga_o))[which(rowSums(is.na(kanga_o)) > 0)])

image(is.na(kanga_o[names(rowSums(is.na(kanga_o))[which(rowSums(is.na(kanga_o)) > 0)]), ]), axes=FALSE, col=gray(1:0))

kanga_sc <- scale(select(kanga, -(1:2)))
kanga_prc_na <- prcomp(na.omit(kanga_sc))

kanga_prc_na$sdev

kanga_sc <- data.frame(select(kanga, 1:2), kanga_sc)
set.seed(123)
kanga_mi <- amelia(kanga_sc, m=25, noms=1:2)

ses <- NULL
rots <- NULL
for(i in 1:kanga_mi$m) {
  kanga_prc <- prcomp(select(kanga_mi$imputations[[i]], -(1:2)))
  ses <- rbind(ses, kanga_prc$sdev)
}

apply(ses, 2, mean)

data("pima")
summary(pima)
nrow(pima)
pima_na <- pima

for (v in c("diastolic", "glucose"
            , "triceps", "insulin"
            , "bmi")) {
  pima_na[[v]] <- ifelse(pima[[v]] == 0, NA, pima[[v]])
}
summary(pima_na)
image(is.na(pima_na), axes=FALSE, col=gray(1:0))
lmod <- lm(diastolic~.,data=pima_na)
sumary(lmod)

pima_na <- pima
for (v in c("diastolic", "glucose"
            , "triceps", "insulin"
            , "bmi")) {
  pima_na[[v]] <- ifelse(pima[[v]] == 0
                         , mean(pima_na[[v]], na.rm = TRUE)
                         , pima[[v]])
}
summary(pima_na)
image(is.na(pima_na), axes=FALSE, col=gray(1:0))
lmod <- lm(diastolic~.,data=pima_na)
sumary(lmod)

pima_na <- pima
for (v in c("diastolic", "glucose"
            , "triceps", "insulin"
            , "bmi")) {
  pima_na[[v]] <- ifelse(pima[[v]] == 0, NA, pima[[v]])
}
image(is.na(pima_na), axes=FALSE, col=gray(1:0))
for (v in c("diastolic", "glucose"
            , "triceps", "insulin"
            , "bmi")) {
  lb <- min(pima_na[[v]], na.rm = TRUE)
  ub <- max(pima_na[[v]], na.rm = TRUE)
  sc <- (pima_na[[v]] - lb)/(ub - lb)
  fmla <- as.formula(paste("logit(sc) ~ pregnant + diabetes + age + test"))
  lmodr <- lm(fmla, pima_na)
  preds <- ilogit(predict(lmodr, pima_na[is.na(pima_na[[v]]), ])) * (ub - lb) + lb
  pima_na[is.na(pima_na[[v]]), v] <- preds
}

summary(pima_na)
image(is.na(pima_na), axes=FALSE, col=gray(1:0))
lmod <- lm(diastolic~.,data=pima_na)
sumary(lmod)

pima_na <- pima
for (v in c("diastolic", "glucose"
            , "triceps", "insulin"
            , "bmi")) {
  pima_na[[v]] <- ifelse(pima[[v]] == 0, NA, pima[[v]])
}
image(is.na(pima_na), axes=FALSE, col=gray(1:0))

set.seed(123)
pima_mi <- amelia(pima_na, m=25)

ses <- NULL
betas <- NULL
for( i in 1:pima_mi$m) {
lmod <- lm(diastolic~.
           , pima_mi$imputations[[i]])
  betas <- rbind(betas, coef(lmod))
  ses <- rbind(ses, coef(summary(lmod))[,2])
}

(cr <- mi.meld(q=betas, se=ses))
cr$q.mi / cr$se.mi # t stats
