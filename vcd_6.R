library(vcd)
library(ca)
haireye <- margin.table(HairEyeColor, 1 : 2)
(haireye.ca <- ca(haireye))
summary(haireye.ca)

(Phi <- haireye.ca$rowcoord)
(Gamma <- haireye.ca$colcoord)

# demonstrate orthogonality of std coordinates | this doesn't mean much to me.
Dr <- diag(haireye.ca$rowmass)
zapsmall(t(Phi) %*% Dr %*% Phi)
Dc <- diag(haireye.ca$colmass)
zapsmall(t(Gamma) %*% Dc %*% Gamma)

op <- par(cex=1.4, mar=c(5,4,1,2)+.1)
res <- plot(haireye.ca) # silently returns co-ordinates
par(op)
res

data("Mental", package="vcdExtra")
mental.tab <- xtabs(Freq ~ ses + mental, data = Mental)
mental.ca <- ca(mental.tab)
summary(mental.ca)

op <- par(cex=1.3, mar=c(5,4,1,1)+.1)
res <- plot(mental.ca,  ylim = c(-.2, .2))
lines(res$rows, col = "blue", lty = 3)
lines(res$cols, col = "red", lty = 4)
par(op)

data("RepVict", package = "vcd")
victim.ca <- ca(RepVict)
summary(victim.ca)
chisq.test(RepVict)
(chisq <- sum(RepVict) * sum(victim.ca$sv^2))
op <- par(cex=1.3, mar=c(4,4,1,1)+.1)
res <- plot(victim.ca, labels = c(2, 0))
segments(res$rows[,1], res$rows[,2], res$cols[,1], res$cols[,2])
legend("topleft", legend = c("First", "Second"), title = "Occurrence",
       col = c("blue", "red"), pch = 16 : 17, bg = "gray90")
par(op)

# sim with symmetrical table
RVsym <- (RepVict + t(RepVict)) / 2
RVsym.ca <- ca(RVsym)
res <- plot(RVsym.ca) # all points overlap
all.equal(res$rows, res$cols)

data("TV", package = "vcdExtra")
TV2 <- margin.table(TV, c(1, 3))
TV2
TV.ca <- ca(TV2)
TV.ca
res <- plot(TV.ca)
segments(0, 0, res$cols[,1], res$cols[,2], col = "red", lwd = 2)
days.order <- order(TV.ca$rowcoord[,1])
mosaic(t(TV2[days.order,]), shade = TRUE, legend = FALSE,
        labeling = labeling_residuals, suppress=0)

set.seed(1234)
dim <- c(3, 2, 2, 2)
tab <- array(rpois(prod(dim), 15), dim = dim)
dimnames(tab) <- list(Pet = c("dog", "cat", "bird"), 
                      Age = c("young", "old"), 
                      Color = c("black", "white"), 
                      Sex = c("male", "female"))
ftable(Pet + Age ~ Color + Sex, tab)
(pet.mat <- as.matrix(ftable(Pet + Age ~ Color + Sex, tab), sep = '.'))

tab.df <- as.data.frame(as.table(tab))
tab.df <- within(tab.df, 
                 {Pet.Age = interaction(Pet, Age)
                 Color.Sex = interaction(Color, Sex)
                 })               
xtabs(Freq ~ Color.Sex + Pet.Age, data = tab.df)


data("Suicide", package = "vcd")
# interactive coding of sex and age.group
Suicide <- within(Suicide, {
  age_sex <- paste(age.group, toupper(substr(sex, 1, 1)))
})
suicide.tab <- xtabs(Freq ~ age_sex + method2, data = Suicide)
suicide.tab
suicide.ca <- ca(suicide.tab)
summary(suicide.ca)

op <- par(cex=1.3, mar=c(4,4,1,1)+.1)
plot(suicide.ca)
par(op)

suicide.tab3 <- xtabs(Freq ~ sex + age.group + method2, data = Suicide)
# methods, ordered as in the table
suicide.ca$colnames
# order of methods on CA scores for Dim 1
suicide.ca$colnames[order(suicide.ca$colcoord[,1])]
# reorder methods by CA scores on Dim 1
suicide.tab3 <- suicide.tab3[, , order(suicide.ca$colcoord[,1])]
# delete "other"
suicide.tab3 <- suicide.tab3[,, -5]
ftable(suicide.tab3)
library(vcdExtra)
mosaic(suicide.tab3, shade = TRUE, legend = FALSE,
       expected = ~ age.group * sex + method2,
       labeling_args = list(abbreviate_labs = c(FALSE, FALSE, 5)),
       rot_labels = c(0, 0, 0, 90))

suicide.tab2 <- xtabs(Freq ~ age.group + method2, data = Suicide)
suicide.tab2
suicide.ca2 <- ca(suicide.tab2)

# relation of sex and method
suicide.sup <- xtabs(Freq ~ sex + method2, data = Suicide)
suicide.tab2s <- rbind(suicide.tab2, suicide.sup)

suicide.ca2s <- ca(suicide.tab2s, suprow = 6 : 7)
summary(suicide.ca2s)

op <- par(cex=1.3, mar=c(4,4,1,1)+.1)
res <- plot(suicide.ca2s, pch = c(16, 15, 17, 24))
lines(res$rows[6 : 7,])
par(op)

haireye.df <- cbind(
  as.data.frame(haireye),
  model.matrix(Freq ~ Hair + Eye, data=haireye,
               contrasts.arg=list(Hair=diag(4), Eye=diag(4)))[,-1]
)
haireye.df

Z <- expand.dft(haireye.df)[,-(1:2)]
vnames <- c(levels(haireye.df$Hair), levels(haireye.df$Eye))
colnames(Z) <- vnames
dim(Z)
(N <- t(as.matrix(Z[,1:4])) %*% as.matrix(Z[,5:8]))
Z.ca <- ca(Z)
res <- plot(Z.ca, what = c("none", "all"))

# customized plot
res <- plot(Z.ca, what=c("none", "all"), labels = 0, pch = ".", xpd = TRUE)
# extract factor names and levels
coords <- data.frame(res$cols)
coords$factor <- rep(c("Hair", "Eye"), each = 4)
coords$levels <- rownames(res$cols)
coords
# sort by Dim 1
coords <- coords[ order(coords[,"factor"], coords[,"Dim1"]), ]

cols <- c("blue", "red")
nlev <- c(4,4)
text(coords[,1:2], coords$levels, col=rep(cols, nlev), pos=2, cex=1.2)
points(coords[,1:2], pch=rep(16:17, nlev), col=rep(cols, nlev), cex=1.2)

lines(Dim2 ~ Dim1, data=coords, subset=factor=="Eye",  lty=1, lwd=2, col=cols[1])
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Hair", lty=1, lwd=2, col=cols[2])

## ----Burt1---------------------------------------------------------------
Burt <- t(as.matrix(Z)) %*% as.matrix(Z)
rownames(Burt) <- colnames(Burt) <- vnames
Burt

Burt.ca <- ca(Burt)
plot(Burt.ca)

data("PreSex", package = "vcd")
PreSex <- aperm(PreSex, 4:1)   # order variables G, P, E, M
presex.mca <- mjca(PreSex, lambda = "Burt")
summary(presex.mca)

plot(presex.mca)

# plot, but don't use point labels or points
res <- plot(presex.mca, labels = 0, pch = ".", cex.lab = 1.2)
# extract factor names and levels
coords <- data.frame(res$cols, presex.mca$factors)
nlev <- presex.mca$levels.n
fact <- unique(as.character(coords$factor))

cols <- c("blue", "red", "brown", "black")
points(coords[,1:2], pch=rep(16:19, nlev), col=rep(cols, nlev), cex=1.2)
text(coords[,1:2], label=coords$level, col=rep(cols, nlev), pos=3, 
     cex=1.2, xpd=TRUE)
lwd <- c(2, 2, 2, 4)
for(i in seq_along(fact)) {
  lines(Dim2 ~ Dim1, data = coords, subset = factor==fact[i], 
        lwd = lwd[i], col = cols[i])
}

legend("bottomright", 
       legend = c("Gender", "PreSex", "ExtraSex", "Marital"),
       title = "Factor", title.col = "black",
       col = cols, text.col = cols, pch = 16:19,
       bg = "gray95", cex = 1.2)

titanic.mca <- mjca(Titanic)
summary(titanic.mca)
res <- plot(titanic.mca)

res <- plot(titanic.mca, labels=0, pch='.', cex.lab=1.2)
# extract factor names and levels
coords <- data.frame(res$cols, titanic.mca$factors)
cols <- c("blue", "red", "brown", "black")
nlev <- c(4,2,2,2)
points(coords[,1:2], pch=rep(16:19, nlev), col=rep(cols, nlev), cex=1.2)
pos <- c(3,1,1,3)
text(coords[,1:2], labels=coords$level, col=rep(cols, nlev), pos=rep(pos,nlev), cex=1.1, xpd=TRUE)
coords <- coords[ order(coords[,"factor"], coords[,"Dim1"]), ]
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Class", lty=1, lwd=2, col="blue")
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Sex",  lty=1, lwd=2, col="red")
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Age",  lty=1, lwd=2, col="brown")
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Survived",  lty=1, lwd=2, col="black")

legend("topleft", legend=c("Class", "Sex", "Age", "Survived"),
       title="Factor", title.col="black",
       col=cols, text.col=cols, pch=16:19,
       bg="gray95", cex=1.2)

suicide.tab <- xtabs(Freq ~ age_sex + method2, data = Suicide)
suicide.ca <- ca(suicide.tab)

op <- par(cex=1.3, mar=c(4,4,1,1)+.1, lwd=2)
plot(suicide.ca, map = "colgreen", arrows = c(FALSE, TRUE))
par(op)

data("UKSoccer", package = "vcd")
dimnames(UKSoccer) <- list(Home = paste0("H", 0:4),
                           Away = paste0("A", 0:4))

soccer.pca <- prcomp(log(UKSoccer + 1), center = TRUE, scale. = FALSE)

biplot(soccer.pca, scale = 0, var.axes = FALSE,
       col = c("blue", "red"), cex = 1.2, cex.lab = 1.2,
       xlab = "Dimension 1", ylab = "Dimension 2")

# get the row and column scores
rscores <- soccer.pca$x[,1:2]
cscores <- soccer.pca$rotation[,1:2]
# means, excluding A2 and H2
rmean <- colMeans(rscores[-3,])[2]
cmean <- colMeans(cscores[-3,])[1]

abline(h = rmean, col = "blue", lwd = 2)
abline(v = cmean, col = "red", lwd = 2)
abline(h = 0, lty = 3, col = "gray")
abline(v = 0, lty = 3, col = "gray")