library(ISLR)
nci.labs <- NCI60$labs
nci.data <- NCI60$data

pr.out=prcomp(nci.data , scale=TRUE)

# assigns a colour to each of the cell lines

Cols <- function(vec) {
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

# plot the first 3 components
par(mfrow=c(1,2))
plot(pr.out$x[,1:2]
     , col=Cols(nci.labs)
     , pch=19, xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)]
     , col=Cols(nci.labs)
     , pch=19, xlab="Z1",ylab="Z3")

summary(pr.out)
par(mfrow = c(1,1))
plot(pr.out)

# scree splot
pve <- 100*pr.out$sdev^2/sum(pr.out$sdev^2)

par(mfrow = c(1,2))
plot(pve, type="o", ylab="PVE"
     , xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE"
     , xlab="Principal Component", col="brown3")
par(mfrow = c(1,1))

summary(pr.out)$importance[2,]
summary(pr.out)$importance[3,]

sd.data=scale(nci.data)

par(mfrow=c(3,1))
data.dist=dist(sd.data)
plot(hclust(data.dist)
     , labels=nci.labs
     , main="Complete Linkage"
     , xlab="", sub="",ylab="")
plot(hclust(data.dist , method ="average")
     , labels=nci.labs , main="Average Linkage"
     , xlab="", sub="",ylab="")
plot(hclust(data.dist , method ="single")
     , labels=nci.labs, main="Single Linkage"
     , xlab="", sub="",ylab="")
par(mfrow = c(1,1))

hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out ,4)
table(hc.clusters ,nci.labs)

plot(hc.out, labels=nci.labs)
abline(h=139, col="red")

load("10.R.RData")
q10 <- prcomp(rbind(x, x.test), scale = TRUE)
pve <- q10$sdev^2/sum(q10$sdev^2)
cumsum(pve[1:5])


q10 <- prcomp(x, scale = TRUE)
lm.q10 <- lm(y~., data = q10$x[1:300,1:5])
pred <- predict(q10, x.test)
mean((pred - y.test)^2)

q10.lm <- lm(y~., data = x)
pred <- predict(q10.lm, x.test)
mean((pred - y.test)^2)