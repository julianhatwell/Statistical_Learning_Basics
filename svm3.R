library(lattice)
library(gridExtra)
library(e1071)
set.seed(10)
x3.1 <- x <- runif(60, -1.5, 1.5)
x4.2 <- -(x3.1^4 + rnorm(60, mean = -7.5, sd = 0.5))
set4 <- cbind(x3.1 * 2 + 7, x4.2)


xban <- rbind(set3, set4)
xban <- data.frame(xban)
names(xban) <- c("x", "y")
xban$z <- matrix(c(rep(1,60),rep(-1,60)))

svm.fit <- svm(z~., data = xban, type = "C", kernel = "rad", epsilon = 0.001)

predmat <- expand.grid(x = seq(0, 10, length.out = 1001), y = seq(0, 10, length.out = 1001))
predmat$z <- predict(svm.fit, newdata = predmat)


plot1 <- contourplot(z~x*y, predmat, alpha = 0.1, labels = FALSE
            , main = "SVM Decision Boundary"
            , xlim = c(-0.5, 10.5)
            , ylim = c(-0.5, 10.5)) +
  as.layer(xyplot(y~x, xban, col = xban$z + 3, pch = xban$z + 3))
plot2 <- xyplot(y~x, xban, col = xban$z + 3, pch = xban$z + 3, type = c("p", "r")
     , panel = function(x,
                        y, ...) {
       panel.xyplot(x, y, ...)
       panel.lmline(x, y, col = "black")
     }
      , main = "Linear Decision Boundary"
      , xlim = c(-0.5, 10.5)
      , ylim = c(-0.5, 10.5))
grid.arrange(plot1, plot2, ncol = 2)

# export width 1000, height 550
