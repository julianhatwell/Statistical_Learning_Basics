library(neuralnet)
library(nnet)
library(NeuralNetTools)
library(MASS)
library(ISLR)
library(caTools) # split.sample
library(boot) # cv.glm
library(plyr) # for a progress bar
# link
# utils <- "https://raw.githubusercontent.com/julianhatwell/Utilities/master/Utilities.R"
# local file
utils <- "C:\\Dev\\Study\\R\\Utilities\\Utilities.R"
source(utils)

# ----- Boston Housing Example -----
# Check that no data is missing
apply(Boston, 2, function(x) sum(is.na(x)))

# Train-test random splitting for linear model
set.seed(500)
# sample.split creates a Boolean Vector
Boston.split = sample.split(Boston$medv
                            , SplitRatio = 0.75)
Boston.train <- Boston[Boston.split, ]
Boston.test <- Boston[!Boston.split, ]

# Fitting linear model using glm function
# so boot::cv.glm can be used
Boston.glm <- glm(medv~., data=Boston.train)
summary(Boston.glm)

# Predicted data from lm
Boston.glm.preds <- predict(Boston.glm
                            , newdata = Boston.test)

# Test Accuracy Measures
Boston.glm.rm <- reg.measures(Boston.glm.preds, Boston.test$medv)
Boston.glm.rm

# Neural net fitting
# Scaling data for the NN
# This routine gives values between 0, 1
maxs <- apply(Boston, 2, max) 
mins <- apply(Boston, 2, min)
Boston.scaled <- as.data.frame(scale(Boston
                              , center = mins
                              , scale = maxs - mins))

# Train-test split
Boston.train.scaled <- Boston.scaled[Boston.split, ]
Boston.test.scaled <- Boston.scaled[!Boston.split, ]

Boston.nn.fmla <- generate.full.fmla("medv", Boston)

Boston.nn.5.3 <- neuralnet(Boston.nn.fmla
                , data=Boston.train.scaled
                , hidden=c(5,3)
                , linear.output=T)

# NN single hidden layer for comparison
# NeuralNetTools don't work on multiple hidden layers
Boston.nn.8 <- neuralnet(Boston.nn.fmla
                , data=Boston.train.scaled
                , hidden=8
                , linear.output=T)

# Predict - remember the output is scaled
Boston.5.3.preds.scaled <- compute(Boston.nn.5.3
                                      , Boston.test.scaled[,1:13])
Boston.8.preds.scaled <- compute(Boston.nn.8
                                      , Boston.test.scaled[,1:13])
# Results from NN are normalized (scaled)
# unscale the response to compensate for rounding errors
Boston.medv.unscaled <- (Boston.test.scaled$medv) * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)

# Descaling for comparison
Boston.5.3.preds <- Boston.5.3.preds.scaled$net.result * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)
Boston.8.preds <- Boston.8.preds.scaled$net.result * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)

# Calculating MSE
Boston.5.3.rm <- reg.measures(Boston.5.3.preds, Boston.medv.unscaled)
Boston.8.rm <- reg.measures(Boston.8.preds, Boston.medv.unscaled)

# Compare the metrics
Boston.glm.rm
Boston.5.3.rm
Boston.8.rm # this model seems to have done slightly better

# Visual plot of the model
plot(Boston.nn.5.3)
plot(Boston.nn.8)
# NeuralNetTools
plotnet(Boston.nn.5.3)
plotnet(Boston.nn.8)
garson(Boston.nn.8)
lekprofile(Boston.nn.8) # something wrong as there is one hidden layer

# My custom tools
B.nn.5.3.diag <- neuralnet.diagnostics(Boston.nn.5.3)
B.nn.8.diag <- neuralnet.diagnostics(Boston.nn.8)

# can highlight vars of interest
nn.varimp.p.plot(B.nn.5.3.diag)
nn.varimp.p.plot(B.nn.8.diag)

# can highlight vars of interest
nn.varimp.w.plot(B.nn.5.3.diag)
nn.varimp.w.plot(B.nn.8.diag)

# individual effect plots
nn.profile.plot(B.nn.5.3.diag, "crim")
nn.profile.plot(B.nn.5.3.diag, "dis")
nn.profile.plot(B.nn.5.3.diag, "rm")
nn.profile.plot(B.nn.8.diag, "crim")
nn.profile.plot(B.nn.8.diag, "dis")
nn.profile.plot(B.nn.8.diag, "rm")

# Plot predictions
par(mfrow=c(1,3))
plot(Boston.test$medv
     , Boston.5.3.preds
     , col="red"
     , main="Real vs predicted NN\ntwo hidden layers"
     , pch=18
     , cex=0.7
     , xlab = "Actual Median Value"
     , ylab = "Predicted Median Value")
abline(0,1,lwd=2)
legend("bottomright",legend="NN.5.3"
       ,pch=18,col="red", bty="n")

plot(Boston.test$medv
     , Boston.8.preds
     , col="green"
     , main="Real vs predicted NN\nsingle hidden layer"
     , pch=18
     , cex=0.7
     , xlab = "Actual Median Value"
     , ylab = "Predicted Median Value")
abline(0,1,lwd=2)
legend("bottomright",legend="NN.8",pch=18,col="green", bty="n")

plot(Boston.test$medv
     , Boston.glm.preds
     , col="blue"
     , main="Real vs predicted lm"
     , pch=18
     , cex=0.7
     , xlab = "Actual Median Value"
     , ylab = "Predicted Median Value")
abline(0,1,lwd=2)
legend("bottomright",legend="LM",pch=18,col="blue", bty="n", cex=.95)

par(mfrow = c(1, 1))

# Compare predictions on the same plot
plot(Boston.test$medv
     , Boston.5.3.preds
     , col="red"
     , main="Real vs Predicted"
     , pch=18,cex=0.7)
points(Boston.test$medv
       , Boston.8.preds
       , col="green"
       , pch=18
       , cex=0.7)
points(Boston.test$medv
       , Boston.glm.preds
       , col="blue"
       , pch=18
       , cex=0.7)
abline(0,1,lwd=2)
legend("bottomright"
       , legend=c("NN.5.3","NN.8","LM")
       , pch=18
       , col=c("red","green","blue"))

# Cross validating
# Linear model cross validation
seed.val <- 200
set.seed(seed.val)

Boston.glm.full <- glm(medv~.,data=Boston) # full data set
Boston.cv.RMSE <- cv.glm(Boston, Boston.glm.full
       , cost = RMSE
       , K=10)$delta[1]
Boston.cv.MAD <- cv.glm(Boston, Boston.glm.full
       , cost = MAD
       , K=10)$delta[1]
# delta[1] is unadjusted for fair comparison

# Neural net cross validation
set.seed(seed.val)
k <- 10
cv.error <- matrix(nrow = k, ncol = 4)

# Initialize progress bar
pbar <- create_progress_bar('text')
pbar$init(k)

folds <- sample(1:k, nrow(Boston)
                , replace = TRUE)

for(i in 1:k){
  Boston.train.cv <- Boston.scaled[folds == i,]
  Boston.test.cv <- Boston.scaled[folds != i,]
  
  nn.5.3 <- neuralnet(Boston.nn.fmla
                  , data=Boston.train.cv
                  , hidden=c(5,3)
                  , linear.output=T)

  nn.8 <- neuralnet(Boston.nn.fmla
                  , data=Boston.train.cv
                  , hidden=8
                  , linear.output=T)

  Boston.5.3.preds.scaled <- compute(nn.5.3, Boston.test.cv[, 1:13])
  Boston.8.preds.scaled <- compute(nn.8, Boston.test.cv[, 1:13])

  Boston.5.3.preds <- Boston.5.3.preds.scaled$net.result * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)
  Boston.8.preds <- Boston.8.preds.scaled$net.result * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)
  
  medv.unscaled <- (Boston.test.cv$medv) * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)
  
  cv.error[i, ] <- c(
    RMSE(medv.unscaled, Boston.5.3.preds)
    , MAD(medv.unscaled, Boston.5.3.preds)
    , RMSE(medv.unscaled, Boston.8.preds)
    , MAD(medv.unscaled, Boston.8.preds)
    )
  
  pbar$step()
}

# MSE vector from CV
cv.error

# Average MSE
Boston.5.3.RMSE <- colMeans(cv.error)[1]
Boston.5.3.MAD <- colMeans(cv.error)[2]
Boston.8.RMSE <- colMeans(cv.error)[3]
Boston.8.MAD <- colMeans(cv.error)[4]


# Visual plot of CV results
boxplot(cv.error
        , xlab="CV Error"
        , col="cyan"
        , border="blue"
        , main="CV error (RMSE and MAD) for Neural Net Models"
        , names = paste0(rep(c("2h model\n", "1h model\n"), each = 2)
                        , c("RMSE", "MAD"))
        , horizontal=TRUE)

# overall 2 h model more stable and lower RMSE
# despite early (single run results above)

t.test(cv.error[,1], cv.error[,3]) # RMSE
t.test(cv.error[,2], cv.error[,4]) # MAD
# MAD shows signif difference
# shows majority of errors are within a smaller bound
# a few poor error predictions