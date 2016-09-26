## ---- load_libraries ----
load_file_source <- function(link, local) {
  if (exists("connected")) {
    if (connected) {
      # link
      f <- link
    } else {
      # local file
      f <- local
    }
    source(f)
  } else {
    stop("Set connected state")
  }
}

library(neuralnet)
library(nnet)
library(NeuralNetTools)
library(MASS)
library(ISLR)
library(caTools) # sample.split
library(boot) # cv.glm
library(faraway) # compact lm summary "sumary" function
library(caret) # useful tools for machine learning
library(corrplot)
# location of boiler plate utilities
load_file_source("https://raw.githubusercontent.com/julianhatwell/Utilities/master/Utilities.R"
                 , "C:\\Dev\\Study\\R\\Utilities\\Utilities.R")

# plotting theme
load_file_source("https://raw.githubusercontent.com/julianhatwell/R_Themes/master/BluesGreysTheme.R"
                 , "C:\\Dev\\Study\\R\\R_Themes\\BluesGreysTheme.R")

# boiler plate functions based on caret package
load_file_source("https://raw.githubusercontent.com/julianhatwell/Statistical_Learning_Basics/master/utilityCode.R"
                 , "utilityCode.R")

# boiler plate functions based on caret package
load_file_source("https://raw.githubusercontent.com/julianhatwell/Statistical_Learning_Basics/master/neuralnet_diags.R"
                 , "neuralnet_diags.R")

## ---- Boston_data ----
# Using my standard boiler plate for machine learning
# Run through some standard checks for data quality
dt <- setData(Boston, "medv")

# problem type: either classification or regression
dt$ptype

# check for NA vals
na.vals.check(dt)
# and near zero variance
nzv.check(dt) 
# and highly correlated variables
cor.vars.check(dt, 0.8)
# and linear combinations
lin.comb.check(dt)

# Have a closer look at the tax variable
corrplot(cor(dt$dt.frm))

## ---- glm_fit ----
# use a standard seed throughout
seed.val <- 200

# Train-test random splitting for linear model
set.seed(seed.val)
# sample.split creates a Boolean Vector
Boston.split <- sample.split(Boston$medv
                             , SplitRatio = 0.75)
Boston.train <- Boston[Boston.split, ]
Boston.test <- Boston[!Boston.split, ]

# Fitting linear model using both lm and glm functions
# lm function will give a more useful summary
# glm function allows me to use boot::cv.glm for further anaylisis
# lm and glm yield exactly the same coefficients
# models use all predictors, as I'll do with neuralnet later
Boston.glm <- glm(medv~., data=Boston.train)
Boston.lm <- lm(medv~., data=Boston.train)
sumary(Boston.lm)

## ---- neuralnet_fit ----
# The predictor vars must be scaled data for the ANN fitting
Boston.scaled <- as.data.frame(scale(Boston))
min.medv <- min(Boston$medv)
max.medv <- max(Boston$medv)
# response var must be scaled to [0 < resp < 1]
Boston.scaled$medv <- scale(Boston$medv
                    , center = min.medv
                    , scale = max.medv - min.medv)

# Train-test split
Boston.train.scaled <- Boston.scaled[Boston.split, ]
Boston.test.scaled <- Boston.scaled[!Boston.split, ]

# neuralnet doesn't accept resp~. (dot) notation
# so a utility function to create a verbose formula is used
Boston.nn.fmla <- generate.full.fmla("medv", Boston)

# 2 models, one with 2 layers of 5 and 3
# the second with one layer of 8
# linear output is used for a regression problem
Boston.nn.5.3 <- neuralnet(Boston.nn.fmla
                , data=Boston.train.scaled
                , hidden=c(5,3)
                , linear.output=TRUE)

Boston.nn.8 <- neuralnet(Boston.nn.fmla
                , data=Boston.train.scaled
                , hidden=8
                , linear.output=TRUE)

## ---- performance_metrics ----
# Predict - remember the output is scaled
Boston.5.3.preds.scaled <- neuralnet::compute(Boston.nn.5.3
                                      , Boston.test.scaled[,1:13])
Boston.8.preds.scaled <- neuralnet::compute(Boston.nn.8
                                      , Boston.test.scaled[,1:13])
# Results from NN are normalized (scaled)
# unscale the response to compensate for rounding errors
Boston.medv.unscaled <- (Boston.test.scaled$medv) * (max.medv - min.medv) + min.medv

# Descaling for comparison
Boston.5.3.preds <- Boston.5.3.preds.scaled$net.result * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)
Boston.8.preds <- Boston.8.preds.scaled$net.result * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)

# Calculating MSE
Boston.5.3.rm <- reg.measures(Boston.5.3.preds, Boston.medv.unscaled)
Boston.8.rm <- reg.measures(Boston.8.preds, Boston.medv.unscaled)

# Predicted data from lm
Boston.glm.preds <- predict(Boston.glm
                            , newdata = Boston.test)
Boston.glm.rm <- reg.measures(Boston.glm.preds, Boston.test$medv)

# Compare the metrics
print("OLS regression measures")
Boston.glm.rm
print("NN.5.3 regression measures")
Boston.5.3.rm
print("NN.8 regression measures")
Boston.8.rm # this model seems to have done slightly better

# Also prepare objects using my custom tools for later
B.nn.5.3.diag <- neuralnet.diagnostics(Boston.nn.5.3)
B.nn.8.diag <- neuralnet.diagnostics(Boston.nn.8)

## ---- nn_model_default_plots ----

# Visual plot of the model
# this plot function behaves very strangely
# and doesn't appear to be compatible with knitr
# have exported manually for the report
plot(Boston.nn.5.3)
plot(Boston.nn.8)

## ---- nn_model_NNTools_plots ----
# NeuralNetTools
plotnet(Boston.nn.5.3)
plotnet(Boston.nn.8)

## ---- nn_model_NNTools_varimp ----
# garson function on two hidden layer model returns an error
tryCatch.W.E(garson(Boston.nn.5.3))
# works fine on single hidden layer model
garson(Boston.nn.8)

## ---- nn_model_custom_varimp_w ----
nn.varimp.w.plot(B.nn.5.3.diag)
nn.varimp.w.plot(B.nn.5.3.diag, weight = "f")

nn.varimp.w.plot(B.nn.8.diag)
nn.varimp.w.plot(B.nn.8.diag, weight = "f")


## ---- nn_model_varimp_w_analyisis ----
# weights from chas input
B.nn.8.diag$ptrons[[1]][4,] # layer 1, ptron 4
# weights from dis input
B.nn.8.diag$ptrons[[1]][8,] # layer 1, ptron 8
# weights from layer 2
B.nn.8.diag$ptrons[[2]][8] # layer 3, ptron 8
B.nn.8.diag$ptrons[[2]][c(1, 2, 4, 7)] # layer 2, ptrons 1, 2, 4 and 7

## ---- nn_model_NNTools_profile ----
# not working
# expected to fail
tryCatch.W.E(lekprofile(Boston.nn.5.3)) # ex
# something wrong as there is one hidden layer
tryCatch.W.E(lekprofile(Boston.nn.8))

## ---- nn_model_confidence ----
# not working
# expected to fail
tryCatch.W.E(confidence.interval(Boston.nn.5.3)) # ex
# something wrong as there is one hidden layer
tryCatch.W.E(confidence.interval(Boston.nn.8))

## ---- nn_model_custom_profile ----
# full plot
nn.profile.plot(B.nn.5.3.diag)
nn.profile.plot(B.nn.8.diag)

## ---- nn_model_varimp_p ----
nn.varimp.p.plot(B.nn.5.3.diag)
nn.varimp.p.plot(B.nn.8.diag)

## ---- nn_model_custom_profile_indiv ----
# individual effect plots
nn.profile.plot(B.nn.5.3.diag, "rm")
nn.profile.plot(B.nn.5.3.diag, "crim")
nn.profile.plot(B.nn.5.3.diag, "lstat")
nn.profile.plot(B.nn.5.3.diag, "chas")
nn.profile.plot(B.nn.5.3.diag, "zn")

nn.profile.plot(B.nn.8.diag, "dis")
nn.profile.plot(B.nn.8.diag, "rm")
nn.profile.plot(B.nn.8.diag, "nox")
nn.profile.plot(B.nn.8.diag, "crim")
nn.profile.plot(B.nn.8.diag, "rad")

## ---- nn_gwplot ----
gwplot(Boston.nn.8, selected.covariate = "dis")

# ---- pred_fitted_plots ----
# Plot real vs predictions
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

# ---- pred_resid_plots ----
# Plot fitted vs residual
par(mfrow=c(1,3))
plot(Boston.5.3.preds
     , Boston.5.3.preds - Boston.test$medv
     , col="red"
     , main="Fitted vs residual NN\ntwo hidden layers"
     , pch=18
     , cex=0.7
     , xlab = "Predicted Value"
     , ylab = "Residual")
abline(0,0,lwd=2)
legend("bottomright",legend="NN.5.3"
       ,pch=18,col="red", bty="n")

plot(Boston.8.preds
     , Boston.8.preds - Boston.test$medv
     , col="green"
     , main="Fitted vs residual NN\none hidden layer"
     , pch=18
     , cex=0.7
     , xlab = "Predicted Value"
     , ylab = "Residual")
abline(0,0,lwd=2)
legend("bottomright",legend="NN.8",pch=18,col="green", bty="n")

plot(Boston.glm.preds
     , Boston.glm.preds - Boston.test$medv
     , col="blue"
     , main="Fitted vs residual lm"
     , pch=18
     , cex=0.7
     , xlab = "Predicted Value"
     , ylab = "Residual")
abline(0,0,lwd=2)
legend("bottomright",legend="LM",pch=18,col="blue", bty="n", cex=.95)
par(mfrow = c(1, 1))

## ---- cross_validation_lm ----
# Linear model cross validation
# same seed value each time
seed.val <- 2016
Boston.glm.full <- glm(medv~.,data=Boston) # full data set

set.seed(seed.val)
Boston.cv.RMSE <- cv.glm(Boston, Boston.glm.full
       , cost = RMSE
       , K=10)$delta[1]

set.seed(seed.val)
Boston.cv.MAD <- cv.glm(Boston, Boston.glm.full
       , cost = MAD
       , K=10)$delta[1]
# delta[1] is unadjusted for fair comparison
Boston.cv.RMSE
Boston.cv.MAD

## ---- cross_validation_nn ----
set.seed(seed.val)
k <- 10
cv.error <- matrix(nrow = k, ncol = 4)

folds <- sample(1:k, nrow(Boston)
                , replace = TRUE)

for(i in 1:k){
  Boston.train.cv <- Boston.scaled[folds == i,]
  Boston.test.cv <- Boston.scaled[folds != i,]
  
  nn.5.3 <- neuralnet(Boston.nn.fmla
                  , data=Boston.train.cv
                  , hidden=c(5,3)
                  , linear.output=TRUE)

  nn.8 <- neuralnet(Boston.nn.fmla
                  , data=Boston.train.cv
                  , hidden=8
                  , linear.output=TRUE)

  Boston.5.3.preds.scaled <- neuralnet::compute(nn.5.3, Boston.test.cv[, 1:13])
  Boston.8.preds.scaled <- neuralnet::compute(nn.8, Boston.test.cv[, 1:13])

  Boston.5.3.preds <- Boston.5.3.preds.scaled$net.result * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)
  Boston.8.preds <- Boston.8.preds.scaled$net.result * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)
  
  medv.unscaled <- (Boston.test.cv$medv) * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)
  
  cv.error[i, ] <- c(
    RMSE(medv.unscaled, Boston.5.3.preds)
    , MAD(medv.unscaled, Boston.5.3.preds)
    , RMSE(medv.unscaled, Boston.8.preds)
    , MAD(medv.unscaled, Boston.8.preds)
    )
}

# MSE vector from CV
cv.error

# Average MSE
Boston.5.3.RMSE <- colMeans(cv.error)[1]
Boston.5.3.MAD <- colMeans(cv.error)[2]
Boston.8.RMSE <- colMeans(cv.error)[3]
Boston.8.MAD <- colMeans(cv.error)[4]

## ---- cross_validation_results_plot ----
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

## ---- cross_validation_results_ttest ----
t.test(cv.error[,1], cv.error[,3]) # RMSE
t.test(cv.error[,2], cv.error[,4]) # MAD
# MAD shows signif difference
# shows majority of errors are within a smaller bound
# a few poor error predictions