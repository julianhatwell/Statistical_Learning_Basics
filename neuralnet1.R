rm(list = ls())
library(neuralnet)
library(nnet)
library(NeuralNetTools)
library(MASS)
library(ISLR)
library(caTools) # split.sample
library(boot) # cv.glm
library(plyr) # for a progress bar

# ----- Utility Funcs -----
# neuralnet library needs specific setup
neuralnet.fmla <- function(resp, dt) {
  r <- resp
  n <- names(dt)
  f <- as.formula(paste(r, "~", paste(n[!n %in% r], collapse = " + ")))
  f
}


MSE <- function(pred, act) {
  sum((pred - act)^2)/length(pred)
} # mean squared error

MAD <- function(pred, act) {
  median(abs(pred - act))
} # absolute median deviation

RMSE <- function(pred, act) {
  sqrt(sum((pred - act)^2)/length(pred))
} # route mean squared error

reg.measures <- function(pred, act) {
  MSE.measure <- MSE(pred, act)
  RMSE.measure <- RMSE(pred, act)
  MAD.measure <- MAD(pred, act)
  data.frame(MSE.measure, RMSE.measure, MAD.measure)
}

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
reg.measures(Boston.glm.preds, Boston.test$medv)

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

Boston.nn.fmla <- neuralnet.fmla("medv", Boston)
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

# Visual plot of the model
plot(Boston.nn.5.3)
plot(Boston.nn.8)
# NeuralNetTools
plotnet(Boston.nn.5.3)
plotnet(Boston.nn.8)
garson(Boston.nn.8)
lekprofile(Boston.nn.8) # something wrong as there is one hidden layer


# Predict - remember the output is scaled
pr.nn.scaled <- compute(nn, test.scaled[,1:13])
# Predict - remember the output is scaled
pr.nn.s.scaled <- compute(nn.s, test.scaled[,1:13])

# Results from NN are normalized (scaled)
# unscale the response to compensate for rounding errors
medv.unscaled <- (test.scaled$medv) * (max(Boston$medv) - min(Boston$medv))+min(Boston$medv)

# Descaling for comparison
pr.nn <- pr.nn.scaled$net.result * (max(Boston$medv) - min(Boston$medv))+min(Boston$medv)
pr.nn.s <- pr.nn.s.scaled$net.result * (max(Boston$medv) - min(Boston$medv))+min(Boston$medv)

# Calculating MSE
MSE.nn <- sum((medv.unscaled - pr.nn)^2)/length(medv.unscaled)
MSE.nn.s <- sum((medv.unscaled - pr.nn.s)^2)/length(medv.unscaled)

# Compare the two MSEs
MSE.glm
MSE.nn
MSE.nn.s

# Plot predictions
par(mfrow=c(1,3))
plot(test$medv,pr.nn,col='red'
     , main='Real vs predicted NN\ntwo hidden layers'
     , pch=18
     , cex=0.7
     , xlab = "Actual Median Value"
     , ylab = "Predicted Median Value")
abline(0,1,lwd=2)
legend('bottomright',legend='NN'
       ,pch=18,col='red', bty='n')

plot(test$medv,pr.nn.s,col='green'
     , main='Real vs predicted NN\nsingle hidden layer'
     , pch=18
     , cex=0.7
     , xlab = "Actual Median Value"
     , ylab = "Predicted Median Value")
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='green', bty='n')

plot(test$medv,pr.glm,col='blue'
     , main='Real vs predicted lm'
     , pch=18
     , cex=0.7
     , xlab = "Actual Median Value"
     , ylab = "Predicted Median Value")
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

par(mfrow = c(1, 1))

# Compare predictions on the same plot
plot(test$medv
     , pr.nn
     , col='red'
     , main='Real vs Predicted'
     , pch=18,cex=0.7)
points(test$medv
       , pr.nn.s
       , col='green'
       , pch=18
       , cex=0.7)
points(test$medv
       , pr.glm
       , col='blue'
       , pch=18
       , cex=0.7)
abline(0,1,lwd=2)
legend('bottomright'
       , legend=c('NN','NN.s','LM')
       , pch=18
       , col=c('red','green','blue'))

# Cross validating
# Linear model cross validation
set.seed(200)
glm.fit <- glm(medv~.,data=Boston) # full data set
cv.glm(Boston, glm.fit
       , K=10)$delta[1]
# delta[1] is unadjusted for fair comparison

# Neural net cross validation
set.seed(450)
k <- 10
cv.error <- matrix(nrow = k, ncol = 2)

# Initialize progress bar
pbar <- create_progress_bar('text')
pbar$init(k)

folds <- sample(1:k, nrow(Boston)
                , replace = TRUE)

for(i in 1:k){
  train.cv <- scaled[folds == i,]
  test.cv <- scaled[folds != i,]
  
  nn <- neuralnet(f
                  , data=train.cv
                  , hidden=c(5,2)
                  , linear.output=T)

  nn.s <- neuralnet(f
                  , data=train.cv
                  , hidden=8
                  , linear.output=T)
  
    
  pr.nn.scaled <- compute(nn, test.cv[, 1:13])
  pr.nn.s.scaled <- compute(nn.s, test.cv[, 1:13])

  pr.nn <- pr.nn.scaled$net.result * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)
  pr.nn.s <- pr.nn.s.scaled$net.result * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)  
  
  medv.unscaled <- (test.cv$medv) * (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)
  
  cv.error[i, ] <- c(
    sum((medv.unscaled - pr.nn)^2)/length(medv.unscaled)
    , sum((medv.unscaled - pr.nn.s)^2)/length(medv.unscaled)
    )
  
  pbar$step()
}

# MSE vector from CV
cv.error

# Average MSE
colMeans(cv.error)

# Visual plot of CV results
boxplot(cv.error
        , xlab='MSE CV'
        , col='cyan'
        , border='blue'
        , main='CV error (MSE) for NN'
        , names = c("2h model", "1h model")
        , horizontal=TRUE)
# overall 2 h model more stable and lower MSE
# despite early (single run results above)

print(head(College,2))

# Create Vector of Column Max and Min Values
maxs <- apply(College[,2:18], 2, max)
mins <- apply(College[,2:18], 2, min)
# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(College[,2:18],center = mins, scale = maxs - mins))
# Check results
print(head(scaled.data,2))

# Convert Private column from Yes/No to 1/0
Private = as.numeric(College$Private)-1
dt = cbind(Private,scaled.data)

# Create Split (any column is fine)
set.seed(101)
split = sample.split(dt$Private, SplitRatio = 0.70)
# Split based off of split Boolean Vector
train = subset(dt, split == TRUE)
test = subset(dt, split == FALSE)

# create formula for neuralnet (does not accept y~.)
r <- "Private"
n <- names(scaled.data)
f <- as.formula(paste(r, "~", paste(n[!n %in% r], collapse = " + ")))

# train
nn <- neuralnet(f, train
                , hidden=c(10,10,10)
                , linear.output=FALSE)

# Compute Predictions off Test Set
predicted.nn.values <- compute(nn,test[2:18])
pred.nn <- sapply(predicted.nn.values$net.result
                  , function(x) {
                    ifelse(x > 0.5, 1, 0)
                  })

table(pred = pred.nn, actual = test$Private)


# infert example
nn <- neuralnet(case~age+
                  parity+induced+spontaneous
                , data=infert
                , hidden=2
                , err.fct="ce"
                , linear.output=FALSE)

nn$result.matrix
plot(nn)