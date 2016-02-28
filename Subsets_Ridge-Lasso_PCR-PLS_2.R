library(ISLR)

# Question 9
set.seed(1001001)
summary(College)
train <- sample(dim(College)[1], dim(College)[1] * 0.8, replace = FALSE)
  
# lm
fit.lm <- lm(Apps~., data = College[train,])
pred.lm <- predict(fit.lm, newdata = College[-train,])
mean((pred.lm - College$Apps[-train])^2) #MSE
sqrt(mean((pred.lm - College$Apps[-train])^2)) #RMSE

# First create the data arguments for glmnet
library(glmnet)
x <- model.matrix(Apps~.-1, data = College)
y <- College$Apps

cv.out.ridge <- cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out.ridge)
bestlam.ridge <- cv.out.ridge$lambda.min
bestlam.ridge
selam.ridge <- cv.out.ridge$lambda.1se
selam.ridge

# Setting up for ridge (alpha = 0)
fit.ridge <- glmnet(x[train,],y[train],alpha=0)
plot(fit.ridge, xvar = "lambda")

# MSE associated with the best value of lambda
ridge.pred.best <- predict(fit.ridge, s=bestlam.ridge, newx = x[-train,])
mean((ridge.pred.best-y[-train])^2) #MSE
sqrt(mean((ridge.pred.best-y[-train])^2)) #RMSE
round(predict(fit.ridge, s=bestlam.ridge
              , type = "coefficients"),4)

# MSE associated with the good enough value of lambda
ridge.pred.se <- predict(fit.ridge, s=selam.ridge, newx = x[-train,])
mean((ridge.pred.se-y[-train])^2) #MSE
sqrt(mean((ridge.pred.se-y[-train])^2)) #RMSE
round(predict(fit.ridge, s=selam.ridge
              , type = "coefficients"),4)

# get the MSE/RMSE for the full data set on this value of lambda
fit.ridge.full <- glmnet(x,y,alpha=0, lambda = bestlam.ridge)
ridge.pred.full <- predict(fit.ridge.full, x)
mean((ridge.pred.full-y)^2) #MSE
sqrt(mean((ridge.pred.full-y)^2)) #RMSE

# Now a lasso (alpha = 1)
cv.out.lasso <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out.lasso)
bestlam.lasso <- cv.out.lasso$lambda.min
bestlam.lasso
selam.lasso <- cv.out.lasso$lambda.1se
selam.lasso

fit.lasso <- glmnet(x[train,],y[train],alpha=1)
plot(fit.lasso, xvar = "lambda")

# MSE associated with the best value of lambda
lasso.pred.best <- predict(fit.lasso, s=bestlam.lasso, newx = x[-train,])
mean((lasso.pred.best-y[-train])^2) #MSE
sqrt(mean((lasso.pred.best-y[-train])^2)) #RMSE
round(predict(fit.lasso, s=bestlam.lasso
              , type = "coefficients"),4)

# MSE associated with the good enough value of lambda
lasso.pred.se <- predict(fit.lasso, s=selam.lasso, newx = x[-train,])
mean((lasso.pred.se-y[-train])^2) #MSE
sqrt(mean((lasso.pred.se-y[-train])^2)) #RMSE
round(predict(fit.lasso, s=selam.lasso
              , type = "coefficients"),4)

# get the MSE/RMSE for the full data set on this value of lambda
fit.lasso.full <- glmnet(x,y,alpha=1, lambda = bestlam.lasso)
lasso.pred.full <- predict(fit.lasso.full, x)
mean((lasso.pred.full-y)^2) #MSE
sqrt(mean((lasso.pred.full-y)^2)) #RMSE

# Now with PCR
# Fitting with validation = CV 
# uses a built in cross validation 
# and finds the best number of components.

library(pls)
set.seed(101)
fit.pcr <- pcr(Apps~., data = College, subset = train, scale = TRUE, validation = "CV")
summary(fit.pcr)
validationplot(fit.pcr, val.type = "MSEP")

# from the results could take a model with 2, 5 or 9 comps
pred.pcr2 <- predict(fit.pcr, College[-train,], ncomp = 2)
pred.pcr5 <- predict(fit.pcr, College[-train,], ncomp = 5)
pred.pcr9 <- predict(fit.pcr, College[-train,], ncomp = 9)

mean((pred.pcr2-College$Apps[-train])^2)
sqrt(mean((pred.pcr2-College$Apps[-train])^2))

mean((pred.pcr5-College$Apps[-train])^2)
sqrt(mean((pred.pcr5-College$Apps[-train])^2))

mean((pred.pcr9-College$Apps[-train])^2)
sqrt(mean((pred.pcr9-College$Apps[-train])^2))

# 9 is the best and in range of other approaches. 
# Refit the model with 9 comps on all the data
fit.pcr.full <- pcr(Apps~., data = College, nrcomp = 9)
summary(fit.pcr.full)
pred.pcr.full <- predict(fit.pcr.full, College)
mean((pred.pcr.full-College$Apps)^2)
sqrt(mean((pred.pcr.full-College$Apps)^2))

# Now PLS
set.seed(101)
fit.pls <- plsr(Apps~., data = College, subset = train, scale = TRUE, validation = "CV")
summary(fit.pls)
validationplot(fit.pls, val.type = "MSEP")

# best model 6
pred.pls6 <- predict(fit.pls, College[-train,], ncomp = 6)
mean((pred.pls6-College$Apps[-train])^2)
sqrt(mean((pred.pls6-College$Apps[-train])^2))

# refit model 6 on whole set
fit.pls.full <- plsr(Apps~., data = College, ncomp = 6)
pred.pls.full <- predict(fit.pls.full, College)
mean((pred.pls.full-College$Apps)^2)
sqrt(mean((pred.pls.full-College$Apps)^2))

# To compare the results obtained above, 
# we have to compute the test R2R2 for all models.

test.avg <- mean(College$Apps[-train])
lm.r2 <- 1 - mean((pred.lm - College$Apps[-train])^2) / mean((test.avg - College$Apps[-train])^2)
ridge.r2 <- 1 - mean((ridge.pred.best - College$Apps[-train])^2) / mean((test.avg - College$Apps[-train])^2)
lasso.r2 <- 1 - mean((lasso.pred.best - College$Apps[-train])^2) / mean((test.avg - College$Apps[-train])^2)
pcr.r2 <- 1 - mean((pred.pcr9 - College$Apps[-train])^2) / mean((test.avg - College$Apps[-train])^2)
pls.r2 <- 1 - mean((pred.pls6 - College$Apps[-train])^2) / mean((test.avg - College$Apps[-train])^2)

lm.r2
ridge.r2
lasso.r2
pcr.r2
pls.r2

# Question 10
set.seed(101101)

# data simulation, 20 predictors, some zero coefs
X <- matrix(NA, 1000, 20)
colnames(X) <- paste0("X",1:20)
for(i in 1:20) {
  X[,i] <- rnorm(1000, mean = i, sd = sample(1:20)/sample(1:20))  
}
beta <- c(1,2,3,4,5,1/6,1/7,1/8,1/9,-1,-1.1,-1.2,-1.3,0,0,0,0,0,0,0)
eps <- rnorm(1000, sd = 0.1)
y <- X %*% beta + eps 

train <- sample(1000, 900)
test <- (1:1000)[-train]

data.train <- data.frame(y = y, X)[train,]
train.mat <- model.matrix(y ~ ., data = data.train, nvmax = 20)

data.test <- data.frame(y = y, X)[test,]
test.mat <- model.matrix(y ~ ., data = data.test, nvmax = 20)

# fit best subset
library(leaps)
fit.bss <- regsubsets(y~., data=data.train, nvmax = 20)
fit.bss.smry <- summary(fit.bss)
fit.bss.smry
plot(fit.bss.smry$rss ,xlab="Number of Variables ",ylab="RSS", type= "b")
plot(fit.bss.smry$cp ,xlab="Number of Variables ",ylab="Cp", type= "b")
plot(fit.bss.smry$adjr2 ,xlab="Number of Variables ",ylab="Adjusted R2", type= "b")
plot(fit.bss.smry$bic ,xlab="Number of Variables ",ylab="bic", type= "b")

which.min(fit.bss.smry$cp)
which.max(fit.bss.smry$adjr2)
which.min(fit.bss.smry$bic)

# report the training MSE for each model size
val.errors <- rep(NA, 20)
for (i in 1:20) {
  coefi <- coef(fit.bss, id = i)
  pred <- train.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((pred - y[train])^2)
}
plot(val.errors, xlab = "Number of predictors"
     , ylab = "Training MSE", pch = 19, type = "b")

which.min(val.errors)

# report the test MSE for each model size
val.errors <- rep(NA, 20)
for (i in 1:20) {
  coefi <- coef(fit.bss, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((pred - y[test])^2)
}
plot(val.errors, xlab = "Number of predictors"
     , ylab = "Test MSE", pch = 19, type = "b")

which.min(val.errors)
coef(fit.bss, id = which.min(val.errors))

# plot root squared error of coef estimates
colnum <- function(c) {
  as.integer(sub("X","", c))
}
val.errors <- rep(NA, 20)
for (i in 1:20) {
  coefi <- coef(fit.bss, id = i)
  print(coefi)
  coln <- names(coefi)[names(coefi) != "(Intercept)"]
  colnums <- sapply(coln, colnum)
  betas <- c(0, beta[colnums])
  val.errors[i] <- sqrt(sum((coefi - betas)^2))
}
plot(val.errors, xlab = "Number of predictors"
     , ylab = "Error between estimated and true coeffs", pch = 19, type = "b")
which.min(val.errors)

# Question 11
library(MASS)
set.seed(12321)

k <- 5
folds <- sample(1:k, dim(Boston)[1], replace = TRUE)
coefs <- matrix(NA, 5 * 13, 14)
val.errors <- matrix(NA, 5, 13)

for (i in 1:k) {
  Boston.train <- Boston[folds != i,]
  Boston.test <- Boston[folds == i,]
  fit.fwd <- regsubsets(crim~., data=Boston.train, nvmax = 13, method = "forward")
  test.mat <- model.matrix(crim~., data = Boston.test)
    for (j in 1:13) {
      coefi <- coef(fit.fwd, id = j)
      coefs[j + (i-1) * 13, 1:(j+1)] <- coefi
      pred <- test.mat[, names(coefi)] %*% coefi
      val.errors[i,j] <- mean((pred - Boston.test$crim)^2)
    }    
}

coefs
cv.errors <- colMeans(val.errors)
cv.errors
plot(cv.errors, type = "b", xlab = "Number of variables", ylab = "CV error")

# over the cross validation it appears that all the predictors are required

# now for lasso
# I'll use fold 1 as a test set to determine the OOS error rate
# the lambda selection is done by cross validation inside the glmnet library
bestlams <- rep(NA, k)
MSEs <- rep(NA, k)
coef.table <- matrix(NA, 14, 5)
for (i in 1:k) {
  set.seed(1001)
  x <- model.matrix(crim~., data = Boston[folds != i,])[, -1]
  x.test <- model.matrix(crim~., data = Boston[folds == i,])[, -1]
  y <- Boston$crim[folds != i]
  y.test <- Boston$crim[folds == i]
  
  cv.Boston.lasso <- cv.glmnet(x, y, alpha = 1)
  plot(cv.Boston.lasso)
  bestlams[i] <- cv.Boston.lasso$lambda.min
  
  fit.Boston.lasso <- glmnet(x,y,alpha=1)
  plot(fit.lasso, xvar = "lambda")
  
  # OOB - using the held back fold, MSE associated with the best value of lambda
  pred.Boston.lasso <- predict(fit.Boston.lasso, s=bestlams[i], newx = x.test)
  MSEs[i] <- mean((pred.Boston.lasso-y.test)^2) #MSE
  coef.table[1:14,i] <- as.numeric(round(predict(fit.Boston.lasso, s=bestlams[i]
                , type = "coefficients"),4))
}
coef.table[,which.min(MSEs)]

# the lowest MSE was from fold 2. The model has a lambda of approx 0.03 and includes all the features