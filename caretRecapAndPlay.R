library(car)
library(caret)
library(parallel)
library(doParallel)
library(foreach)

library(QSARdata)
data("Mutagen")

# follow examples
mutagen <- Mutagen_Outcome
descr <- Mutagen_Dragon

inTrain <- createDataPartition(mutagen
                               , p = 3/4
                               , list = FALSE)
trainDescr <- descr[inTrain, ]
testDescr <- descr[-inTrain, ]
trainClass <- mutagen[inTrain]
testClass <- mutagen[-inTrain]

# create partition has preserved the proportions
prop.table(table(mutagen))
prop.table(table(trainClass))

ncol(trainDescr)
# remove zero only predictors
nzv <- nearZeroVar(trainDescr, uniqueCut = 0.01
                   , foreach = TRUE, allowParallel = TRUE)
trainDescr <- trainDescr[, -nzv]
testDescr <- testDescr[, -nzv]

descrCorr <- cor(trainDescr) # correlation matrix
highCorr <- findCorrelation(descrCorr, 0.90)
trainDescr <- trainDescr[, -highCorr]
testDescr <- testDescr[, -highCorr]

ncol(trainDescr)

# preprocess with default centre and scale
xTrans <- preProcess(trainDescr)
trainDescr.pp <- predict(xTrans, trainDescr)
testDescr.pp <- predict(xTrans, testDescr)

# general tControl
cvControl <- trainControl(method = "cv"
                          , number = 5)

if (file.exists("svmFit.RData")) {
  attach("svmFit.RData", warn.conflicts = FALSE)
} else {
  
  set.seed(2)
  sigma.estimate <- sigest(trainClass~as.matrix(trainDescr.pp)) # get bounds for sigma param
  svmGrid <- expand.grid(C = 10^seq(-2, 2, length.out = 5)
                         , sigma = sigma.estimate)
  # set up parallel
  p_clus <- makeCluster(detectCores() - 1)
  registerDoParallel(p_clus)
  # train
  svmFit <- train(trainDescr.pp, trainClass
                  , method = "svmRadial"
                  , tuneGrid = svmGrid
                  , trControl = cvControl
                  , scaled = FALSE)
  # close parallel
  stopCluster(p_clus)
  
  # save object
  save(svmFit, file = "svmFit.RData")
  }
# inspect
svmFit
svmFit$finalModel
plot(svmFit)
plot(svmFit, plotType = "line")
plot(svmFit, metric = "Kappa")
plot(svmFit, plotType = "level")
resampleHist(svmFit) # more useful for bootstrapped stats

if (file.exists("gbmFit.RData")) {
  attach("svmFit.RData", warn.conflicts = FALSE)
} else {
  
  set.seed(2)
  gbmGrid <- expand.grid(interaction.depth = c(2, 4, 8)
                         , n.trees = c(4, 6, 8)*500
                         , shrinkage = 10^c(-3, -2, -1)
                         , n.minobsinnode = 10)
  # set up parallel
  p_clus <- makeCluster(detectCores() - 1)
  registerDoParallel(p_clus)
  # train
  gbmFit <- train(trainDescr.pp
                  , trainClass
                  , method = "gbm"
                  , trControl = cvControl
                  , verbose = FALSE
                  , bag.fraction = 0.5
                  , tuneGrid = gbmGrid)
  # close parallel
  stopCluster(p_clus)
  
  # save object
  save(gbmFit, file = "gbmFit.RData")
}

# inspect
gbmFit
gbmFit$finalModel
plot(gbmFit)
plot(gbmFit, plotType = "line")
plot(gbmFit, metric = "Kappa")
plot(gbmFit, plotType = "level")
resampleHist(gbmFit) # more useful for bootstrapped stats

# looks like we might not have the upper limit for 
# n.trees or interaction.depth
# but I'm not running this again as it takes too long.

predict(svmFit, newdata = testDescr.pp)[1:5]
predict(gbmFit, newdata = testDescr.pp)[1:5]

models <- list(svm = svmFit, gbm = gbmFit)

testPred <- predict(models, newdata = testDescr.pp)
lapply(testPred, function(x) x[1:5])

predValues <- extractPrediction(models
                                , testX = testDescr.pp
                                , testY = testClass)
testValues <- subset(predValues, dataType == "Test")

svmPred <- subset(testValues, model == "svmRadial")
confusionMatrix(svmPred$pred, svmPred$obs)

gbmPred <- subset(testValues, model == "gbm")
confusionMatrix(gbmPred$pred, gbmPred$obs)

varImp(svmFit, scale = FALSE)
plot(varImp(svmFit), top = 20)
varImp(gbmFit, scale = FALSE)
plot(varImp(gbmFit), top = 20)

# data simul
set.seed(2016)
n <- 1000

noise0 <- rnorm(n, mean = 0, sd = 1)
noise1 <- rnorm(n, mean = 1, sd = 0.5)
noise2 <- rnorm(n, mean = 0, sd = 0.75)
noise3 <- rnorm(n, mean = 0.1, sd = 0.1)
noise4 <- rnorm(n, mean = 0.05, sd = 0.05)
noise5 <- rnorm(n, mean = 1, sd = seq(0.05, 0.5, length.out = n))

x0 <- ifelse(rbinom(n, size = 1, prob = 0.2) == 1
             , 2, 0)
x1 <- rexp(n, 5)
# getting a box cox parameter
lambda <- coef(powerTransform(x1)) # car package

x2 <- runif(n)

x3 <- factor(sample(c("A", "B", "C", "D")
                    , size = n 
                    , replace = TRUE))

x3 <- ifelse(x2 + noise3 > 0.5 & x3 == "A", "D", as.character(x3))

A <- which(x3 == "A")
B <- which(x3 == "B")
C <- which(x3 == "C")
D <- which(x3 == "D")

bA <- 0.5
bB <- 0.8
bC <- -0.2
bD <- 0.2

x4 <- rep(NA, n)
x4[A] <- bcPower(x1[A]
                 , lambda + mean(noise3[A])) + bA * noise1[A]
x4[B] <- bcPower(x1[B]
                 , lambda + mean(noise3[B])) + bB * noise1[B]
x4[C] <- bcPower(x1[C]
                 , lambda + mean(noise3[C])) + bC * noise1[C]
x4[D] <- bcPower(x1[D]
                 , lambda + mean(noise3[D])) + bD * noise1[D]

x4 <- x4/10

x5 <- rep(NA, n)
x5[A] <- bA + -log(ifelse(x2[A] < 0.5 + noise3[A]
                          , exp(x2[A])
                          , sqrt(x2[A])
)
) / (pi + x2[A]^2) + noise2[A]
x5[B] <- bB + -log(ifelse(x2[B] < 0.5 + noise3[B]
                          , exp(x2[B])
                          , sqrt(x2[B])
)
) / (pi + x2[B]^2) + noise2[B]
x5[C] <- bC + -log(ifelse(x2[C] < 0.5 + noise3[C]
                          , exp(x2[C])
                          , sqrt(x2[C])
)
) / (pi + x2[C]^2) + noise2[C]
x5[D] <- bD + -log(ifelse(x2[D] < 0.5 + noise3[D]
                          , exp(x2[D])
                          , sqrt(x2[D])
)
) / (pi + x2[D]^2) + noise2[D]

x6 <- x4
x6 <- ifelse(rbinom(n, 1, 0.33) == 1
             , x4 + noise3
             , runif(n, -2, 0.5))
for(i in 1:n) {
  x6[i] <- mean(c(x6[i], x4[i], x4[i]))
}

x7 <- 1/(1 + x5^2/2) + noise5

y <- x0 + noise0 +
  0.2 * x1 +
  0.1 * x1^2 +
  0.25 * x2 +
  -0.5 * x2^2 +
  -0.25 * (x2-0.1)^3 +
  8 * x4 +
  1.5 * x5 +
  5 * x6 + 
  3 * x7

cutoff <- median(bcPower(0.00000003100 + y + abs(min(y)), 1.002))
ybin <- ifelse(y <= cutoff - (0.00000003100 + abs(min(y)))
               , "Low"
               , "High")

xs <- data.frame(fx0 = factor(x0)
                 , x0, x1, x2, x3
                 , x4, x5, x6, x7
                 , y, ybin)

xs.preds <- data.frame(x0, x1, x2
                     , x4, x5, x6, x7)
set.seed(65)
trn <- createDataPartition(xs$ybin
                               , p = 3/4
                               , list = FALSE)
train.preds <- xs.preds[trn, ]
test.preds <- xs.preds[-trn, ]
train.y <- xs[trn, "y"]
test.y <- xs[-trn, "y"]
train.ybin <- xs[trn, "ybin"]
test.ybin <- xs[-trn, "ybin"]

prop.table(table(xs$ybin))
prop.table(table(train.ybin))

# preprocess with default centre and scale
myPreProc <- preProcess(train.preds)
train.preds.pp <- predict(myPreProc, train.preds)
test.preds.pp <- predict(myPreProc, test.preds)

if (file.exists("xs.models.RData")) {
  attach("xs.models.RData", warn.conflicts = FALSE)
} else {
    
  # general tControl
  bootControl <- trainControl(method = "boot"
                            , number = 50)
  
  xs.sigma.estimate <- sigest(train.ybin~as.matrix(train.preds.pp)) # get bounds for sigma param
  xs.svmGrid <- expand.grid(C = 10^seq(-2, 2, length.out = 5)
                         , sigma = seq(xs.sigma.estimate[1], xs.sigma.estimate[3], length.out = 5))
  
  set.seed(25)
  # set up parallel
  p_clus <- makeCluster(detectCores() - 1)
  registerDoParallel(p_clus)
  # train
  xs.svmFit <- train(train.preds.pp, train.ybin
                  , method = "svmRadial"
                  , tuneGrid = xs.svmGrid
                  , trControl = bootControl
                  , scaled = FALSE)
  # close parallel
  stopCluster(p_clus)
  
  xs.gbmGrid <- expand.grid(interaction.depth = 1:4
                         , n.trees = 1:3*1000
                         , shrinkage = 10^c(-3, -2, -1)
                         , n.minobsinnode = 5)
  set.seed(25)
  # set up parallel
  p_clus <- makeCluster(detectCores() - 1)
  registerDoParallel(p_clus)
  # train
  xs.gbmFit <- train(train.preds.pp, train.ybin
                  , method = "gbm"
                  , trControl = bootControl
                  , verbose = FALSE
                  , bag.fraction = 0.5
                  , tuneGrid = xs.gbmGrid)
  # close parallel
  stopCluster(p_clus)
  
  xs.nnetGrid <- expand.grid(size = 0:3
                            , decay = c(0, exp(seq(-2, 1, 0.333))/exp(1)))
  set.seed(25)
  # set up parallel
  p_clus <- makeCluster(detectCores() - 1)
  registerDoParallel(p_clus)
  # train
  xs.nnetFit <- train(train.preds.pp, train.ybin
                     , method = "nnet"
                     , trControl = bootControl
                     , verbose = FALSE
                     , tuneGrid = xs.nnetGrid)
  # close parallel
  stopCluster(p_clus)
  
  save(xs.svmFit, xs.gbmFit, xs.nnetFit, file = "xs.models.RData")
}

myPal <- colorRampPalette(c("#44AADD"
                            , "#44DDAA"
                            , "#AADD44"
                            , "#DDAA44"
                            , "#DD44AA"
                            , "#AA44DD"))

xs.gbmFit
xs.gbmFit$finalModel
plot(xs.gbmFit)
plot(xs.gbmFit, plotType = "level", col.regions = myPal(200))
resampleHist(xs.gbmFit) # more useful for bootstrapped stats

xs.svmFit
xs.svmFit$finalModel
plot(xs.svmFit)
plot(xs.svmFit, plotType = "level", col.regions = myPal(200))
resampleHist(xs.svmFit) # more useful for bootstrapped stats

xs.nnetFit
xs.nnetFit$finalModel
plot(xs.nnetFit)
plot(xs.nnetFit, plotType = "level", col.regions = myPal(200))
resampleHist(xs.nnetFit) # more useful for bootstrapped stats

predict(xs.svmFit, newdata = test.preds.pp)[1:5]
predict(xs.gbmFit, newdata = test.preds.pp)[1:5]
predict(xs.nnetFit, newdata = test.preds.pp)[1:5]

xs.models <- list(svm = xs.svmFit, gbm = xs.gbmFit, nnet = xs.nnetFit)

xs.testPreds <- predict(xs.models, newdata = test.preds.pp)
lapply(xs.testPreds, function(x) x[1:5])

xs.predValues <- extractPrediction(xs.models
                                , testX = test.preds.pp
                                , testY = test.ybin)
xs.testValues <- subset(xs.predValues, dataType == "Test")

xs.svmPred <- subset(xs.testValues, model == "svmRadial")
confusionMatrix(xs.svmPred$pred, xs.svmPred$obs)
xs.gbmPred <- subset(xs.testValues, model == "gbm")
confusionMatrix(xs.gbmPred$pred, xs.gbmPred$obs) # best Kappa
xs.nnetPred <- subset(xs.testValues, model == "nnet")
confusionMatrix(xs.nnetPred$pred, xs.nnetPred$obs)

varImp(xs.svmFit, scale = FALSE)
plot(varImp(xs.svmFit))
varImp(xs.gbmFit, scale = FALSE)
plot(varImp(xs.gbmFit))
varImp(xs.nnetFit, scale = FALSE)
plot(varImp(xs.nnetFit))
