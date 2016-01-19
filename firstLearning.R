# stat learning project set up
source("utilityCode.R")

# choose your statistical learning method
algorithms <- c("gbm", "qda", "rf")
# list your transforms or just set for full set.
transforms <- c("pca", "set")

# set up the models matrix
models <- createModelMatrix(algorithms, transforms)

# configure the data frame here
# refer out to any custom code, to do the basics
# such as making appropriate factors, ditching obviously useless columns
data("diamonds")
dt <- setData(diamonds, "cut")

# use this to do EDA, as well as the marvelous EDA report
par(ask = TRUE)
for (var in dt$vars[!dt$vars_fac]) {
    v <- myViolinPlot(var, dt)
    print(v)
  }
for (var in dt$vars[!dt$vars_fac]) {
  v <- myScatterPlot(var, dt)
  print(v)
}
for (var in dt$vars[!dt$vars_fac]) {
  v <- myDensityPlot(var, dt, pnts = FALSE)
  print(v)
}
par(ask = FALSE)

# check for NA vals
na.vals.check(dt)
# and near zero variance
nzv.check(dt)

trn.val.tst <- myStandardPartitioning(dt)

# choose some training control parameters for each model
# to DO - vectorise
tc <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

