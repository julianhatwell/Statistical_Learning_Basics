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

# create dummy variables out of factors if required
# NB this is not currently working as expected
trn.val.tst$training_set <- createDummies(trn.val.tst$training_set, dt$resp)

# check for NA vals
na.vals.check(dt)
# and near zero variance
nzv.check(dt)
# and correlated variables
cor.vars.check(dt, 0.8)
# and for linear dependencies
lin.comb.check(dt)
# refer to any custom code to "pre-preprocess"
# i.e. remove useless columns, combine correlated columns after partitioning
# so it can follow straight into any transformative pre-process
# which has to run on train, validate and test all alike.

# partition the data here for modeling and validation
trn.val.tst <- myStandardPartitioning(dt)

# all pre-processing here. use the boiler plate funcs and add any custom code here



# choose some training control parameters for each model
# to DO - vectorise
tc <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

# loop over models and tc objects! TO DO.
n <- nrow(models)
for (m in 1:n) {
  assign(models[m,"model"]
         , get_or_train(algo = models[m,"algo"]
                        , trans = models[m, "trans"])
  )
}
