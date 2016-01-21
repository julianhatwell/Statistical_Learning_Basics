# stat learning project set up
source("utilityCode.R")

# give this run a unique name - TO DO implement this
thisRun <- "uniqueName"

# problem type, either classification or regression
ptype <- "regression"

# configure the data frame here
# refer out to any custom code, to do the basics
# such as making appropriate factors, ditching obviously useless columns
data("diamonds")
dt <- setData(diamonds, "price")

# use the EDA file to explore the data
# do anything you can to the wholesale data set before partitioning
# such as removing entire columns
# creating dummy variables out of factors if required
# NB this is not currently working as expected
# commented out for now
# dt$dt.frm <- createDummies(dt$dt.frm, dt$resp)

# partition the data here for modeling and validation
trn.val.tst <- myStandardPartitioning(dt)

# choose your statistical learning method
algorithms <- c("lm", "foba", "lasso")
# set up the train controls for each model
# to customise for any model, over-write the default
tCtrls <- list()
for (algo in algorithms) {
  tCtrls[[algo]] <- trainControl(method = "cv", number = 4, allowParallel = TRUE)
}

# TO DO - implement tune grids

# list your transforms or just "asis" for the as is training set
# and remember to build any transformed sets with custom code
# to avoid building unecesary duplicates
sets <- c("asis", "pca")

# all pre-processing here. use the boiler plate funcs and add any custom code here
# add further data sets to the trn.val.tst object

# define models, transforms and custom code here
# for example
myPreProc <- preProcess(trn.val.tst$trn.asis[-dt$respCol]
                        , method = "pca"
                        , thresh = 0.9)

# the new data frame must be named as a set in the model config
trn.val.tst$trn.pca <- predict(myPreProc, trn.val.tst$trn.asis)
trn.val.tst$val.pca <- predict(myPreProc, trn.val.tst$val.asis)
trn.val.tst$tst.pca <- predict(myPreProc, trn.val.tst$tst.asis)

# set up the models matrix to control the remaining workflow
models <- createModelMatrix(algorithms, sets)

# create the models
createModels(trn.val.tst, dt$resp, models, tCtrls)

preds <- generatePredictions(trn.val.tst, models, "val.set")
testPreds <- generatePredictions(trn.val.tst, models, "tst.set")

# for classification problems, build a confusion matrix index from the built models
if (ptype == "classification") {
  confmats <- createConfMats(preds, "val.set", trn.val.tst, dt$resp, models)
  myConfMatsPlot(confmats)
}

# look at the buildTime v accuracy stats
modelStats <- compareModelStats(models, ptype)
compareModelStatsPlot(modelStats, ptype)