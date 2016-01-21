# stat learning project set up
source("utilityCode.R")

# give this run a unique name
thisRun <- "uniqueName"

# configure the data frame here
# refer out to any custom code, to do the basics
# such as making appropriate factors, ditching obviously useless columns
data("diamonds")
dt <- setData(diamonds, "cut")

# set up the model config file

# use the EDA file to explore the data

# create dummy variables out of factors if required
# NB this is not currently working as expected
# commented out for now
# dt$dt.frm <- createDummies(dt$dt.frm, dt$resp)

# refer to any custom code to "pre-preprocess"
# i.e. remove useless columns, combine correlated columns after partitioning
# so it can follow straight into any transformative pre-process
# which has to run on train, validate and test all alike.

# partition the data here for modeling and validation
trn.val.tst <- myStandardPartitioning(dt)

# all pre-processing here. use the boiler plate funcs and add any custom code here
# add further data sets to the trn.val.tst object

# for example
myPreProc <- preProcess(trn.val.tst$trn.asis[-dt$respCol]
                        , method = "pca"
                        , thresh = 0.9)

# the new data frame must be named as a set in the model config
trn.val.tst$trn.pca <- predict(myPreProc, trn.val.tst$trn.asis)
trn.val.tst$val.pca <- predict(myPreProc, trn.val.tst$val.asis)
# The number of predictor dimensions has been reduced to:                      
dim(trn.val.tst$trn.pca)[2] -1

# create the models
# df version
createModels(trn.val.tst, dt$resp, models, tCtrls)
# dt version - will try to match model config with dt_collection members by set name

# for classification problems, build a confusion matrix index from the built models
confmats <- createConfMats(trn.val.tst, dt$resp, models)
myConfMatsPlot(confmats)
