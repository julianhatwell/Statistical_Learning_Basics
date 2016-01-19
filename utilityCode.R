library(caret)
library(dplyr)
library(lattice)
library(ggplot2)
library(parallel)
library(doParallel)

source("https://raw.githubusercontent.com/julianhatwell/R_Themes/master/myFirstTheme.R")

# default data pack
data("diamonds")

# utility functions
# get the col number of the response - use whenever cols are added/removed
getRespCol <- function(df, y) {
  return(which(names(df) == y))
}

createModelMatrix <- function(algorithms, transforms) {
  models <- cbind(algo = rep(algorithms, each = 2),trans = transforms)
  models <- cbind(models
                  , model = paste(models[,1]
                                  , models[,2]
                                  ,"model", sep = "_")
  )
  models <- cbind(models
                  , result = gsub("model", "cm", models[,3])
                  , pred = gsub("model", "pred", models[,3])
                  , mark = gsub("model", "mark", models[,3])
  )
  return(models)
}

# to be generic - set the data set names here
setData <- function(df, resp, makeFactorResp = FALSE) {
  if (makeFactorResp) { # if resp should be a factor but is not one
    df[[resp]] <- factor(df[[resp]])
    isFactorResp <- TRUE
  }
  num_classes <- NA
  if (any(class(df[[resp]]) == "factor")) {
    num_classes <- length(levels(factor(df[[resp]])))
  }
  respCol = getRespCol(df, resp)
  return(list(dt.frm = df
              , resp = resp
              , num_classes = num_classes
              , vars = names(df)[-respCol]
              , vars_fac = sapply(df[-respCol], function(j) { any(class(j) == "factor") } )
              , respCol = respCol
  )
  )
}

# various loopable plots for examining vars
myScatterPlot <- function(j, dt) {
  xyplot(dt$dt.frm[[j]]~I(1:nrow(dt$dt.frm)) | dt$dt.frm[[dt$resp]]
         , groups = dt$dt.frm[[dt$resp]] 
         , col = myPal.rangeContrasts(
            (dt$num_classes + 1)* 10)[seq(10, dt$num_classes*10, 10)]
         , alpha = 1/((nrow(dt$dt.frm))/2000)
         , ylab = j
         , xlab = "index"
         , par.settings = MyLatticeTheme
         , strip = MyLatticeStrip
  )
}

myViolinPlot <- function(j, dt) {
  bwplot(dt$dt.frm[[j]]~dt$dt.frm[[dt$resp]]
         , groups = dt$dt.frm[[dt$resp]]
         , col = myPal.rangeContrasts(
            (dt$num_classes + 1)* 10)[seq(10, dt$num_classes*10, 10)]
         , scales = list(y = list(tck = c(1, 0)))
         , panel = panel.superpose
         , panel.groups = panel.violin
         , xlab = dt$resp
         , ylab = j
         , par.settings = MyLatticeTheme
         , strip = MyLatticeStrip
  )
}

myDensityPlot <- function(j, dt, pnts) {
  densityplot(~dt$dt.frm[[j]] | dt$dt.frm[[dt$resp]]
              , groups = dt$dt.frm[[dt$resp]]
              , plot.points = pnts
              , lwd = 1.25
              , col = myPal.rangeContrasts(
                (dt$num_classes + 1)* 10)[seq(10, dt$num_classes*10, 10)]
              , scales = list(y = list(tck = c(1, 0)))
              , xlab = j
              , par.settings = MyLatticeTheme
              , strip = MyLatticeStrip
  )
}

layoutPlots_4 <- function(vars, plotFunc, df) {
  print(plotFunc(vars[1], df), pos = c(0,0.5, 0.5, 1), more = TRUE)
  print(plotFunc(vars[2], df), pos = c(0.5, 0.5, 1, 1), more = TRUE)
  print(plotFunc(vars[3], df), pos = c(0, 0, 0.5, 0.5), more = TRUE)
  print(plotFunc(vars[4], df), pos = c(0.5, 0, 1, 0.5))
}

# look for NAs
na.vals.check <- function(dt) {
  return(apply(dt$dt.frm, 2, function(j) {
                  sum(is.na(j))/length(j)
                }
               )
         )
}

# look for near zero variance variables
nzv.check <- function(dt) {
  return(nearZeroVar(dt$dt.frm, saveMetrics = TRUE))
}

myStandardPartitioning <- function(dt, seed = 23) {
  set.seed(seed)
  # partitioning the data
  training_ids <- createDataPartition(dt$dt.frm[[dt$resp]], p = 0.6, list = FALSE)
  training_set <- dt$dt.frm[training_ids,]
  
  holdout_set <- dt$dt.frm[-training_ids,]
  
  validation_ids <- createDataPartition(holdout_set[[dt$resp]], p = 0.5, list = FALSE)
  validation_set <- holdout_set[validation_ids, ]
  test_set <- holdout_set[-validation_ids, ]
  
  return(list(training_set, validation_set, test_set, training_ids, validation_ids))
}

# the following code takes a data frame 
# i.e. training, validation or test sets, not my dt class
createDummies <- function(df, resp) {
  return(dummyVars(resp~.,data = df))
}


# still TO DO
# look for highly correlated vars
cor.vars.check <- cor(df[,vars[!vars_fac]])
summary(cor.vars.check[upper.tri(cor.vars.check)])
cor.vars <- findCorrelation(cor.vars.check, cutoff = .75)
# removing them all us too crude at the moment

# look for linear depends
findLinearCombos(df[,vars[!vars_fac]])

# create dummy vars from factors
training_facsAsDummies <- dummyVars(resp~.,data = df)
head(predict(training_facsAsDummies, newdata = df))



# beautiful boiler plate for creating the models
get_or_train <- function(algo, trans, tc) {
  
  modelName <- paste(algo, trans, sep = "_")
  modelFileName <- paste0("model_", modelName, ".RData")
  
  if (file.exists(modelFileName)) {
    attach(modelFileName, warn.conflicts = FALSE)
  } else {
    dsName <- paste0("training_", trans)
    
    # set up parallel processing
    p_clus <- makeCluster(detectCores())
    registerDoParallel(p_clus)
    
    # build the model
    assign(modelName, train(classe~., data = get(dsName),  trControl = tc, method = algo))
    
    # close parallel processing
    stopCluster(p_clus)
    
    # naive cache
    # save out to an external file for re-use
    model <- get(modelName)
    save(model, file = modelFileName)
  }
  return(model)
}

