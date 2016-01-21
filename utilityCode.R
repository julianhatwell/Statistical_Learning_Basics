library(caret)
library(dplyr)
library(lattice)
library(ggplot2)
library(parallel)
library(doParallel)

source("https://raw.githubusercontent.com/julianhatwell/Utilities/master/Utilities.R")
source("https://raw.githubusercontent.com/julianhatwell/R_Themes/master/myFirstTheme.R")

# default data pack
data("diamonds")

# utility functions
# get the col number of the response - use whenever cols are added/removed
getRespCol <- function(df, y) {
  return(which(names(df) == y))
}

createModelMatrix <- function(algorithms, sets) {
  models <- cbind(algo = rep(algorithms, each = length(sets)), set = sets)
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
  df.e <- list(dt.frm = df
              , resp = resp
              , num_classes = num_classes
              , vars = names(df)[-respCol]
              , vars_fac = sapply(df[-respCol], function(j) { any(class(j) == "factor") } )
              , respCol = respCol
             )
  class(df.e) <- "df.enum"
  return(df.e)

}

# various loopable plots for examining vars
myScatterPlot <- function(j, df, resp) {
  xyplot(df[[j]]~I(1:nrow(df)) | df[[resp]]
         , groups = df[[resp]] 
         , col = myPal.rangeContrasts(
            (dt$num_classes + 1)* 10)[seq(10, dt$num_classes*10, 10)]
         , alpha = 1/((nrow(dt$dt.frm))/2000)
         , ylab = j
         , xlab = "index"
         , par.settings = MyLatticeTheme
         , strip = MyLatticeStrip
  )
}

myViolinPlot <- function(j, df, resp) {
  bwplot(df[[j]]~df[[resp]]
         , groups = df[[resp]]
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

myDensityPlot <- function(j, df, resp, pnts = TRUE) {
  densityplot(~df[[j]] | df[[resp]]
              , groups = df[[resp]]
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

layoutPlots_4 <- function(vars, plotFunc, df, ...) {
  print(plotFunc(vars[1], df, ...), pos = c(0,0.5, 0.5, 1), more = TRUE)
  print(plotFunc(vars[2], df, ...), pos = c(0.5, 0.5, 1, 1), more = TRUE)
  print(plotFunc(vars[3], df, ...), pos = c(0, 0, 0.5, 0.5), more = TRUE)
  print(plotFunc(vars[4], df, ...), pos = c(0.5, 0, 1, 0.5))
}

# the following code takes a data frame 
# i.e. training, validation or test sets, not my dt class
# NB this is not working as expected
createDummies <- function(df, resp) {
  fmla <- as.formula(paste0(resp, "~."))
  dummify <- dummyVars(fmla,data = df)
  return(as.data.frame(predict(dummify, df)))
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

# look for highly correlated vars
cor.vars.check <- function(dt, ctff = 0.75) {
  cor.dt <- cor(dt$dt.frm[,dt$vars[!dt$vars_fac]])
  summary(cor.dt[upper.tri(cor.dt)])
  return(findCorrelation(cor.dt, cutoff = ctff))
}

# look for linear dependencies
lin.comb.check <- function(dt) {
  findLinearCombos(dt$dt.frm[,dt$vars[!dt$vars_fac]])
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
  
  df.c <- list(trn = training_set
              , val = validation_set
              , tst = test_set
              , training_ids = training_ids
              , validation_ids = validation_ids)
  class(df.c) <- "df.collection"
  return(df.c)
}


# beautiful boiler plate for creating the models
get_or_train <- function(df, resp, algo, set = "trn"
                         , tc = trainControl(method = "cv"
                                            , number = 5
                                            , allowParallel = TRUE)) {
  
  modelName <- paste(algo, set, sep = "_")
  modelFileName <- paste0(modelName, ".RData")
  
  if (file.exists(modelFileName)) {
    attach(modelFileName, warn.conflicts = FALSE)
  } else {
    # set up parallel processing
    p_clus <- makeCluster(detectCores())
    registerDoParallel(p_clus)
    
    # build the model
    fmla <- as.formula(paste0(resp, "~."))
    assign(modelName, train(fmla, data = df,  trControl = tc, method = algo))
    
    # close parallel processing
    stopCluster(p_clus)
    
    # naive cache
    # save out to an external file for re-use
    model <- get(modelName)
    save(model, file = modelFileName)
  }
  return(model)
}

createModels <- function(df, resp, models, tCtrls) {
  # this needs a tidy up
  n <- nrow(models)
  if (any(class(df) == "data.frame")) {
    for (m in 1:n) {
      assign(models[m,"model"]
             , get_or_train(df, resp
                            , algo = models[m,"algo"]
                            , set = models[m, "set"]
                            , tc = tCtrls[[algo]])
             , envir = .GlobalEnv
      )
    }
  return()}
  if (any(class(df) == "df.collection")) {
    for (m in 1:n) {
      assign(models[m,"model"]
             , get_or_train(df[[models[m, "set"]]], resp
                            , algo = models[m,"algo"]
                            , set = models[m, "set"]
                            , tc = tCtrls[[algo]])
             , envir = .GlobalEnv
      )
    }
  return()}
}
