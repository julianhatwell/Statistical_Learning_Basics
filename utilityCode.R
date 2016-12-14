library(caret)
library(dplyr)
library(lattice)
library(ggplot2)
library(parallel)
library(doParallel)

# if (connected) {
#   source("https://raw.githubusercontent.com/julianhatwell/Utilities/master/Utilities.R")
#   source("https://raw.githubusercontent.com/julianhatwell/R_Themes/master/myFirstTheme.R")
# } else {
#   source("C:\\Dev\\Study\\R\\Utilities\\Utilities.R")
#   source("C:\\Dev\\Study\\R\\R_Themes\\myFirstTheme.R")
# }

# default data pack
data("diamonds")

# utility functions
# get the col number of the response - use whenever cols are added/removed
getRespCol <- function(df, y) {
  return(which(names(df) == y))
}

createModelMatrix <- function(algorithms, sets) {
  models <- cbind(algo = rep(algorithms, each = length(sets))
                  , sets = sets
                  , trn.set = paste0("trn.",sets)
                  , val.set = paste0("val.",sets)
                  , tst.set = paste0("tst.",sets)
                  , control = paste0(rep(algorithms, each = length(sets)), ".tc")
                  , grid = paste0(rep(algorithms, each = length(sets)), ".grid"))
  models <- cbind(models
                  , model = paste(models[, "algo"]
                                  , models[, "sets"]
                                  , "model"
                                  , sep = ".")
  )
  models <- cbind(models
                  , cmat = gsub("model", "cmat", models[,"model"])
                  , pred = gsub("model", "pred", models[,"model"])
                  , mark = gsub("model", "mark", models[,"model"])
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
  ptype <- if (is.numeric(df[[resp]])) { "regression" } else { "classification"}
  df.e <- list(dt.frm = df
              , resp = resp
              , num_classes = num_classes
              , vars = names(df)[-respCol]
              , vars_fac = sapply(df[-respCol], function(j) { any(class(j) == "factor") } )
              , respCol = respCol
              , ptype = ptype
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
  dummify <- cbind(df[[resp]], as.data.frame(predict(dummify, df)))
  names(dummify)[1] <- dt$resp
  return(dummify)
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
cor.vars.check <- function(dt, ctff = 0.75, rmv = FALSE) {
  cor.dt <- cor(dt$dt.frm[,dt$vars[!dt$vars_fac]])
  cor.cols <- names(dt$dt.frm[,dt$vars[!dt$vars_fac]])[findCorrelation(cor.dt, cutoff = ctff)]
  if (rmv) {
    return(dt$dt.frm[,!(names(dt$dt.frm) %in% cor.cols)])
  }
  else {
    return(cor.cols)
  }
}

# look for linear dependencies
lin.comb.check <- function(dt) {
  findLinearCombos(dt$dt.frm[,dt$vars[!dt$vars_fac]])
}

myStandardPartitioning <- function(dt, seed = 23) {
  set.seed(seed)
  # partitioning the data
  trn.ids <- createDataPartition(dt$dt.frm[[dt$resp]], p = 0.6, list = FALSE)
  trn.set <- dt$dt.frm[trn.ids,]
  
  holdout.set <- dt$dt.frm[-trn.ids,]
  
  val.ids <- createDataPartition(holdout.set[[dt$resp]], p = 0.5, list = FALSE)
  val.set <- holdout.set[val.ids, ]
  tst.set <- holdout.set[-val.ids, ]
  
  df.c <- list(trn.asis = trn.set
              , val.asis = val.set
              , tst.asis = tst.set
              , trn.ids = trn.ids
              , val.ids = val.ids)
  class(df.c) <- "df.collection"
  return(df.c)
}

flippedTwoClassSummary <- function(data, lev = NULL, model = NULL) {
  data <- data[, 2:1]
  twoClassSummary(data, lev = NULL, model = NULL)
}

# beautiful boiler plate for creating the models
get_or_train <- function(df, resp, algo
                         , set = "asis"
                         , modelName
                         , tg = NA
                         , tc = trainControl(method = "cv"
                                            , number = 5
                                            , allowParallel = TRUE)
                         , seed = 1001
                         , thisRun = "1"
                         , ...) {
  modelFileName <- paste0(modelName, "_", thisRun, ".RData")
  
  if (file.exists(modelFileName)) {
    attach(modelFileName, warn.conflicts = FALSE)
  } else {
    # set up parallel processing
    p_clus <- makeCluster(detectCores() - 1)
    registerDoParallel(p_clus)
    
    # build the model
    set.seed(seed) # ensure same resampling ids over multiple iters
    fmla <- as.formula(paste0(resp, "~."))
    modelTrain <- function(fmla, df, algo, ...) {
      dots <- list(...)
      argList <- list(fmla, data = df, method = algo)
      
      if (any(names(dots) == "metric")) {
        if (!(is.na(dots["metric"]))) {
          argList["metric"] <- dots["metric"]
        }
      }

      if (any(names(dots) == "maximize")) {
        if (!(is.na(dots["maximize"]))) {
          argList["maximize"] <- dots["maximize"]
        }
      }
      
      if (any(names(dots) == "trControl")) {
        if (!(is.na(dots["trControl"]))) {
          argList["trControl"] <- dots["trControl"]
        }
      }
      
      if (any(names(dots) == "tuneGrid")) {
        if (!(is.na(dots["tuneGrid"]))) {
          argList["tuneGrid"] <- dots["tuneGrid"]
        }
      }
      do.call(caret::train, argList)
    }
    assign(modelName, modelTrain(fmla, df, algo
                            , tuneGrid = tg, trControl = tc, ...)
           , envir = .GlobalEnv)
    
    # close parallel processing
    stopCluster(p_clus)
    
    # naive cache
    # save out to an external file for re-use
    model <- get(modelName)
    save(model, file = modelFileName)
  }
  return(model)
}

createModels <- function(df, resp, models, seed) {
  # this needs a tidy up
  n <- nrow(models)
    for (m in 1:n) {
      d <- if (any(class(df) == "data.frame")) { 
        df
      } else {
        if (any(class(df) == "df.collection")) { 
          df[[models[m, "trn.set"]]]
        }
      }
      tc <- if (exists(models[m, "control"])) { get(models[m, "control"]) } else { NA }
      tg <- if (exists(models[m, "grid"])) { get(models[m, "grid"]) } else { NA }
      assign(models[m,"model"]
             , get_or_train(d, resp
                            , algo = models[m,"algo"]
                            , set = models[m, "trn.set"]
                            , modelName = models[m, "model"]
                            , tc = tc
                            , tg = tg
                            , seed = seed)
      , envir = .GlobalEnv
      )
    }
}

generatePredictions <- function(dt, models, type = "val.set") {
  n <- nrow(models)
  predictions <- data.frame()
  
  for (m in 1:n) {
    assign(models[m,"pred"], predict(get(models[m,"model"])
                                     , dt[[models[m, type]]])
           , envir = .GlobalEnv
    )
    predictions <- rbind(predictions
                         , data.frame(model = models[m,"model"]
                                      , prediction = get(models[m, "pred"])
                         )
    )
    
  }
  return(predictions)
}

compareModelStats <- function(models, ptype) {
  n <- nrow(models)
  
  buildTimes <- numeric(n)
  for (m in 1:n) {
    buildTimes[m] <- get(models[m, "model"])$times$everything[3]
    
  }
  modelStats <- data.frame(model = models[, "model"]
                           , buildTimes = buildTimes)
  
  if (ptype == "regression") {
    RMSE <- numeric(n)
      for (m in 1:n) {
        RMSE[m] <- round(min(get(models[m, "model"])$results$RMSE), 4)
      }
    modelStats <- cbind(modelStats, RMSE)
  }
  
  if (ptype == "classification") {
    cvAccuracy <- numeric(n)
    accuracy <- matrix(0, n, 3
                       , dimnames = list(NULL # name the rows later
                                         , c("Accuracy"
                                             , "Accuracy_Lower"
                                             , "Accuracy_Upper")))
    for (m in 1:n) {
      cvAccuracy[m] <- round(max(get(models[m, "model"])$results$cvAccuracy),4)
      accuracy[m,] <- round(get(models[m, "cmat"])$overall[c(1, 3:4)], 4)
    }
    modelStats <- cbind(modelStats
                          , cvAccuracy = cvAccuracy
                          , OOS_Err_cv = 1 - cvAccuracy
                          , accuracy)
  }
  return(modelStats)
}

MADmodelStats <- function(models, ms, type, df, resp, preds) {
  n <- nrow(models)
  MAD <- numeric(n)
  for (m in 1:n) {
    diffs <- preds[preds$model == models[m, "model"], "prediction"] - 
                           df[[models[m, type]]][resp]
    MAD[m] <- round(mad(diffs[[1]]),4)
        
  }
  modelStats <- cbind(ms, MAD)
  return(modelStats)
}

compareModelStatsPlot <- function(ms, ptype) {
  if (ptype == "regression") {
    fmla <- as.formula("RMSE + MAD ~ buildTimes")
  }
  if (ptype == "classification") {
    fmla <- as.formula("OOS_Err_cv~buildTimes")
  }
  xyplot(fmla, groups = model, data = ms
         , prepanel = function(x, y, type, ...) {
             prepanel.default.xyplot(c(min(x) - 0.1, max(x) + 0.1)
                                     , c(min(y) - 0.01, max(y) + 0.01)
                                     , type, ...)
         }
         , panel = function(x,y) {
             panel.xyplot(x, y
                          , pch = 19
                          , col = myPal
                          , alpha = 1)
             panel.text(x, y, pos = 1
                        , modelStats$model)
         }
         , scales = list(relation = "sliced", tck = 1:0)
         , main = "Model Performance and Training Times"
         , xlab = "Training Time (seconds)"
         , ylab = "Error Rate Estimates"
         , par.settings = MyLatticeTheme
         )
}

createConfMats <- function(preds, type = "val.set", dt, resp, models) {
  n <- nrow(models)
  confmats <- data.frame()
  for (m in 1:n) {
    assign(models[m,"cmat"]
           , confusionMatrix(preds[preds$model == models[m, "model"], "prediction"]
                             , dt[[models[m, type]]][[resp]])
           , envir = .GlobalEnv
    )
    confmats <- rbind(confmats
                      , cbind.data.frame(model =  models[m, "model"]
                                         ,  get(models[m, "cmat"])$table)
    )
  }
  return(confmats)
}

myConfMatsPlot <- function(cf) {
  levelplot(sqrt(Freq)~Prediction+Reference | model
            , data = cf
            , shrink = c(0.25, 1)
            , col.regions = myPal.range
            , strip = MyLatticeStrip
            , scales = list(x = list(alternating = c(1,0,1)
                                     , tck = 1:0                                   )
                            , y = list(alternating = c(0,3))
                            , rot = 30
            )
            , between = list(x = 0.2, y = 0.2)
            , par.settings = MyLatticeTheme
            , main = list(label = "Levelplots of the confusion matrices"
                          , cex = 0.8)
            , sub = list(label = expression(paste("Colour and size scaled for emphasis to ", sqrt("Frequency")))
                         , cex = 0.75)
  )
}

#### --- cross validation - the next generation ---- ####
# utilities
getConfmatMetrics <- function(pred, resp, pred.dimname = NULL, resp.dimname = NULL) {
  confmat <- table(pred, resp)
  names(dimnames(confmat)) <- c(pred.dimname, resp.dimname)
  metrics <- confmat.metrics(confmat)
  return(metrics)  
}

createMetrics.nb <- function(m, d, t = 0.5
                             , r = resp
                             , classLabels = c(TRUE, FALSE)
                             , pred.dimname = NULL
                             , resp.dimname = NULL) {
  resp <- d[[r]]
  pred <- ifelse(predict(m
                         , type = "raw"
                         , newdata = d) > t, classLabels[1], classLabels[2])[, 2]
  
  metrics <- getConfmatMetrics(pred, resp, pred.dimname, resp.dimname)
  metrics$predictions <- pred
  metrics$cutoff <- t
  return(metrics)
}

createMetrics.nb.raw <- function(m, d, t = 0.5
                                 , r = resp
                                 , pred.dimname = NULL
                                 , resp.dimname = NULL) {
  resp <- d[[r]]
  pred <- predict(m, type = "raw"
                  , newdata = d)[, 2]
}

createMetrics.NB <- function(m, d, t = 0.5
                             , r = resp
                             , classLabels = c(TRUE, FALSE)
                             , pred.dimname = NULL
                             , resp.dimname = NULL) {
  resp <- d[[r]]
  pred <- predict(m, newdata = d) 
  pred <- ifelse(pred$posterior > t
                 , classLabels[1]
                 , classLabels[2])[, 2]
  
  metrics <- getConfmatMetrics(pred, resp, pred.dimname, resp.dimname)
  metrics$predictions <- pred
  metrics$cutoff <- t
  return(metrics)
}

createMetrics.lm <- function(m, d, t = 0.5, r
                             , classLabels = c(TRUE, FALSE)) {
  resp <- d[[r]]
  pred <- ifelse(predict(m
                         , type = "response"
                         , newdata = d) > t, classLabels[1], classLabels[2])
  
  metrics <- getConfmatMetrics(pred, resp)
  metrics$predictions <- pred
  metrics$cutoff <- t
  return(metrics)
}
standardMetrics <- function(m
                            , d = trn.val.tst$val.nzv
                            , t = 0.5) {
  sm <- createMetrics.lm(m = m
                         , d = d
                         , t = t
                         , r = resp)
  return(sm)
}

createMetrics.tree <- function(m, d, r) {
  type <- case_when(
    ("tree" %in% class(m)) ~ "class"
    , "randomForest" %in% class(m) ~ "response")
  
  resp <- d[[r]]
  pred <- predict(m, newdata = d, type = type)
  metrics <- getConfmatMetrics(pred, resp)
  metrics$predictions <- pred
  metrics$cutoff <- NA
  return(metrics)
}

createMetrics.gbm <- function(m, d, r, n, iters = 100) {
  t <- "response"
  n <- round(seq(10, n, length.out = iters))
  resp <- d[[r]]
  pred <- predict(m, newdata = d
                  , type = t, n.trees = n)
  pred <- ifelse(pred > 0.5, TRUE, FALSE)
  accuracy <- double(iters)
  for(i in 1:iters) {
    confmat <- table(pred[, i], resp)
    accuracy[i] <- if (dim(confmat)[1] == 2) {
      (confmat[1,1]+confmat[2,2])/sum(confmat)
    } else {
      confmat[rownames(confmat) == colnames(confmat)]/sum(confmat)
    }
  }
  return(accuracy)
}

drop.preds <- function(x) {
  rtn <- x
  rtn$predictions <- NULL
  rtn$cutoff <- NULL
  return(rtn)
}

xval.boot.func <- function(df, type = "fold"
                           , k = 2, seed = 105
                           , fmla
                           , algo = "naiveBayes"
                           , metrics.func = "createMetrics.nb"
                           , metrics.args
                           , ...) {
  # pre setup
  set.seed(seed)
  instances <- nrow(df)
  fit.list <- list()
  metrics.list <- list()
  index.list <- list()
  instances.list <- list()
  argList.list <- list()
  dots <- list(...)
  
  # create folds index if required
  if (type == "fold") {
    index <- sample(k
                    , size = instances
                    , replace = TRUE)
  }
  
  for (i in 1:k) {
    # get a new copy of metrics function arguments
    metrics.argList <- metrics.args
    # create the training set
    if(type == "fold") {
      train <- df[index != i, ]
      test <- df[index == i, ]
    } else {
      index <- sample(instances, replace = TRUE)
      train <- df[index, ]
      test <- df[-unique(index), ]
    }
    argList <- list(formula = fmla
                    , data = train)
    for (dot in names(dots)) {
      argList[dot] <- dots[dot]
    }

    fit.list[[i]] <- do.call(what = algo
                             , args = argList)
    
    metrics.argList$m <- fit.list[[i]]
    if (!(any(names(metrics.argList) == "d"))) {
      metrics.argList$d <- test
    }
    metrics.list[[i]] <- do.call(what = metrics.func
                                 , args = metrics.argList)
    
    index.list[[i]] <- index
    instances.list[[i]] <- list()
    instances.list[[i]]$train <- train
    instances.list[[i]]$test <- test
    argList.list[[i]] <- list()
    argList.list[[i]]$algo <- argList
    argList.list[[i]]$metrics <- metrics.argList
  }
  return(list(fit.list, metrics.list, index.list, instances.list, argList.list))
}
