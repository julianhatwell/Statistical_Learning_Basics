library(caret)
library(lattice)
library(ggplot2)
library(parallel)
library(doParallel)

# f_classe <- factor(training_set$classe)
# myScatterPlot <- function(j, df) {
#   xyplot(df[[j]]~I(1:nrow(df))
#          , groups = f_classe 
#          , col = myPal
#          , ylab = j
#          , xlab = "index"
#   )
# }
# 
# myViolinPlot <- function(j, df) {
#   bwplot(df[[j]]~f_classe
#          , groups = f_classe
#          , col = myPal
#          , scales = list(y = list(tck = c(1, 0)))
#          , panel = panel.superpose
#          , panel.groups = panel.violin
#          , ylab = j)
# }
# 
# layoutPlots_4 <- function(vars, plotFunc, df) {
#   print(plotFunc(vars[1], df), pos = c(0,0.5, 0.5, 1), more = TRUE)
#   print(plotFunc(vars[2], df), pos = c(0.5, 0.5, 1, 1), more = TRUE)
#   print(plotFunc(vars[3], df), pos = c(0, 0, 0.5, 0.5), more = TRUE)
#   print(plotFunc(vars[4], df), pos = c(0.5, 0, 1, 0.5))
# }
# 
# 
# layoutPlots_4(exampleVars, myViolinPlot, training_set)


tc <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

get_or_train <- function(algo, trans) {
  
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


algorithms <- c("gbm", "qda", "rf")
transforms <- c("pca", "set")
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

# models[, c("model", "algo", "trans")]
# n <- nrow(models)
# for (m in 1:n) {
#   assign(models[m,"model"]
#          , get_or_train(algo = models[m,"algo"]
#                         , trans = models[m, "trans"])
#   )
# }
