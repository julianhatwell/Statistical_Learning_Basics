library(caret)
library(lattice)
library(ggplot2)
library(parallel)
library(doParallel)

# a nice colour palette
myPal <- c("#8DD3C7", "#B0A8B3", "#9FB5D6", "#9EC0FA", "#DB8072")
myPalDark <- c("#4D8377", "#504853", "#3F5576", "#3E409A", "#7B2012")
myPalContrasts <- c(myPalDark[1], myPalDark[5], myPal[2], myPal[4]
                    ,"#999999"
                    , myPal[1], myPal[5], myPalDark[2], myPalDark[4])
myPal.range <- colorRampPalette(c("#FFFFFF", myPal[3:1]))
myPal.rangeDark <- colorRampPalette(c("#FFFFFF", myPalDark[3:1]))
myPal.rangeDiv <- colorRampPalette(c(myPal[1], "#FFFFFF", myPal[5]))
myPal.rangeContrasts <- colorRampPalette(c("#FFFFFF", myPalContrasts))

myScatterPlot <- function(j, df) {
  xyplot(df[[j]]~I(1:nrow(df))
         , groups = y_fac 
         , col = myPal.range(num_classes)
         , alpha = 1/(length(training_ids)/2000)
         , ylab = j
         , xlab = "index"
  )
}

myViolinPlot <- function(j, df) {
  bwplot(df[[j]]~y_fac
         , groups = y_fac
         , col = myPal.rangeContrasts(num_classes)
         , scales = list(y = list(tck = c(1, 0)))
         , panel = panel.superpose
         , panel.groups = panel.violin
         , ylab = y)
}

layoutPlots_4 <- function(vars, plotFunc, df) {
  print(plotFunc(vars[1], df), pos = c(0,0.5, 0.5, 1), more = TRUE)
  print(plotFunc(vars[2], df), pos = c(0.5, 0.5, 1, 1), more = TRUE)
  print(plotFunc(vars[3], df), pos = c(0, 0, 0.5, 0.5), more = TRUE)
  print(plotFunc(vars[4], df), pos = c(0.5, 0, 1, 0.5))
}


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
