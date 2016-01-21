

buildTimes <- numeric(n)
cvAccuracy <- numeric(n)
accuracy <- matrix(0, n, 3, dimnames = list(NULL, c("Accuracy", "Accuracy_Lower", "Accuracy_Upper")))

for (m in 1:n) {
  buildTimes[m] <- get(models[m, "model"])$times$everything[3]
  cvAccuracy[m] <- round(max(get(models[m, "model"])$results$Accuracy),4)
  accuracy[m,] <- round(get(models[m, "result"])$overall[c(1, 3:4)], 4)
}

modelStats <- data.frame(model = models[, "model"]
                         , buildTimes = buildTimes
                         , cvAccuracy = cvAccuracy
                         , accuracy
                         , inSample_Error_Rate = 1 - cvAccuracy
                         , outOfSample_Error_Rate = 1 - accuracy[,1])

# a little utility function for the text
row.names(accuracy) <- models[,"model"]
accuracyConfInt <- function(m) { accuracy[m, 2:3] }

# Tablulate the results
kable(modelStats)

xyplot(Accuracy~buildTimes
       , data = modelStats
       , panel = function(x,y) {
         panel.loess(x, y
                     , span = 1.5
                     , degree = 1
                     , lwd = 10
                     , col = myPal[1]
                     , alpha = 0.3)
         panel.xyplot(x, y
                      , pch = 19
                      , col = myPal[5]
                      , cex = (1 + cvAccuracy) * cvAccuracy)
         panel.segments(x + c(rep(5,4), rep(-5,2)), y
                        , (x + c(rep(60,4), rep(-60,2))/3), y)
         panel.text(x + c(rep(90,4), rep(-80,2))
                    , y
                    , modelStats$model)
       }
       , main = list(columns = 2, label = "Model Performance and Training Times"
                     , cex = 0.8)
       , xlab = "Training Time (seconds)"
       , ylab = "Out of sample accuracy\nEstimated using validation set"
       , key = list(title = "Accuracy estimate from Cross Validation\n performed by the train function"
                    , cex = 0.5
                    , columns = 2
                    , text = list(as.character(range(cvAccuracy)), cex = 0.75)
                    , points = list(pch = 19, col = myPal[5], cex = range((1 + cvAccuracy) * cvAccuracy))
       )
)




# Tests
# Enact the EXACT same data cleaning and transformation steps.
# Remove the unwanted columns
testing_set <- cbind(rmUnwantedCols(testing)[, -53], classe = character(20))

# Shift above zero, take the log and perform PCA
# Using only parameters built from the training set
testing_pca <- predict(myPreProc
                       , myTransform(testing_set))

for (m in 1:n) {
  trans <- ifelse(grepl("pca", models[m,"pred"]), "pca", "set")
  assign(models[m,"pred"], predict(get(models[m,"model"])
                                   , get(paste0("testing_", trans)))
  )
}

predictions <- data.frame()
for (m in 1:n) {
  predictions <- rbind(predictions
                       , data.frame(id = 1:20
                                    , model = models[m,"model"]
                                    , prediction = get(models[m, "pred"])
                       )
  )
}

reference <- data.frame(id = 1:20
                        , reference = sapply(testing$num_window
                                             , function(nw) { 
                                               unique(training[training$num_window == nw
                                                               , "classe"])
                                             }
                        )
)

g <- ggplot(data = predictions, aes(x = id)) +
  geom_point(data = reference, aes(y = reference)
             , colour = myPal[1], size = 7, alpha = 0.5) +
  geom_point(aes(y = prediction)
             , colour = myPal[5], size = 3, shape = 15) +
  facet_wrap(~ model, ncol = 2) + theme_bw() +
  theme(strip.background = element_rect(fill=myPal[4])) + 
  labs(list(x = "Problem Id", y = "Prediction"))

g