# define models, transforms and custom code here

# choose your statistical learning method
algorithms <- c("lda", "qda")

# list your transforms or just "asis" for the as is training set
# and remember to build any transformed sets with custom code
# to avoid building unecesary duplicates
sets <- c("asis", "pca")

# set up the models matrix
models <- createModelMatrix(algorithms, sets)

# set up the train controls for each model
# to customise for any model, over-write the default
tCtrls <- list()
for (algo in algorithms) {
  tCtrls[[algo]] <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
}