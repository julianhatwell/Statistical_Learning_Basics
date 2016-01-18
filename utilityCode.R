library(ggplot2)
library(caret)
data("diamonds")
# to be generic
df <- diamonds
resp <- "cut"
# y_fac <- factor(df[[y]])
resp_fac <- df[[y]]
num_classes <- length(levels(resp_fac))

getRespCol <- function(df, y) {
  which(names(df) == y)
}

respCol <- getRespCol(df, y)
vars <- names(df)[-respCol]

#create or set factors
vars_fac <- sapply(df, function(j) { any(class(j) == "factor") } )

for (var in vars[!vars_fac]) {
  v <- myViolinPlot(var, df)
  print(v)
}

set.seed(23)
training_ids <- createDataPartition(df[[resp]], p = 0.6, list = FALSE)
training_set <- df[training_ids,]

holdout_set <- df[-training_ids,]
validation_ids <- createDataPartition(holdout_set[[resp]], p = 0.5, list = FALSE)
validation_set <- holdout_set[validation_ids, ]
test_set <- holdout_set[-validation_ids, ]

naVals_check <- apply(df, 2, function(i) { sum(is.na(i))} )

summary(training_set)

training_facsAsDummies <- dummyVars(resp~.,data = df)
head(predict(training_facsAsDummies, newdata = df))
