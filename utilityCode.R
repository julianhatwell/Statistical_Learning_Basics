data("diamonds")
# to be generic
df <- diamonds
y <- "cut"

set.seed(23)
training_ids <- createDataPartition(df[[y]], p = 0.6, list = FALSE)
training_set <- df[training_ids,]

holdout_set <- df[-training_ids,]
validation_ids <- createDataPartition(holdout_set[[y]], p = 0.5, list = FALSE)
validation_set <- holdout_set[validation_ids, ]
test_set <- holdout_set[-validation_ids, ]