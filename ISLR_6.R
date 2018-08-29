library(ISLR)
library(caret)
summary(College)
train <- createDataPartition(College$Apps, p = 0.8, list = FALSE)
fit.lm <- lm(Apps~., data = College[train,])
pred.lm <- predict(fit.lm, newdata = College[-train,])
mean((pred.lm - College$Apps[-train])^2) #MSE
sqrt(mean((pred.lm - College$Apps[-train])^2)) #RMSE