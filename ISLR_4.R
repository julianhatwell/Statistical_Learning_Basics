# https://rpubs.com/ppaquay/65560

library(ISLR)
library(caret)
library(MASS)
library(dplyr)
library(class)
set.seed(12321)

mpg01 <- factor(ifelse(Auto$mpg > median(Auto$mpg), 1, 0))
Auto1 <- Auto %>%
  mutate(mpg01 = mpg01
         , name = factor(name)
         , origin = factor(origin)
         , cylinders = factor(cylinders)
         , year = factor(year)
  ) %>%
  dplyr::select(-mpg)

train <- createDataPartition(Auto1$mpg01, p = 0.8, list = FALSE)
Auto1.train <- Auto1[train,]
Auto1.test <- Auto1[-train,]
refs <- Auto1.test$mpg01

ldam1 <- lda(mpg01~horsepower+weight+year, data = Auto1.train)
ldam2 <- lda(mpg01~origin+acceleration+cylinders, data = Auto1.train)
ldam3 <- lda(mpg01~origin+acceleration+cylinders+year, data = Auto1.train)

qdam2 <- qda(mpg01~origin+acceleration+cylinders, data = Auto1.train)
qdam3 <- qda(mpg01~origin+acceleration+cylinders+weight, data = Auto1.train)

glam1 <- glm(mpg01~horsepower+weight+year, data = Auto1.train, family = binomial)
glam2 <- glm(mpg01~origin+acceleration+cylinders, data = Auto1.train, family = binomial)
glam3 <- glm(mpg01~origin+acceleration+cylinders+year, data = Auto1.train, family = binomial)

ldam1.pred <- predict(ldam1, newdata = Auto1.test)$class
confusionMatrix(ldam1.pred, refs, positive = "1")

ldam2.pred <- predict(ldam2, newdata = Auto1.test)$class
confusionMatrix(ldam2.pred, refs, positive = "1")

ldam3.pred <- predict(ldam3, newdata = Auto1.test)$class
confusionMatrix(ldam3.pred, refs, positive = "1")

qdam2.pred <- predict(qdam2, newdata = Auto1.test)$class
confusionMatrix(qdam2.pred, refs, positive = "1")

qdam3.pred <- predict(qdam3, newdata = Auto1.test)$class
confusionMatrix(qdam3.pred, refs, positive = "1")

glam1.probs <- predict(glam1, newdata = Auto1.test, type = "response")
glam1.pred <- rep(0, 78)
glam1.pred[glam1.probs > 0.5] <- 1
confusionMatrix(glam1.pred, refs, positive = "1")

glam2.probs <- predict(glam2, newdata = Auto1.test, type = "response")
glam2.pred <- rep(0, 78)
glam2.pred[glam2.probs > 0.5] <- 1
confusionMatrix(glam2.pred, refs, positive = "1")

glam3.probs <- predict(glam3, newdata = Auto1.test, type = "response")
glam3.pred <- rep(0, 78)
glam3.pred[glam3.probs > 0.5] <- 1
confusionMatrix(glam3.pred, refs, positive = "1")

Auto1.train.mx <- as.matrix(Auto[train,c("origin", "acceleration", "cylinders")])
Auto1.test.mx <- as.matrix(Auto[-train,c("origin", "acceleration", "cylinders")])
Auto1.train.mpg01 <- mpg01[train]
Auto1.test.mpg01 <- mpg01[-train]
pred1.knn <- knn(Auto1.train.mx, Auto1.test.mx, Auto1.train.mpg01, k = 1)
confusionMatrix(pred1.knn, refs, positive = "1")

pred3.knn <- knn(Auto1.train.mx, Auto1.test.mx, Auto1.train.mpg01, k = 3)
confusionMatrix(pred3.knn, refs, positive = "1")

pred5.knn <- knn(Auto1.train.mx, Auto1.test.mx, Auto1.train.mpg01, k = 5)
confusionMatrix(pred5.knn, refs, positive = "1")

pred10.knn <- knn(Auto1.train.mx, Auto1.test.mx, Auto1.train.mpg01, k = 10)
confusionMatrix(pred10.knn, refs, positive = "1")

pred100.knn <- knn(Auto1.train.mx, Auto1.test.mx, Auto1.train.mpg01, k = 100)
confusionMatrix(pred100.knn, refs, positive = "1")