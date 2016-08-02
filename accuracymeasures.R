pred <- predict(lm1)
error <- pred - Boston$medv
mae <- mean(abs(error))
rmse <- sqrt(mean((error)^2))
rae <- mean(abs(error)) / mean(abs(Boston$medv - mean(Boston$medv)))
rse <- mean((error)^2) / mean((Boston$medv - mean(Boston$medv))^2)

cat("Mean Absolute Error:", round(mae, 6), "\n")
cat("Root Mean Squared Error:", round(rmse, 6), "\n")
cat("Relative Absolute Error:", round(rae, 6), "\n")
cat("Relative Squared Error:", round(rse, 6), "\n")