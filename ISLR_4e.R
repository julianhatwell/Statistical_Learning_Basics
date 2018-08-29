require(ISLR)
require(boot)
load("5.R.RData")

fn <- function(data, index) {
  return(coef(lm(y~., data = data, subset = index))[2])
}
boot.out <- boot(Xy, fn, R = 1000)
boot.out

rows <- list(1:100, 101:200, 201:300, 301:400, 401:500, 501:600, 601:700, 701:800, 801:900, 901:1000)
new.rows <- matrix(nrow = 1000, ncol = 1000)
for (i in 1:1000) {
new.rows[,i] <- unlist(sample(rows, 10, replace = TRUE))
}

block.out <- numeric(1000)
for (i in 1:1000) {
  block.out[i] <- fn(Xy, new.rows[,i])
}