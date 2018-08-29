sz <- 1
n <- 10000
p <- 1/n
q <- 1-p
j <- 10000

pr <- numeric(100000)
for(i in 1:100000) {
  pr[i] <- (1-1/i)^i
}