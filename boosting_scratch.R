# view the shape
# error rate
eps <- seq(0.01, 0.99, 0.01)

# weights update constant
alph <- 0.5 * log((1-eps)/eps)
plot(eps, -alph, type="l")

# demo routine
n <- 100
k <- 200
true_probas <- c(0.8, 0.2)
probas <- c(0.8, 0.2)

true_vals <- c(rep(1, true_probas[1] * n)
               , rep(-1, true_probas[2] * n))
true_vals

wts <- rep(1/n)
wtsmat <- matrix(NA, ncol = n, nrow = k)
predsmat <- matrix(NA, ncol = n, nrow = k)
alphas <- numeric(k)
for(i in 1:k) {
  # generate errors and trues
  resps <- c(sample(c(1, -1)
                    , prob = probas
                    , size = true_probas[1] * n
                    , replace = TRUE)
             , sample(c(-1, 1)
                      , prob = probas
                      , size = true_probas[2] * n
                      , replace = TRUE))
  resps
  
  # choose alpha
  eps <- mean(ifelse(resps == -1, 1, 0))
  eps
  alph <- 0.5 * log((1-eps)/eps)
  alph
  # update distribution
  raws <- exp(-resps * true_vals * alph)
  raws
  wts <- wts * raws
  wts
  wts <- wts/(sum(wts))
  wts
  sum(wts)
  wtsmat[i, ] <- wts
  predsmat[i, ] <- resps
  alphas[i] <- alph
}
matplot(wtsmat, type="n")
matlines(wtsmat)
apply(predsmat * alphas, 2, sum)
sign(apply(predsmat * alphas, 2, sum))

