y <- (rbinom(100, 1, seq(0, 1, 0.01)) * 2) - 1

a <- 1:20
b <- 21:40
c <- 41:60
d <- 61:80
e <- 81:100

fn <- function(x) 0.5 * log(mean(x == 1)/mean(x == -1))
fn(y[a]);fn(y[b]);fn(y[c]);fn(y[d]);fn(y[e])

loss <- function(y, yi, fit=0) sum(log(1 + exp(-2 * y * (yi + fit))))
loss(y, y) # absolute minimum

f0 <- 0.5 * log((1 + mean(y))/(1 - mean(y)))
f0

yi <- rep(f0, length(y))

loss(y, yi) # initial loss

for (l in letters[1:5]) {
  print(loss(y[get(l)], yi[get(l)]))
} # initial loss per segment

ytild <- function(y, yi) (2 * y) / (1 + exp(2 * y * yi))
ytild(y, yi) # initial pseudo responses

probfromyi <- function(yi) exp(yi) / (1 + exp(yi))
probfromyi(yi) # initial preds on prob scale

findfit <- function(yi) sum(yi) / sum(abs(yi) * (2 - abs(yi)))

for (l in letters[1:5]) {
  print(l)
  yt <- ytild(y[get(l)], yi[get(l)])
  print(yt)
  fit <- findfit(yt)
  print(fit)
  print(loss(y[get(l)], yi[get(l)], fit))
  yi[get(l)] <- yi[get(l)] + fit
  print(yi[get(l)])
}

loss(y, yi)

for (l in letters[1:5]) {
  print(loss(y[get(l)], yi[get(l)]))
} 
probfromyi(yi)

# prob to be cherry picked next round
mapply(FUN=loss, y, yi) / ( 1 + mapply(FUN=loss, y, yi))
probfromyi(ytild(y, yi)) # residuals/gradients
