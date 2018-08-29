library(ISLR)
library(boot)
boot.fn <- function(data, index) {
  return(coef(lm(mpg~horsepower
                 , data = data, subset = index)
              )
         )
}
boot.fn(Auto, 1:392)
set.seed(1)

boot.fn(Auto, sample(392,392, replace = TRUE))

boot(Auto,boot.fn,1000)

boot.fn2 <- function(data, index) {
  return(coef(lm(mpg~horsepower+I(horsepower^2)
                 , data = data, subset = index)
  )
  )
}
boot(Auto,boot.fn2,1000)

lm2 <- lm(mpg~horsepower+I(horsepower^2), data = Auto)
lmp <- lm(mpg~poly(horsepower,2), data = Auto)

