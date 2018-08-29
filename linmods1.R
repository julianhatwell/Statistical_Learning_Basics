plot(1:10, 1:10, type = "n"
     , main = )
text(2.5, 9, expression(
  frac(y - bar(y),SD[y]) == r*frac(x - bar(x),SD[x])
  ), cex = 2)
text(2.5, 7.5, "r = cor(x, y)")
text(5, 6, expression(
  beta == Cor(x, y)*frac(SD[y], SD[x])
  ), cex = 2)
text(5, 4, expression(
  alpha == mean(y) - beta * mean(x)
), cex = 2)

data(GaltonFamilies, package = "HistData")
plot(childHeight ~ midparentHeight, GaltonFamilies)

lmod <- lm(childHeight ~ midparentHeight, GaltonFamilies)
coef(lmod)
abline(lmod)


correl  <- with(GaltonFamilies, cor(childHeight, midparentHeight))
beta <- with(GaltonFamilies
             , cor(midparentHeight
                   , childHeight) *
               sd(childHeight) / sd(midparentHeight))
alpha <- with(GaltonFamilies
              , mean(childHeight) -
                beta * mean(midparentHeight))

beta1  <- with(GaltonFamilies, sd(childHeight) / sd(midparentHeight))
alpha1 <- with(GaltonFamilies, mean(childHeight) - beta1 * mean(midparentHeight))

plot(childHeight ~ midparentHeight, GaltonFamilies)
with(GaltonFamilies
     , points(mean(midparentHeight)
              , mean(childHeight)
              , cex = 2, pch = 4
              , col = "red"
              , lwd = 4))
abline(alpha, beta, lty = 2, lwd = 2)
abline(alpha1, beta1, lty = 3, lwd = 2
       , col = "blue")
legend("topleft"
       , legend = c("least squares line"
                    , "cor = 1 line")
       , lty = 2:3
       , lwd = 2
       , col = c("black", "blue"))

