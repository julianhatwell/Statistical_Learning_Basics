library(vcd)
library(vcdExtra)
# distributions
set.seed(12345)
k <- 0:12
Pk <- dbinom(k, 12, 1/3)
b <- barplot(Pk, names.arg = k
             , xlab = "number of successes"
             , ylab = "probability")
lines(x = b, y = Pk, col = "red")

Weldon_df <- as.data.frame(WeldonDice)
# collapse last three cols of simulated set
Pk <- c(Pk[1:10], sum(Pk[11:13]))
Exp <- round(26306 * Pk, 5) # expected
Diff <- Weldon_df$Freq - Exp
Chisq <- Diff^2/Exp
data.frame(Weldon_df, prob = round(Pk, 5), Exp, Diff, Chisq)

b2 <- barplot(Weldon_df$Freq)
lines(x = b2, y = Exp, col = "red")

p <- c(1/6, 1/3, 1/2, 2/3)
k <- 0:12
Prob <- outer(k, p, function(k,p) { dbinom(k, 12, p) })
str(Prob)

col <- palette()[2:5]
matplot(k, Prob, type = "o", pch = 15:17, col = col
        , xlab = "number of Successes", ylab = "Probability")
legend("topright", legend = as.character(p), pch = 15:17
       , lty = 1, col = col, title = "Pr(Success")

data("UKSoccer", package = "vcd")

soccer.df <- as.data.frame(UKSoccer, stringsAsFactors = FALSE)
soccer.df <- within(soccer.df, {
  Home <- as.numeric(Home)       # make numeric
  Away <- as.numeric(Away)       # make numeric
  Total <- Home + Away           # total goals
})
str(soccer.df)

soccer.df <- expand.dft(soccer.df)   # expand to ungrouped form
apply(soccer.df, 2, FUN = function(x) c(mean = mean(x), var = var(x)))


with(CyclingDeaths, c(mean = mean(deaths),
                      var = var(deaths),
                      ratio = mean(deaths) / var(deaths)))

mean.deaths <- mean(CyclingDeaths$deaths)
ppois(5, mean.deaths, lower.tail = FALSE)

KL <- expand.grid(k = 0 : 20, lambda = c(1, 3, 7, 10, 15, 20))
pois_df <- data.frame(KL, prob = dpois(KL$k, KL$lambda))
pois_df$lambda = factor(pois_df$lambda)
str(pois_df)

library(lattice)
library(directlabels)
xyplot(prob ~ k | lambda, data = pois_df,
       type = c("h", "p"), pch = 16, lwd = 2, cex = 1.25, layout = c(3, 2),
       xlab = list("Number of events (k)", cex = 1.25),
       ylab = list("Probability", cex = 1.25))

# use direct labels as an alternative way to legend a plot (lattice)
mycol <- palette()[2:7]
plt <- xyplot(prob ~ k, data = pois_df, groups = lambda,
              type = "b", pch = 15 : 17, lwd = 2, cex = 1.25, col = mycol,
              xlab = list("Number of events (k)", cex = 1.25),
              ylab = list("Probability",  cex = 1.25),
              ylim = c(0, 0.4))

library(directlabels)
direct.label(plt, list("top.points", cex = 1.5, dl.trans(y = y + 0.1)))

# ggplot alternative
library(ggplot2)
gplt <- ggplot(pois_df,
               aes(x = k, y = prob, colour = lambda, shape = lambda)) +
  geom_line(size = 1) + geom_point(size = 3) +
  xlab("Number of events (k)") +
  ylab("Probability")

gplt + theme(legend.position = c(0.9, 0.7)) +  # manually move legend
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

# negative binom
k <- 2
n <- 2 : 4
p <- 0.2
dnbinom(k, n, p)

(mu <- n * (1 - p) / p)
dnbinom(k, n, mu = mu)

XN <- expand.grid(k = 0 : 20, n = c(2, 4, 6), p = c(0.2, 0.3, 0.4))
nbin_df <- data.frame(XN, prob = dnbinom(XN$k, XN$n, XN$p))
nbin_df$n <- factor(nbin_df$n)
nbin_df$p <- factor(nbin_df$p)
str(nbin_df)

xyplot(prob ~ k | n + p, data = nbin_df,
       xlab = list("Number of failures (k)", cex = 1.25),
       ylab = list("Probability",  cex = 1.25),
       type = c("h", "p"), pch = 16, lwd = 2,
       strip = strip.custom(strip.names = TRUE)
)

# goodness-of-fit test
tab <- as.data.frame(HorseKicks, stringsAsFactors = FALSE)
colnames(tab) <- c("nDeaths", "Freq")
str(tab)
(lambda <- weighted.mean(as.numeric(tab$nDeaths), w = tab$Freq))

phat <- dpois(0 : 4, lambda = lambda)
exp <- sum(tab$Freq) * phat
chisq <- (tab$Freq - exp)^2 / exp

GOF <- data.frame(tab, phat, exp, chisq)
GOF

sum(chisq)  # chi-square value
pchisq(sum(chisq), df = nrow(tab) - 2, lower.tail = FALSE)

# sax.fit
data("Saxony", package = "vcd")
Sax_fit <- goodfit(Saxony, type = "binomial")
unlist(Sax_fit$par) # estimated parameters
names(Sax_fit)     # components of "goodfit" objects
Sax_fit            # print method
summary(Sax_fit)   # summary method

# Fed and plots
data("Federalist", package = "vcd")
Fed_fit0 <- goodfit(Federalist, type = "poisson")
unlist(Fed_fit0$par)
Fed_fit0
summary(Fed_fit0)

Fed_fit1 <- goodfit(Federalist, type = "nbinomial")
unlist(Fed_fit1$par)
summary(Fed_fit1)

plot(Fed_fit0, scale = "raw", type = "standing")
plot(Fed_fit0, type = "standing")
plot(Fed_fit0, type = "hanging", shade = TRUE)
plot(Fed_fit0, type = "deviation", shade = TRUE)

plot(Fed_fit1, scale = "raw", type = "standing")
plot(Fed_fit1, type = "standing")
plot(Fed_fit1, type = "hanging", shade = TRUE)
plot(Fed_fit1, type = "deviation", shade = TRUE)

# atrocious fits on butterfly
data("Butterfly", package = "vcd")
But_fit1 <- goodfit(Butterfly, type = "poisson")
But_fit2 <- goodfit(Butterfly, type = "nbinomial")
plot(But_fit1, main = "Poisson", shade = TRUE, legend = FALSE)
plot(But_fit2, main = "Negative binomial", shade = TRUE, legend = FALSE)

# ord plots
ord <- Ord_plot(Butterfly,
                main = "Butterfly species collected in Malaya",
                gp = gpar(cex = 1), pch = 16)
ord

data("HorseKicks", package = "vcd")
nk <- as.vector(HorseKicks)
k <- as.numeric(names(HorseKicks))
nk1 <- c(NA, nk[-length(nk)])
y <- k * nk / nk1
weight <- sqrt(pmax(nk, 1) - 1)
(ord_df <- data.frame(k, nk, nk1, y, weight))
coef(lm(y ~ k, weights = weight, data = ord_df))

Ord_plot(Federalist, main = "Instances of 'may' in Federalist Papers",
         gp = gpar(cex = 1), pch = 16)

Ord_plot(WomenQueue, main = "Women in queues of length 10",
         gp=gpar(cex=1), pch=16)

data("HorseKicks", package = "vcd")
dp <- distplot(HorseKicks, type = "poisson",
               xlab = "Number of deaths"
               , main = "Poissonness plot: HorseKicks data")
print(dp, digits = 4)

# leveled version, specifying lambda
distplot(HorseKicks, type = "poisson", lambda = 0.61,
         xlab = "Number of deaths"
         , main = "Leveled Poissonness plot")

plot(goodfit(Saxony, type = "binomial"
             , par = list(size=12)), 
     shade=TRUE, legend=FALSE,
     xlab = "Number of males")

distplot(Saxony, type = "binomial", size = 12,
         xlab = "Number of males")

distplot(Federalist, type = "poisson"
         , xlab = "Occurrences of 'may'")
distplot(Federalist, type = "nbinomial"
         , xlab = "Occurrences of 'may'")

