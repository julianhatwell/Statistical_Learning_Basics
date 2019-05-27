library(faraway)
library(ggplot2)
data("eco")
plot(income~usborn, data=eco
     , xlab = "Proportion of US Born"
     , ylab = "Mean Annual Income")
lmod <- lm(income~usborn, data=eco)
abline(lmod)
plot(income~usborn, data=eco
     , xlab = "Proportion of US Born"
     , ylab = "Mean Annual Income"
     , xlim = c(0,1)
     , ylim = c(15000, 70000))
abline(lmod)

data("chredlin")
head(chredlin)
summary(chredlin)
plot(chredlin[, -7])

ggplot(data = chredlin
       , aes(x = race, y = involact)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(data = chredlin
       , aes(x = fire, y = involact)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(data = chredlin
       , aes(x = theft, y = involact)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(data = chredlin
       , aes(x = age, y = involact)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(data = chredlin
       , aes(x = income, y = involact)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(data = chredlin
       , aes(x = side, y = involact)) +
  geom_point(position = position_jitter(width = 0.2
                                        , height = 0)) +
  stat_summary(geom = "errorbar", colour = "red")

ggplot(data = chredlin
       , aes(x = race, y = fire)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(data = chredlin
       , aes(x = race, y = theft)) +
  geom_point() +
  stat_smooth(method = "lm")

lmod <- lm(involact ~ fire + race + theft + age + log(income)
           , data = chredlin)
sumary(lmod)
plot(lmod, 1:2) # streak is from zero valued, otherwise OK
termplot(lmod, partial.resid = TRUE, terms=1:2)
termplot(lmod, partial.resid = TRUE, terms=3:4)

listcombo <- unlist(
  sapply(0:4
         , function(x) {
           combn(4, x, simplify = FALSE)
           })
  , recursive = FALSE)

predterms <- lapply(listcombo, function(x) {
  paste(c("race", c("fire", "theft", "age", "log(income)")[x])
        , collapse = "+")})
coefm <- matrix(NA, 16, 2)
for (i in 1:16) {
  lmi <- lm(as.formula(paste("involact ~ ", predterms[[i]]))
            , data = chredlin)
  coefm[i, ] <- sumary(lmi)$coef[2, c(1,4)]
}
rownames(coefm) <- predterms
colnames(coefm) <- c("beta", "pvalue")
round(coefm, 4)

diags <- data.frame(lm.influence(lmod)$coef)
ggplot(data = diags
       , aes(x = row.names(diags)
             , y = race)) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("ZIP") +
  geom_hline(yintercept = 0) +
  geom_linerange(aes(ymax = 0, ymin = race))

ggplot(data = diags
       , aes(x = fire
             , y = theft)) +
  geom_text(label = row.names(diags))
ggplot(data = diags
       , aes(x = age
             , y = log.income.)) +
  geom_text(label = row.names(diags))
plot(lmod, 5) # none are extreme outliers
chredlin[c("60607", "60610"), ]
match(c("60607", "60610"), row.names(chredlin))
lmode <- lm(involact ~ fire + race + theft + age + log(income)
            , data = chredlin[
  -match(c("60607", "60610"), row.names(chredlin))
  , ])
sumary(lmode)

lmod <- lm(involact ~ fire + race + theft + age + log(income)
           , data = chredlin
           , subset = side == "n")
sumary(lmod)
lmod <- lm(involact ~ fire + race + theft + age + log(income)
           , data = chredlin
           , subset = side == "s")
sumary(lmod)
