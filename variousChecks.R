
# check for NA vals
na.vals.check(dt)
# and near zero variance
nzv.check(dt)
# and correlated variables
cor.vars.check(dt, 0.8)
# and for linear dependencies
lin.comb.check(dt)