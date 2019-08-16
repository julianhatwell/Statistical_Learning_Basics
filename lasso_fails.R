# link here: https://insightr.wordpress.com/2017/06/14/when-the-lasso-fails/

# notes: lasso based on two assumptions.
# 1. sparsity - only a small number of many available variables may be relevant
# 2. irrepresentable condition, irc. relevant variables and irrelevant variables are uncorrelated

# this demo is what happens when assumption 2 is violated
library(mvtnorm)
library(corrplot)
library(glmnet)
library(clusterGeneration)

k=10 # = Number of Candidate Variables
p=5 # = Number of Relevant Variables
N=500 # = Number of observations
betas=(-1)^(1:p) # = Values for beta = rep(c(-1, 1), 3)[1:p]
set.seed(12345) # = Seed for replication
sigma1=genPositiveDefMat(k,"unifcorrmat")$Sigma # = Sigma1 violates the irc
sigma2=sigma1 # = Sigma2 satisfies the irc
sigma2[(p+1):k,1:p]=0
sigma2[1:p,(p+1):k]=0

# note that the cov mat divides into 4 theoretical pieces, relevant cov, irrelevant cov, rel-irrel cov, irrel-rel cov
# irc respected if rel-irrl * rel^-1 * sign(betas) < 1 is true for all elements

# = Verify the irrepresentable condition
irc1=sort(abs(sigma1[(p+1):k,1:p]%*%solve(sigma1[1:p,1:p])%*%sign(betas)))
irc2=sort(abs(sigma2[(p+1):k,1:p]%*%solve(sigma2[1:p,1:p])%*%sign(betas)))
c(max(irc1),max(irc2))

# = Have a look at the correlation matrices
par(mfrow=c(1,2))
corrplot(cov2cor(sigma1))
corrplot(cov2cor(sigma2))


X1=rmvnorm(N,sigma = sigma1) # = Variables for the design that violates the IRC = #
X2=rmvnorm(N,sigma = sigma2) # = Variables for the design that satisfies the IRC = #
e=rnorm(N) # = Error = #
y1=X1[,1:p]%*%betas+e # = Generate y for design 1 = #
y2=X2[,1:p]%*%betas+e # = Generate y for design 2 = #

lasso1=glmnet(X1,y1,nlambda = 100) # = Estimation for design 1 = #
lasso2=glmnet(X2,y2,nlambda = 100) # = Estimation for design 2 = #

## == Regularization path == ##
par(mfrow=c(1,2))
l1=log(lasso1$lambda)
matplot(as.matrix(l1),t(coef(lasso1)[-1,])
        ,type="l",lty=1,col=c(rep(1,9),2)
        ,ylab="coef",xlab="log(lambda)",main="Violates IRC")
l2=log(lasso2$lambda)
matplot(as.matrix(l2),t(coef(lasso2)[-1,])
        ,type="l",lty=1,col=c(rep(1,9),2)
        ,ylab="coef",xlab="log(lambda)",main="Satisfies IRC")

# adalasso corrects for this problem
lasso1.1=cv.glmnet(X1,y1)
w.=(abs(coef(lasso1.1)[-1])+1/N)^(-1)
adalasso1=glmnet(X1,y1,penalty.factor = w.)
# penalty.factor	 - from glmnet help
# Separate penalty factors can be applied to each coefficient. 
# This is a number that multiplies lambda to allow differential shrinkage. 
# Can be 0 for some variables, which implies no shrinkage, and that variable is always included in the model. Default is 1 for all variables (and implicitly infinity for variables listed in exclude). Note: the penalty factors are internally rescaled to sum to nvars, and the lambda sequence will reflect this change.

par(mfrow=c(1,2))
l1=log(lasso1$lambda)
matplot(as.matrix(l1),t(coef(lasso1)[-1,])
        ,type="l",lty=1,col=c(rep(1,9),2)
        ,ylab="coef",xlab="log(lambda)",main="LASSO")
l2=log(adalasso1$lambda)
matplot(as.matrix(l2),t(coef(adalasso1)[-1,]),type="l"
        ,lty=1,col=c(rep(1,9),2)
        ,ylab="coef",xlab="log(lambda)",main="adaLASSO")
# extra glmnet plot
plot(lasso1, xvar="dev", label = FALSE, col = c(rep(1, 8), 2, 1))