# nnet analysis tools
# create model
library(neuralnet)
library(NeuralNetTools)
library(nnet)
library(RSNNS)
library(clusterGeneration)
library(devtools)
library(caret)
data(neuraldat)

AND <- c(rep(0, 7), 1)
OR <- c(0, rep(1, 7))
binary_data <- data.frame(expand.grid(c(0, 1), c(0, 1), c(0, 1)), AND, OR)
mod <- neuralnet(AND + OR ~ Var1 + Var2 + Var3, binary_data,
                 hidden = c(6, 12, 8), rep = 10, err.fct = 'ce', linear.output = FALSE)

# plotnet
par(mar = numeric(4), family = 'serif')
plotnet(mod)

# create model
mod <- neuralnet(Y1 ~ X1 + X2 + X3, data = neuraldat)
# garson
garson(mod)

mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5)
# lekprofile only works on nnet objects
lekprofile(mod)
plotnet(mod) # plot.nn doesn't work on nnet objs
garson(mod)


seed.val <- 2
set.seed(seed.val)

# some sim data
num.vars <- 8
num.obs <- 1000

#input variables
cov.mat <- genPositiveDefMat(num.vars
                             , covMethod=c("unifcorrmat"))$Sigma
rand.vars <- mvrnorm(num.obs
                     , rep(0, num.vars)
                     , Sigma=cov.mat)

#output variables
parms <- runif(num.vars, -10, 10)
y1 <- rand.vars %*% matrix(parms) + rnorm(num.obs, sd=20)
parms2 <- runif(num.vars, -10, 10)
y2 <- rand.vars %*% matrix(parms2) + rnorm(num.obs,sd=20)

#final datasets
rand.vars  <-data.frame(rand.vars)
resp <- data.frame(y1,y2)
names(resp) <- c('Y1','Y2')
dat.in <- data.frame(resp,rand.vars)

#nnet function from nnet package
set.seed(seed.val)
mod1 <- nnet(rand.vars
             , resp
             , data=dat.in
             , size=10
             , linout=TRUE)

#neuralnet function from neuralnet package, notice use of only one response
form.in <- as.formula('Y1~X1+X2+X3+X4+X5+X6+X7+X8')
set.seed(seed.val)
mod2 <- neuralnet(form.in
                  , data=dat.in
                  , hidden=10)

#mlp function from RSNNS package
set.seed(seed.val)
mod3 <- mlp(rand.vars
            , resp
            , size=10
            , linOut=T)

# caret train with nnet
# general tControl
cvControl <- trainControl(method = "cv"
                          , number = 5)
mod4<-train(Y1+Y2~.
            , method='nnet'
            , data=dat.in
            , linout=T
            , trainControl = cvControl)
plotnet(mod4, nid=T)

# multiple hidden layers
mod5 <- mlp(rand.vars
            , resp
            , size=c(9,11,8)
            , linOut=T)

#plot each model
plotnet(mod1)
plotnet(mod2) # doesn't work
plotnet(mod3) # no bias or label names
plotnet(mod4)
plotnet(mod5)

# only these work
lekprofile(mod1)
lekprofile(mod4)

# manual construction for plotting
wts.in <- mod1$wts
struct <- mod1$n
plot.nnet(wts.in, struct=struct)

# bias-h1, i1-h1, i2-h1, bias-h2, i1-h2, i2-h2
mod.in<-c(13.12,1.49,0.16,-0.11,-0.19,-0.16,0.56,-0.52,0.81)
struct<-c(2,2,1) #two inputs, two hidden, one output 
plot.nnet(mod.in, struct=struct)

# colour code input vars
# can only work for single layer
g1 <- garson(mod1)$data
col.scale <- (g1$rel_imp - min(g1$rel_imp)) / (max(g1$rel_imp) - min(g1$rel_imp))
cols <- colorRamp(c('green','red'))(col.scale)
cols <- rgb(cols[order(levels(g1$x_names)), ]/255)

plotnet(mod1
        , circle_col=list(cols,'lightblue'))


# demo of lekprofile
# sim data
set.seed(seed.val)
num.vars <- 8
num.obs <- 10000

#define correlation matrix for explanatory variables 
#define actual parameter values
cov.mat <- genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma
rand.vars <- mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)
parms1 <- runif(num.vars,-10,10)
y1 <- rand.vars %*% matrix(parms1) + rnorm(num.obs,sd=20)
parms2 <- runif(num.vars,-10,10)
y2 <- rand.vars %*% matrix(parms2) + rnorm(num.obs,sd=20)

#prep data and create neural network
rand.vars <- data.frame(rand.vars)
resp <- apply(cbind(y1,y2), 2
              , function(y) {
                (y-min(y))/(max(y)-min(y))
                }
              )
resp <- data.frame(resp)
names(resp) <- c('Y1','Y2')
mod1 <- nnet(rand.vars
             , resp
             , size=8
             , linout=T)

plotnet(mod1)
lekprofile(mod1)
lp <- lekprofile(mod1
           , xsel = c('X2','X5')
           , group_vals = seq(0,1,by=0.05))

lp
lp + theme_bw() +
  scale_linetype_manual(values=rep('dashed',6)) +
  scale_size_manual(values=rep(1,6))

# variable importance demo
set.seed(seed.val)
num.vars <- 8
num.obs <- 10000

#define correlation matrix for explanatory variables 
#define actual parameter values
cov.mat <- genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma
rand.vars <- mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)
parms <- runif(num.vars,-10,10)
y <- rand.vars %*% matrix(parms) + rnorm(num.obs,sd=20)

#prep data and create neural network
y <- data.frame((y-min(y))/(max(y)-min(y)))
names(y) <- 'y'
rand.vars <- data.frame(rand.vars)
mod1 <- nnet(rand.vars
             , y
             , size=8
             , linout=T)

#create a pretty color vector for the bar plot
cols.green <- colorRampPalette(c('lightgreen','lightblue'))(num.vars)

#use the function on the model created above
ga1 <- garson(mod1)
ga1 + scale_fill_gradientn(colours = cols.green) +
  scale_colour_gradientn(colours = cols.green) +
  scale_y_continuous('Rel. importance')
