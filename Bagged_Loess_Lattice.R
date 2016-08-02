set.seed(105)
ll <- matrix(NA,nrow=100,ncol=155)
for(i in 1:100){
  ss <- sample(1:dim(ozone)[1],replace=T)
  ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
  loess0 <- loess(temperature ~ ozone,data=ozone0,span=0.2)
  ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))
}

xyplot(temperature~ozone, data = ozone
       , pch = 19, col = "black"
       , bagLines = ll
       , prepanel = function(x,y, bagLines) {
          list(xlim = c(1, max(x))
               , ylim = c(min(bagLines, na.rm = TRUE)
                        , max(bagLines, na.rm = TRUE)))
       }
       , panel = function(x,y, ...) {
          for (i in 1:100) {
            panel.lines(1:155,ll[i,], alpha = 0.1, col = "green")
          }
          panel.xyplot(x,y, ...)
          panel.lines(1:155,apply(ll,2,mean),col="red",lwd=3)
      }
)
