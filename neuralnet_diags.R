neuralnet.diagnostics <- function(nn) {
  outs <- unlist(strsplit(
    as.character(
      formula(nn))[2]
    , split = " + "
    , fixed = TRUE))
  nouts <- length(outs)
  if(nouts != 1) stop("Function can only handle single response variable")
  
  ins <- unlist(strsplit(
    as.character(
      formula(nn))[3]
    , split = " + "
    , fixed = TRUE))
  nins <- length(ins)
  
  ins.range <- sapply(nn$data[, ins], range)
  ins.quant <- sapply(nn$data[, ins], quantile)
  
  ins.mat <- matrix(nrow = 0, ncol = nins, dimnames = list(NULL, ins))
  pred.mat <- matrix(nrow = 50, ncol = nins, dimnames = list(NULL, ins))
  for (var in ins) {
    var.mat <- ins.quant[rep(1:5, each = 10), ]
    var.mat[, var] <- seq(ins.range[1, var], ins.range[2, var], length.out = 10)
    ins.mat <- rbind(ins.mat, var.mat)
    
    for (i in 1:5) {
      pred.mat[1:10 + (i - 1) * 10, var] <- compute(nn, var.mat[1:10 + (i - 1) * 10, ])$net.result
    }
  }
  var.imp <- apply(pred.mat, 2, function(x) {
    max(x) - min(x)
    })
  quantiles <- rownames(ins.mat)
  rownames(ins.mat) <- NULL
  prediction <- data.frame(input.var = rep(ins, each = 50)
                           , quantiles = rep(rep(rownames(ins.quant), each = 10), nins)
                           , pred = as.vector(pred.mat)
                           , ins.mat)
  prediction$quantiles <- factor(prediction$quantiles, levels = c("0%", "25%", "50%", "75%", "100%"), ordered = TRUE)
  return(list(prediction = prediction
              , variable.importance = var.imp))
}