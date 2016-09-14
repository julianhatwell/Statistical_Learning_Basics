# ----- Utility Funcs -----
# neuralnet library needs specific setup
neuralnet.fmla <- function(resp, dt) {
  r <- resp
  n <- names(dt)
  f <- as.formula(paste(r, "~", paste(n[!n %in% r], collapse = " + ")))
  f
}

MSE <- function(act, pred) {
  sum((pred - act)^2)/length(pred)
} # mean squared error

MAD <- function(act, pred) {
  median(abs(pred - act))
} # absolute median deviation

RMSE <- function(act, pred) {
  sqrt(sum((pred - act)^2)/length(pred))
} # route mean squared error

reg.measures <- function(act, pred) {
  MSE.measure <- MSE(act, pred)
  RMSE.measure <- RMSE(act, pred)
  MAD.measure <- MAD(act, pred)
  data.frame(MSE.measure, RMSE.measure, MAD.measure)
}

# action like a lekprofile
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
  
  quantiles <- rownames(ins.mat)
  rownames(ins.mat) <- NULL
  prediction <- data.frame(input.var = rep(ins, each = 50)
                           , quantiles = rep(rep(rownames(ins.quant), each = 10), nins)
                           , pred = as.vector(pred.mat)
                           , ins.mat)
  prediction$quantiles <- factor(prediction$quantiles, levels = c("0%", "25%", "50%", "75%", "100%"), ordered = TRUE)
  
  var.imp <- matrix(nrow = nins, ncol = 3, dimnames = list(ins, NULL))
  for (var in ins) {
    pred.ranges <- with(prediction
                      , tapply(subset(
                          pred, input.var == var)
                          , quantiles[1:50]
                          , range)
    )

    pred.ranges <- lapply(pred.ranges
                          , function(x) {
                            x[2] - x[1]
                            })

    var.imp[var, ] <- c(pred.ranges$`0%`
                        , pred.ranges$`100%`
                        , max(unlist(pred.ranges)))
  }
  input.var <- rownames(var.imp)
  var.imp <- data.frame(input.var, var.imp)
  names(var.imp) <- c("input.var", "vars.quant.0", "vars.quant.100", "max.effect")
  return(list(prediction = prediction
              , variable.importance = var.imp))
}

source("C:\\Dev\\Study\\R\\R_Themes\\MarketingTheme.R")
nn.profile.plot <- function(nn.diag, var, ...) {
  
  n <- names(nn.diag$prediction)
  n <- n[!(n %in% c("input.var", "quantiles", "pred" ))]
  if (!(var %in% n)) {stop("Variable selected does not exist.")}

  fmla <- as.formula(paste("pred ~", var))
  xyplot(fmla
       , group = quantiles
       , data = nn.diag$prediction
       , subset = input.var == var
       , type = "l"
       , scales = MyLatticeScale
       , strip = MyLatticeStrip
       , par.settings = MyLatticeTheme
       , auto.key = list(columns = 3
                        , points = FALSE
                        , lines = TRUE)
       , ...)
}

nn.varimp.plot <- function(nn.diag, ...) {
  
  barchart(max.effect+vars.quant.0+vars.quant.100~input.var
         , data = nn.diag$variable.importance
         , scales = MyLatticeScale
         , strip = MyLatticeStrip
         , par.settings = MyLatticeTheme
         , auto.key = list(columns = 3)
         , ...)
}