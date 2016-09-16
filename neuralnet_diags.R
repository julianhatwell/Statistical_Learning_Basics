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

  # quantiles and ranges for each input.var  
  ins.range <- sapply(nn$data[, ins], range)
  ins.quant <- sapply(nn$data[, ins], quantile)
  
  # matrix of all the quantiles for all input.vars
  ins.mat <- ins.quant[rep(1:5, each = 10), ]
  
  # empty df to hold the predictions
  preds <- list()
  input.vals <- list()
  preds$quantiles <- rownames(ins.mat)
  preds$quantiles <- factor(preds$quantiles, levels = c("0%", "25%", "50%", "75%", "100%"), ordered = TRUE)
  
  for (var in ins) {
    # one input.var at a time
    # temporarily replace the static quantiles with a incremental range
    ins.val <- seq(ins.range[1, var], ins.range[2, var], length.out = 10)
    ins.mat[, var] <- ins.val
    input.vals[[var]] <- ins.val
    # empty vector to hold the predictions
    pred <- numeric(50)
    for (i in 1:5) {
      pred[1:10 + (i - 1) * 10] <- compute(nn
      , ins.mat[1:10 + (i - 1) * 10, ])$net.result
    }
  # capture
  preds[[var]] <- pred
  
  # reset
  ins.mat <- ins.quant[rep(1:5, each = 10), ]
  }
  preds <- as.data.frame(preds)
  input.vals <- as.data.frame(input.vals)

  # variable importance from profile  
  var.imp.p <- matrix(nrow = 3, ncol = nins, dimnames = list(NULL, ins))

  pred.ranges <- as.data.frame(
                  lapply(preds[, -1]
                  , function(x) {
                      tapply(x
                      , preds$quantiles
                      , function(y) {
                        range(y)[2] - range(y)[1]
                      }
                    )
                  }
                )
              )
  for (var in ins) {
    var.imp.p[, var] <- c(pred.ranges[[var]]["0%"]
                          , pred.ranges[[var]]["100%"]
                          , max(pred.ranges[[var]]))
  }

  dimnames(var.imp.p) <- list(
    c("vars.quant.0", "vars.quant.100", "max.effect")
    , NULL)
  var.imp.p <- data.frame(input.var = factor(ins, levels = ins[order(var.imp.p["max.effect", ], decreasing = TRUE)])
                          , t(var.imp.p))
  
  # variable importance from weights
  wts <- nn$weights[[1]]
  layers <- numeric(0)
  # input layer = rows of wts element 1, minus 1
  # (the first values is the bias value)
  layers[1] <- dim(wts[[1]])[1] - 1
  seq.layers <- seq_along(wts)
  for (i in seq.layers) {
    # other layers are the number of columns of each wts element
    layers[i + 1] <- dim(wts[[i]])[2]
  }

  # extract the biases and weights
  bias <- list()
  ptrons <- list()
  
  for (i in seq.layers) {
    bias[[i]] <- wts[[i]][1, ]
    ptrons[[i]] <- wts[[i]][-1, ]
  }
  
  # calculate the influence by weights accumulation
  var.imp.w <- rnorm(1)
  for (i in rev(seq.layers)) {
    var.imp.w <- var.imp.w - bias[[i]]
    if (layers[i + 1] == 1) {
      var.imp.w <- var.imp.w - ptrons[[i]]
    } else {
      var.imp.w <- ptrons[[i]] - rep(var.imp.w, each = layers[i])
      var.imp.w <- apply(var.imp.w, 1, sum)
    }
  }
  var.imp.w <- data.frame(input.var = factor(ins, levels = ins[order(abs(var.imp.w), decreasing = TRUE)])
                          , effect = var.imp.w
                          , sgn = factor(sign(var.imp.w), labels = c("negative", "positive")))
  
  return(list(preds = preds
              , var.imp.p = var.imp.p
              , var.imp.w = var.imp.w
              , compute.matrix = ins.mat
              , input.values = input.vals
              , layers = layers))
}

library(lattice)
source("C:\\Dev\\Study\\R\\R_Themes\\MarketingTheme.R")
nn.profile.plot <- function(nn.diag, var = NULL, ...) {

  if (missing(var)) {
    preds <- gather(nn.diag$preds, input.var, effect, - quantiles)
    preds$input <- gather(nn.diag$input.values, input.var, input)[, -1]
    
    main.title <- "Profile Plot of changing each predictor
    while holding other predictors at quantiles"
    fmla <- as.formula("effect ~ input | input.var")
  } else { 
    n <- names(nn.diag$preds)
    n <- n[n != "quantiles"]
    if (!(var %in% n)) {stop("Variable selected does not exist.")}
  
    preds <- nn.diag$preds
    preds$input <- nn.diag$input.values[[var]]
  
    paste("Profile Plot of changing"
          , var, "\nwhile holding other predictors at quantiles")
    fmla <- as.formula(paste(var, "~ input"))
  }
  
  xyplot(fmla
       , group = quantiles
       , data = preds
       , type = "l"
       , xlab = paste(var, "(scaled)")
       , ylab = "Predicted value (scaled)"
       , main = main.title
       , scales = MyLatticeScale
       , strip = MyLatticeStrip
       , par.settings = MyLatticeTheme
       , auto.key = list(columns = 3
                        , points = FALSE
                        , lines = TRUE)
       , ...)
}

nn.varimp.p.plot <- function(nn.diag, ...) {
  
  barchart(max.effect+vars.quant.0+vars.quant.100~input.var
         , data = nn.diag$var.imp.p
         , ylab = "Range of effect of changing each input
         while holding the others constant"
         , main = "Variable Importance Plot from profile"
         , sub = paste("Hidden layers"
                       , paste(layers[-c(1, length(layers))]
                              , collapse = ", "))
         , scales = MyLatticeScale
         , strip = MyLatticeStrip
         , par.settings = MyLatticeTheme
         , auto.key = list(columns = 3)
         , ...)
}

nn.varimp.w.plot <- function(nn.diag, ...) {
  
  dotplot(abs(effect)~input.var
         , groups = sgn
         , data = nn.diag$var.imp.w
         , ylab = "Result of matrix multiplication of weights"
         , main = "Variable Importance Plot from weights accumulation"
         , sub = paste("Hidden layers"
                       , paste(layers[-c(1, length(layers))]
                               , collapse = ", "))
         , scales = MyLatticeScale
         , strip = MyLatticeStrip
         , par.settings = MyLatticeTheme
         , auto.key = list(columns = 2)
         , ...)
}