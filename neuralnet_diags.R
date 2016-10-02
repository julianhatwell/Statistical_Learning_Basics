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
      pred[1:10 + (i - 1) * 10] <- neuralnet::compute(nn
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
  var.imp.p <- data.frame(input.var = factor(ins, levels = ins[order(var.imp.p["max.effect", ])])
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
  var.imp.w <- runif(1)
  for (i in rev(seq.layers)) {
    var.imp.w <- var.imp.w - bias[[i]]
    if (layers[i + 1] == 1) {
      var.imp.w <- var.imp.w * ptrons[[i]]
    } else {
      # sign according to input layer only
      var.imp.w <- ptrons[[i]] * rep(var.imp.w, each = layers[i])
      var.imp.w <- apply(var.imp.w, 1, sum)
      if(i == 1) {
        var.imp.w <- abs(var.imp.w) * sign(apply(ptrons[[i]], 1, sum))
      }
    }
  }
  var.imp.w <- data.frame(input.var = factor(ins, levels = ins[order(abs(var.imp.w))])
                          , effect = var.imp.w
                          , sgn = factor(sign(var.imp.w), labels = c("negative", "positive")))

  # calculate the influence by weights of only the input layer
  var.imp.f <- apply(ptrons[[1]], 1, sum)
  var.imp.f <- data.frame(input.var = factor(ins, levels = ins[order(abs(var.imp.f))])
                          , effect = var.imp.f
                          , sgn = factor(sign(var.imp.f), labels = c("negative", "positive")))
  
    
  return(list(preds = preds
              , var.imp.p = var.imp.p
              , var.imp.w = var.imp.w
              , var.imp.f = var.imp.f
              , compute.matrix = ins.mat
              , input.values = input.vals
              , layers = layers
              , ptrons = ptrons
              , bias = bias))
}

library(lattice)
library(tidyr)
nn.profile.plot <- function(nn.diag, var = NULL, ...) {

  if (missing(var)) {
    MyLatticeScale$x <- list(relation = "free")
    number.of.levels <- length(unique(dimnames(nn.diag$compute.matrix[,])[[1]]))
    input.vars <- matrix(nrow = 0, ncol = nn.diag$layers[1])
    for (i in 1:number.of.levels) {
      input.vars <- rbind(input.vars, nn.diag$input.values)
    }
    preds <- gather(nn.diag$preds, input.var, effect, - quantiles)
    preds$input <- gather(input.vars, input.var, input)[, -1]
    
    xlab.title <- "Predictor values"
    main.title <- "Profile Plot of changing each predictor
    while holding other predictors at quantiles"
    fmla <- as.formula("effect ~ input | input.var")
  } else { 
    n <- names(nn.diag$preds)
    n <- n[n != "quantiles"]
    if (!(var %in% n)) {stop("Variable selected does not exist.")}
  
    preds <- nn.diag$preds
    preds$input <- nn.diag$input.values[[var]]

    xlab.title <- var
    main.title <- paste("Profile Plot of changing"
          , var, "\nwhile holding other predictors at quantiles")
    fmla <- as.formula(paste(var, "~ input"))
  }
  
  xyplot(fmla
       , group = quantiles
       , data = preds
       , type = "l"
       , xlab = xlab.title
       , ylab = "Predicted value (scaled)"
       , main = main.title
       , sub = paste("Hidden layers"
                     , paste(nn.diag$layers[-c(1, length(nn.diag$layers))]
                             , collapse = ", "))
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
                       , paste(nn.diag$layers[-c(1, length(nn.diag$layers))]
                              , collapse = ", "))
         , scales = MyLatticeScale
         , strip = MyLatticeStrip
         , par.settings = MyLatticeTheme
         , auto.key = list(columns = 3)
         , ...)
}

nn.varimp.w.plot <- function(nn.diag, weight = "w", ...) {
  
  if (weight == "w") { 
    d <- nn.diag$var.imp.w
    main.title <- "Variable Importance Plot from weights accumulation"
  } 
  if (weight == "f") { 
    d <- nn.diag$var.imp.f
    main.title <- "Variable Importance Plot from input weights"
  } 
  
  dotplot(abs(effect)~input.var
         , groups = sgn
         , data = d
         , ylab = "Result of matrix multiplication of weights"
         , main = main.title
         , sub = paste("Hidden layers"
                       , paste(nn.diag$layers[-c(1, length(nn.diag$layers))]
                               , collapse = ", "))
         , scales = MyLatticeScale
         , strip = MyLatticeStrip
         , par.settings = MyLatticeTheme
         , auto.key = list(columns = 2)
         , ...)
}