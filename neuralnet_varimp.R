nLayers <- length(ptrons)
var.influences <- list()

for (i in 1:nLayers) {
  if (is.matrix(ptrons[[i]])) {
    nPtrons <- nrow(ptrons[[i]])  
    nPtrons.next <- ncol(ptrons[[i]])
    } else {
    nPtrons <- length(ptrons[[i]])
    nPtrons.next <- 1
  }
  
  neg.influence <- matrix(ncol = nPtrons.next
                          , nrow = nPtrons)
  pos.influence <- neg.influence
  
  if (nPtrons.next == 1) {
    influence <- ptrons[[i]]
    neg.influence <- ifelse(sign(influence) == -1
                     , influence
                     , 0)
    pos.influence <- ifelse(sign(influence) == 1
                     , influence
                     , 0)
    neg.influence <- neg.influence/sum(neg.influence)
    pos.influence <- pos.influence/sum(pos.influence)
  } else {
    for (j in 1:nPtrons.next) {
      influence <- ptrons[[i]][, j]
      neg.influence[, j] <- ifelse(sign(influence) == -1
                                   , influence
                                   , 0)
      pos.influence[, j] <- ifelse(sign(influence) == 1
                                   , influence
                                   , 0)
      neg.influence[, j] <- neg.influence[, j]/sum(neg.influence[, j])
      pos.influence[, j] <- pos.influence[, j]/sum(pos.influence[, j])
    }
  }
  var.influences[[paste0("layer.", i, ".neg")]] <- neg.influence
  var.influences[[paste0("layer.", i, ".pos")]] <- pos.influence
#  neg.influence <- -apply(neg.influence, 1, sum)
#  pos.influence <- apply(pos.influence, 1, sum)
}