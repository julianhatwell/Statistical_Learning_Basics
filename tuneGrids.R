# tuneGrid examples

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9)
                        , n.trees = (1:30)*50
                        , shrinkage = 0.1
                        , n.minobsinnode = 20)

brnn.grid <- expand.grid(neurons = 1:5)

lasso.grid <-  expand.grid(fraction = seq(0.1, 1, 0.1))

foba.grid <- expand.grid(lambda = seq(0.01, 0.001 ,-0.001)
                         , k = c(5, 10, 15, 20))
