
print(head(College,2))

# Create Vector of Column Max and Min Values
maxs <- apply(College[,2:18], 2, max)
mins <- apply(College[,2:18], 2, min)
# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(College[,2:18],center = mins, scale = maxs - mins))
# Check results
print(head(scaled.data,2))

# Convert Private column from Yes/No to 1/0
Private = as.numeric(College$Private)-1
dt = cbind(Private,scaled.data)

# Create Split (any column is fine)
set.seed(101)
split = sample.split(dt$Private, SplitRatio = 0.70)
# Split based off of split Boolean Vector
train = subset(dt, split == TRUE)
test = subset(dt, split == FALSE)

# create formula for neuralnet (does not accept y~.)
r <- "Private"
n <- names(scaled.data)
f <- as.formula(paste(r, "~", paste(n[!n %in% r], collapse = " + ")))

# train
nn <- neuralnet(f, train
                , hidden=c(10,10,10)
                , linear.output=FALSE)

# Compute Predictions off Test Set
predicted.nn.values <- compute(nn,test[2:18])
pred.nn <- sapply(predicted.nn.values$net.result
                  , function(x) {
                    ifelse(x > 0.5, 1, 0)
                  })

table(pred = pred.nn, actual = test$Private)


# infert example
nn <- neuralnet(case~age+
                  parity+induced+spontaneous
                , data=infert
                , hidden=2
                , err.fct="ce"
                , linear.output=FALSE)

nn$result.matrix
plot(nn)