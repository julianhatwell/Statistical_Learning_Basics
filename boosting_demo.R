library(ISLR); data(Wage); library(ggplot2); library(caret);
Wage <- subset(Wage,select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

modFit <- train(wage ~ ., method="gbm",data=training,verbose=FALSE)
print(modFit)

qplot(predict(modFit,testing),wage,data=testing)

p <-  data.frame(frm = "prediction"
            , dat = predict(modFit,testing))
r <-  data.frame(frm = "reference"
            , dat = testing$wage)
forqq <- rbind(p,r)

qq(frm~dat, forqq)
