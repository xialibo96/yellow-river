#### Model training 
library(caret)
library(readxl)
citynoNA <- read_excel("training and testing dataset.xls")

yy<-log(citynoNA$y)
hist(yy)
citynoNA<-citynoNA[,-16]
city2003<-citynoNA
city2003<-as.data.frame(city2003)

##### Split the dataset.
set.seed(22222) ####Set seed
inTrain = createDataPartition(yy, p = 3/4, list = FALSE)
traindata = city2003[inTrain,]
testdata = city2003[-inTrain,]
trainy = yy[inTrain]
testy = yy[-inTrain]
####### Centralization and standardization.
preProcValues <- preProcess(traindata, method = c( "center","scale"))

traindata <- predict(preProcValues, traindata)
testdata <- predict(preProcValues, testdata)
##### Merge x and y
traindata$y<-trainy
#testdata$y<-testy

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

########Support vector machine

svmRadfit <- train(y ~ ., data = traindata,  
                   method = "svmRadial", 
                   trControl = fitControl, 
                   verbose = FALSE,
                   tuneLength=10)
svmRadfit

svmLinear3fit<-train(y ~ ., data = traindata,  
                     method = "svmLinear3", 
                     trControl = fitControl, 
                     verbose = FALSE,
                     tuneLength=10)
svmLinear3fit

lmfit<-train(y ~ ., data = traindata,  
             method = "lm", 
             trControl = fitControl, 
             verbose = FALSE,
             tuneLength=10)
lmfit

#####Random Forest
rfFit1<- train(y ~ ., data = traindata, 
             method = "rf", 
             trControl = fitControl,
             verbose = FALSE,
             tuneLength=10)

plot(rfFit1)
ggplot(rfFit1) + theme_bw()


library(randomForest)
rf2<- randomForest(y ~ ., data = traindata)

###### k-Nearest neighbor
knn1<- train(y ~ ., data = traindata, 
             method = "knn", 
             trControl = fitControl,
             verbose = FALSE,
             tuneLength=10)

# Weighted k nearest neighbor 
## The hyperparameters is include kmax, distance and kernel
### "rectangular" "triangular", "epanechnikov" , "biweight" 
###   "triweight" , "cos", "inv", "gaussian", "rank" and "optimal".

kknnGrid3 <- expand.grid(kernel = c("cos", "inv", "gaussian","rectangular" ,"epanechnikov","triangular"), 
                         kmax=c(1:10), 
                         distance=c(1:10))

kknn3<- train(y ~ ., data = traindata, 
              method = "kknn", 
              trControl = fitControl,
              verbose = FALSE,
              tuneGrid=kknnGrid3)
kknn3
ggplot(kknn3)
plot(kknn3, metric = "RMSE", plotType = "level",
     scales = list(x = list(rot = 90)))
plot(kknn3, plotType = "level")
ggplot(kknn3) + theme_bw()


#### Model selection and prediction
traindata
testdata
trainy
testy
Plmtrain<-predict(lmfit,traindata)
R2(Plmtrain,trainy)
Plmtest<-predict(lmfit,testdata)
R2(Plmtest,testy)

Pkknntrain<-predict(kknn3,traindata)
R2(Pkknntrain,trainy)
Pkknntest<-predict(kknn3,testdata)
R2(Pkknntest,testy)

Prftrain<-predict(rfFit1,traindata)
R2(Prftrain,trainy)
Prftest<-predict(rfFit1,testdata)
R2(Prftest,testy)

Psvmtrain<-predict(svmRadfit,traindata)
R2(Psvmtrain,trainy)
Psvmtest<-predict(svmRadfit,testdata)
R2(Psvmtest,testy)

RMSE(Pkknntrain,trainy)
RMSE(Pkknntest,testy)

RMSE(Psvmtrain,trainy)
RMSE(Psvmtest,testy)

RMSE(Prftrain,trainy)
RMSE(Prftest,testy)

RMSE(Plmtrain,trainy)
RMSE(Plmtest,testy)

MAE(Pkknntrain,trainy)
MAE(Pkknntest,testy)

MAE(Psvmtrain,trainy)
MAE(Psvmtest,testy)

MAE(Prftrain,trainy)
MAE(Prftest,testy)

MAE(Plmtrain,trainy)
MAE(Plmtest,testy)

plot(testy,type='o',lwd = 1.5,main='lm')
lines(Plmtest,col='blue')
legend("topright",c("Actual value","Predict value"),col=c("black","blue"),
       text.col=c("black","blue"),lty=c(1,1))


plot(testy,type='o',lwd = 1.5,main='RF')
lines(Prftest,col='blue')
legend("topright",c("Actual value","Predict value"),col=c("black","blue"),
       text.col=c("black","blue"),lty=c(1,1))


plot(testy,type='o',lwd = 1.5,main='SVM')
lines(Psvmtest,col='blue')
legend("topright",c("Actual value","Predict value"),col=c("black","blue"),
       text.col=c("black","blue"),lty=c(1,1))


plot(testy,type='o',lwd = 1.5,main='knn')
lines(Pkknntest,col='blue')
legend("topright",c("Actual value","Predict value"),col=c("black","blue"),
       text.col=c("black","blue"),lty=c(1,1))

