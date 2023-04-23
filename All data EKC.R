###### The industrial wastewater discharge and economic development 
######   in the Yellow River basin are calculated.
###### The whole Yellow River basin and different cities were compared
library(readxl)
citynoNAlm <- read_excel("citydataGDP.xls")
yylm<-log(citynoNAlm$ylm)

citynoNAlm<-as.data.frame(citynoNAlm)
citynoNAlm$y<-yylm
GDPdata<-citynoNAlm[,-2]
gdpdata<-citynoNAlm[,-1]

gdpdata$gdp2<-(gdpdata$gdp)^2
GDPdata$GDP2<-(GDPdata$GDP)^2
library(caret)
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

set.seed(111111)
lmFit1<- train(y~GDP+GDP2,data=GDPdata,
                method = "lm", 
                trControl = fitControl,
                verbose = FALSE,
                tuneLength=10)

lmFit1
summary(lm(y~GDP+GDP2,data=GDPdata))
gdplmFit1<- train(y~gdp+gdp2,data=gdpdata,
               method = "lm", 
               trControl = fitControl,
               verbose = FALSE,
               tuneLength=10)

gdplmFit1
summary(lm(y~gdp+gdp2,data=gdpdata))
############### GDP and IWD all Yellow River
sumyy <- read_excel("citydatasum.xls")

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

sumyy$GDP2<-(sumyy$GDP)^2
sumyy$gdp2<-(sumyy$gdp)^2

####plot the graph
plot(sumyy$GDP,sumyy$yy,type = 'o',col='blue',main='GDP')
plot(sumyy$gdp,sumyy$yy,type = 'o',col='red',main='gdp')

##### fit the linear model
sumGDPFit<- train(log(yy)~GDP+GDP2,data=sumyy,
                  method = "lm", 
                  trControl = fitControl,
                  verbose = FALSE,
                  tuneLength=10)

sumGDPFit
summary(lm(log(yy)~GDP+GDP2,data=sumyy))

sumgdpFit<- train(log(yy)~gdp+gdp2,data=sumyy,
                  method = "lm", 
                  trControl = fitControl,
                  verbose = FALSE,
                  tuneLength=10)

sumgdpFit
summary(lm(log(yy)~gdp+gdp2,data=sumyy))


