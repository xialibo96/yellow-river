
#########Variable selection
##preprocessing
library(caret)
library(readxl)
citydata <- read_excel("citydata.xls")
### x42 is the industrial wastewater discharge,
###  and logarithm of industrial wastewater discharge.
yy<-log(citydata$x42)
hist(yy)
citydata<-citydata[,-42]
library(caret)

citydata<-as.data.frame(citydata)
set.seed(22222)

#####Remove the Multicollinearity
descrCorr <- cor(citydata)
highCorr <- findCorrelation(descrCorr, 0.90)
newdata <- citydata[, -highCorr]
Process <- preProcess(newdata)
newdata2 <- predict(Process, newdata)
subsets <- c(1:36)

###### rfFuncs  treebagFuncs  lmFuncs
###### treebagFuncs
ctrl<-  rfeControl(functions = treebagFuncs, method = 'cv',
                   verbose = FALSE, returnResamp = 'final')

subsets <- c(1:36)
treelmProfile <- rfe(x = newdata2, y = yy,
                     sizes = subsets,
                     rfeControl = ctrl)

plot(treelmProfile)
##### lmFuncs
ctrl <- rfeControl(functions = lmFuncs, method = 'cv',
                   verbose = FALSE, returnResamp = 'final')

lmProfile <- rfe(x = newdata2, y = yy,
                 sizes = subsets,
                 rfeControl = ctrl)

plot(lmProfile)

######rfFuncs
ctrl <- rfeControl(functions = rfFuncs, method = 'cv',
                   verbose = FALSE, returnResamp = 'final')

rfProfile <- rfe(x = newdata2, y = yy,
                 sizes = subsets,
                 rfeControl = ctrl)

plot(rfProfile)

