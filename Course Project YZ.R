install.packages("randomForest")
library(caret)
library(AppliedPredictiveModeling)
library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(ggplot2)
library(corrplot)
library(gbm)

training <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),header=TRUE)

dim(training)

valid_in <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),header=TRUE)

dim(valid_in)

str(training)

trainData<-training[,colSums(is.na(training))==0]
validData<-valid_in[,colSums(is.na(testing))==0]

dim(trainData)

dim(validData)

trainData<-trainData[,-c(1:7)]
validData<-validData[,-c(1:7)]
dim(trainData)
dim(validData)

set.seed(1234)

inTrain<-createDataPartition(trainData$classe,p=0.7,list=FALSE)
trainData<-trainData[inTrain,]
testData<-trainData[-inTrain,]
dim(trainData)
dim(testData)

NZV<-nearZeroVar(trainData)
trainData<-trainData[,-NZV]
testData<-testData[,-NZV]
dim(trainData)
dim(testData)

cor_mat<-cor(trainData[,-53])
corrplot(cor_mat,order="FPC", method="color", type="lower",t1.cex=0.8,t1.col=rgb(0,0,0))
highlyCorrelated=findCorrelation(cor_mat,cutoff=0.75)
names(trainData)[highlyCorrelated]

set.seed(12345)
decisionTreeMod1<-rpart(classe~.,data=trainData,method="class")
fancyRpartPlot(decisionTreeMod1)

predictTreeMod1<-predict(decisionTreeMod1,testData,type="class")
cmtree<-confusionMatrix(predictTreeMod1,testData$classe)
cmtree


plot(cmtree$table, col = cmtree$byClass, 
     main = paste("Decision Tree - Accuracy =", round(cmtree$overall['Accuracy'], 4)))

controlRF<-trainControl(method="cv",number=3,verboseIter=FALSE)
modRF1<-train(classe~.,data=trainData,method="rf",trControl=controlRF)
modRF1$finalModel

predictRF1<-predict(modRF1,newdata=testData)
cmrf<-confusionMatrix(predictRF1,testData$classe)
cmrf

plot(modRF1)

plot(cmrf$table, col = cmrf$byClass, main = paste("Random Forest Confusion Matrix: Accuracy =", round(cmrf$overall['Accuracy'], 4)))

set.seed(12345)
controlGBM<-trainControl(method="repeatedcv", number=5, repeats=1)
modGBM<-train(classe~.,data=trainData,method="gbm",trControl=controlGBM,verbose=FALSE)
modGBM$finalModel

print(modGBM)

predictGBM<-predict(modGBM,newdata=testData)
cmGBM<-confusionMatrix(predictGBM,testData$classe)
cmGBM


Results<-predict(modRF1,newdata=validData)
Results






