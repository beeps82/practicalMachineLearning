library(ggplot2)
library(lattice)
library(AppliedPredictiveModeling)
library(caret)
library(rpart)
library(corrplot)
library(randomForest)

set.seed(1250)

#foldTrain<- createFolds(y = data$classe, k= 10,list = TRUE,returnTrain = TRUE)
#foldTest<- createFolds(y = data$classe, k= 10,list = TRUE, returnTrain = FALSE)


data<- read.csv(file="C:/Users/Acer User/Documents/coursera/PracticalMachineLearning/pml-training.csv");


# Pre-processing

#tmp<- training[,!nsV$nzv]

data$classe = factor(data$classe)

siz = dim(data)
data<- data[,8:siz[2]]

nsV<- nearZeroVar(data,saveMetrics=TRUE)
naCols<- colSums (is.na(data), na.rm = FALSE) > 0.5*siz[1]

# Near Zero covarient columns
cols<- colnames(data[nsV$nzv])

preData<- data[,(!naCols & !nsV$nzv)]

corrplot(cor(preData[,-53]), method = "circle",type = 'upper')


#descrCor<-  cor(preData[,-53])
#highCorr<- sum(abs(descrCor[upper.tri(descrCor)]) > .95)
#highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)


inTrain<- createDataPartition(y=preData$classe, p=0.75,list = FALSE);
training<- preData[inTrain,];
validation<- preData[-inTrain,];

pcaData<- preProcess(training[,-53],method ='pca', thresh =0.8, na.rm = TRUE)
trainData<- predict(pcaData,training[,-53])

 # Use the trainSet dataframe as the training data
# Use cross-validation
trainModel<- train(training$classe ~ .,data = trainData, method = "rf",verbose = 'TRUE',# Use the "random forest" algorithm
              trControl = trainControl(method = "cv",number = 10)); # Use 10 folds for cross-validation
               

trainResults<- predict(trainModel,newData = trainData);

validationData<- predict(pcaData,validation[,-53]);
validationResults<- predict(trainModel,newData = validationData);

confusionMatrix(training$classe,trainingResults)
confusionMatrix(validation$classe,validationResults)



finData<- read.csv(file="C:/Users/Acer User/Documents/coursera/PracticalMachineLearning/pml-testing.csv");
finData<- finData[,8:siz[2]]
finData<- finData[,(!naCols & !nsV$nzv)]

finalPCA<- predict(pcaData,finData[,-53]);
finalRes<- predict(trainModel,newData = finalPCA)
#confusionMatrix(finalData$classe,)


               
               