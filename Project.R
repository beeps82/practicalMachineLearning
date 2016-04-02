data<- read.csv(file="C:/Users/Acer User/Documents/R/pml-training.csv");
library(ggplot2)
library(lattice)
library(AppliedPredictiveModeling)
library(caret)
library(rpart)
library(corrplot)

set.seed(1250)

#foldTrain<- createFolds(y = data$classe, k= 10,list = TRUE,returnTrain = TRUE)
#foldTest<- createFolds(y = data$classe, k= 10,list = TRUE, returnTrain = FALSE)

# Pre-processing

tmp<- training[,!nsV$nzv]

data$classe = factor(data$classe)

siz = dim(data)
data<- data[,8:siz[2]]

nsV<- nearZeroVar(data,saveMetrics=TRUE)
naCols<- colSums (is.na(data), na.rm = FALSE) > 0.5*siz[1]

# Near Zero covarient columns
cols<- colnames(data[nsV$nzv])

preData<- data[,(!naCols & !nsV$nzv)]

corrplot(cor(preData[,-53]), method = "circle",type = 'upper')

pcaData<- preProcess(preData[,-53],method ='pca', thresh =0.8, na.rm = TRUE)



descrCor<-  cor(preData[,-53])
highCorr<- sum(abs(descrCor[upper.tri(descrCor)]) > .95)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)





highCorr<- sum(abs(prds[upper.tri(prds)]) > .999)
corrplot(preData[,-53], method = "number")

inTrain<- createDataPartition(y=preData$classe, p=0.75,list = FALSE);
training<- preData[inTrain,];
testing<- preDdata[-inTrain,];




pp_hpc <- preProcess(training, method = c("center", "scale"))


model <- train(training$classe ~ ., # Survived is a function of the variables we decided to include
               data = training, # Use the trainSet dataframe as the training data
               method = "rf",# Use the "random forest" algorithm
               trControl = trainControl(method = "cv", # Use cross-validation
                                        number = 10) # Use 5 folds for cross-validation