library(ggplot2)
library(lattice)
library(AppliedPredictiveModeling)
library(caret)
library(rpart)
library(rattle)
data(segmentationOriginal)
set.seed(125)

trainSet<- segmentationOriginal[!test,]
testSet<- segmentationOriginal[test,]

cMod<- train(Class ~ .,method ='rpart',data = trainSet)
fancyRpartPlot(cMod$finalModel)


## 
library(pgmm)
data(olive)
olive = olive[,-1]

mod<-train(Area ~ . ,method = 'rpart',data = olive)

newdata = as.data.frame(t(colMeans(olive)))
predict(mod$finalModel,newdata=newdata)

##

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

tmp<- trainSA[,c('chd','age','alcohol','obesity','tobacco','typea','ldl')]
tMod<- train(chd ~ .,method='glm',family='binomial',data = tmp)
tmpTest<- testSA[,c('chd','age','alcohol','obesity','tobacco','typea','ldl')]

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(tmp$chd,predict(tMod,tmp))
missClass(tmpTest$chd,predict(tMod,tmpTest))


##

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
library(ggplot)
set.seed(33833)

mod<- train(y~., method= 'rf',data = vowel.train)
varImp(predict(mod,vowel.train)

