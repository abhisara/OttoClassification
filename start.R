#Load the files 
setwd('D:/Userfiles/asarapure/Documents/RStudio/Projects/OttoClassification')
train = read.csv('train.csv')
test = read.csv('test.csv')
sub = read.csv('sampleSubmission.csv')


#Load the libraries 

library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caret)
library(nnet)
library(e1071)



for (j in 0:2){
  for( i in 0:3){
    numTrees = 100 + ( 10 * i)
    ndSize = 1 + (1 * j)
    spl = sample.split(train$target , SplitRatio = 0.4)
    train.data = subset(train, spl == TRUE)
    test.data = subset(train,  spl == FALSE)
    
    rf.Model = randomForest(target ~ . , data = train.data[,-1] , mtry = 40 , nodesize = 5, ntree = 101)
    rf.Pred = predict(rf.Model , newdata = test.data[,-1])
    
    rocr.pred = prediction(predTrees , test.data$target)
    auc = as.numeric(performance(rocr.pred, 'auc')@y.values)
    RMSES_sum = sum(sqrt((rf.Pred - test.data$target)^2))
    print(paste('RMSE is ', RMSES_sum, ' and auc is ' , auc))
  }
}




spl = sample.split(train$target , SplitRatio = 0.7)
train.data = subset(train, spl == TRUE)
test.data = subset(train , spl == FALSE)


rf.Model = randomForest(target ~ . , data = train.data[,-1] , mtry = 60 , nodesize = 5, ntree = 501)
rf.Pred = predict(rf.Model , newdata = test.data[,-1])
table(rf.Pred, test.data$target)

#Removing id variable. 
train = train[,-1]
tuneRF(train.data[,-94], train.data[,94] , ntreeTry = 50, stepFactor = 2  ,
              trace = TRUE , plot = TRUE , doBest = FALSE)


#Neural network 

nnet.m = nnet(target ~ . , data = train.data[,-1] , size = 9 , MaxNWts = 10000, maxit = 200)
#size is the number of units in the hidden layer. maxit is the number of iterations. 
#I am guessing size should somewhat be close to the number of classes for all classes
# to be predicted. 
net.pred = predict(nnet.m , test.data[,-1], type = 'class')



#SVM


svm.m = svm(target ~ . , data= train.data[,-1] , type = 'C-classification' , kernel = 'linear')
svm.pred= predict(svm.m , newdata = test.data[,-1])

#svm does a good job for some of the classes. 




#Creating the final submission model.
wideData = function(predFile){
  pred.df = as.data.frame(predFile)
  pred.df$id = sub$id
  pred.df$Class_1 = ifelse(pred.df[,1] == 'Class_1' , 1, 0)
  pred.df$Class_2 = ifelse(pred.df[,1] == 'Class_2' , 1, 0)
  pred.df$Class_3 = ifelse(pred.df[,1] == 'Class_3' , 1, 0)
  pred.df$Class_4 = ifelse(pred.df[,1] == 'Class_4' , 1, 0)
  pred.df$Class_5 = ifelse(pred.df[,1] == 'Class_5' , 1, 0)
  pred.df$Class_6 = ifelse(pred.df[,1] == 'Class_6' , 1, 0)
  pred.df$Class_7 = ifelse(pred.df[,1] == 'Class_7' , 1, 0)
  pred.df$Class_8 = ifelse(pred.df[,1] == 'Class_8' , 1, 0)
  pred.df$Class_9 = ifelse(pred.df[,1] == 'Class_9' , 1, 0)
  return(pred.df[,-1])
  
}


write.csv(rf.Pred.df , 'sub2.csv', row.names = FALSE , quote = FALSE)
