#Load the files 

train = read.csv('train.csv')
test = read.csv('test.csv')
sub = read.csv('sampleSubmission.csv')


#Load the libraries 

library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)



for (j in 0:2){
  for( i in 0:3){
    numTrees = 100 + ( 10 * i)
    ndSize = 1 + (1 * j)
    spl = sample.split(dtmTrain$Pop , SplitRatio = 0.7)
    train.data = subset(dtmTrain, spl == TRUE)
    test.data = subset(dtmTrain , spl == FALSE)
    
    rftrees = randomForest(as.factor(Pop) ~ NewsDesk + secName + subSName + wrdCount + weekday + isSaturday + isSunday + hour, mtry = 2,
                           ntree = numTrees , data = train.data , nodesize = ndSize)
    predTrees = predict(rftrees , newdata = test.data)
    RMSES_sum = sum(sqrt((predTrees - test.data$Pop)^2))
    print(paste('trees are ' , numTrees , 'nodesize is ' , ndSize , 'RMSE is ', RMSES_sum))
  }
}




spl = sample.split(train$target , SplitRatio = 0.7)
train.data = subset(train, spl == TRUE)
test.data = subset(train , spl == FALSE)


rf.Model = randomForest(target ~ . , data = train[-1] , mtry = 60 , nodesize = 5, ntree = 501)
rf.Pred = predict(rf.Model , newdata = test[,-1])
table(rf.Pred, test.data$target)

#Removing id variable. 
train = train[,-1]
tuneRF(train.data[,-94], train.data[,94] , ntreeTry = 50, stepFactor = 2  ,
              trace = TRUE , plot = TRUE , doBest = FALSE)




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
