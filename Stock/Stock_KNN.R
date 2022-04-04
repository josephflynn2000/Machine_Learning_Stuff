#----Step 1: Import Stock Data ------------------------------------------------#

#import stock data 
train <- read.csv("./Stock/SPY_train.csv")
test <- read.csv("./Stock/SPY_test.csv")


#----Step 2: Check for Missing Values -----------------------------------------#
print(sum(is.na(train)))
print(sum(is.na(test)))

#----Step 3: Some Visuals------------------------------------------------------#
head(train)
plot(train$Lag1,train$Lag2,col=as.factor(train$Direction))


train.X = cbind(train$Lag1,train$Lag2,train$Lag3)
train.Y = train$Direction

test.X = cbind(test$Lag1,test$Lag2,test$Lag3)
test.Y = test$Direction

knn.pred=knn(train.X,test.X,train.Y,k=72)
a = table(knn.pred,test.Y)
(a[1,1]+a[2,2])/sum(a)

b <- tune.knn(as.matrix(train.X), as.factor(train.Y), k = 1:100, tunecontrol = tune.control(sampling = "fix"))

