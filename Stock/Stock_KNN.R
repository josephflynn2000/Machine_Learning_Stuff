library(class)
library(ggplot2)
library(reshape2)

#----Step 1: Import Stock Data ------------------------------------------------#

#import stock data from CSV file format
train <- read.csv("./Stock/SPY_train.csv")
test <- read.csv("./Stock/SPY_test.csv")

full <- read.csv("./Stock/SPY_full.csv")


#----Step 2: Check for Missing Values -----------------------------------------#
print(sum(is.na(train)))
print(sum(is.na(test)))

# should still do proper data cleaning, 
# but data cleaning was done in the CSV generator

#----Step 3: Some Visuals------------------------------------------------------#

#Scatter Plot of lag1 vs lag2
ggplot(full, aes(lag1, lag2))+
  geom_point(shape = 1, aes(color=direction))+
  scale_color_manual(values = c("Up" = "blue", "Down"="orange"))+
  ggtitle("Daily ROR for Yesterday and 2 Days Ago")+
  xlab("Daily ROR for Yesterday")+ 
  ylab("Daily ROR for 2 Days Ago")+
  labs(color = "Direction")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line())

#----Step 4(a): Data Setup (Non-Normalized)------------------------------------#

#create training variables
train.X = cbind(train$lag1,train$lag2)
train.Y = as.factor(train$direction)

#create testing variables
test.X = cbind(test$lag1,test$lag2)
test.Y = as.factor(test$direction)

#empty vector for non-normalized results
results <- c()

#test k of 1 to √(n)
for (i in 1:round(sqrt(nrow(train)))){
  knn.pred=knn(train.X,test.X,train.Y,k=i)
  a = table(knn.pred,test.Y)
  results <- append(results,(a[1,1]+a[2,2])/sum(a))
} 

#----Normalization Methods-----------------------------------------------------#
# Stock returns are assumed to follow a normal distribution,
# with a center around 0%.
# These should have little to no effect on performance

#----Step 4(b): Data Setup (Z-Score)-------------------------------------------#

#standardize train and test x values
z.train.X <- scale(train.X, center = TRUE, scale = TRUE)
z.test.X <- scale(test.X, center = TRUE, scale = TRUE)

# y values are binary, don't touch

#empty vector for standardized results
results.z <- c()

#test k of 1 to √(n)
for (i in 1:round(sqrt(nrow(train)))){
  knn.pred=knn(z.train.X, z.test.X, train.Y, k=i)
  a = table(knn.pred, test.Y)
  results.z <- append(results.z,(a[1,1]+a[2,2])/sum(a))
} 

#----Step 4(c): Data Setup (scaling)-------------------------------------------#

#scale train and test x values
scaled.train.X <- (train.X-min(train.X))/(max(train.X)-min(train.X))
scaled.test.X <- (test.X-min(test.X))/(max(test.X)-min(test.X))

# y values are binary, don't touch

#empty vector for scaled results
results.scaled <- c()

#test k of 1 to √(n)
for (i in 1:round(sqrt(nrow(train)))){
  knn.pred = knn(scaled.train.X, scaled.test.X, train.Y, k=i)
  a = table(knn.pred,test.Y)
  results.scaled <- append(results.scaled,(a[1,1]+a[2,2])/sum(a))
} 

#----Step 5: Visualize Results-------------------------------------------------#

df.results <- data.frame(base = results, stand = results.z, scale = results.scaled)

df.results$K <- as.numeric(row.names(df.results))

df.results <- df.results[,c(4,1,2,3)]

df.results <- melt(df.results ,  id.vars = 'K', variable.name = 'series')

ggplot(df.results, aes(K,value,color=variable))+
  geom_point(shape = 1, size = 2)+
  geom_line(linetype = 1)+
  ggtitle("Accuracy by K-Value for KNN")+
  xlab("K-Value")+ 
  ylab("Accuracy")+
  labs(color = "Data methods")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line())

#----Step 6: Predict Tomorrow -------------------------------------------------#
(train.X-min(train.X))/(max(train.X)-min(train.X))
sta.full <- full[,8:10]
sta.full <- (sta.full-min(sta.full))/(max(sta.full)-min(sta.full))

sta.x <- cbind(sta.full$lag1,sta.full$lag2)

tomorrow <- cbind(tail(sta.full$daily_ror,1),tail(sta.full$lag1,1))
  
knn.pred = knn(sta.x, tomorrow, as.factor(full$direction), k=8)
