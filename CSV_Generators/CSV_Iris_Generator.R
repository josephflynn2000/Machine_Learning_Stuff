#Sets iris into environment (dataframe)
iris <- iris

#Sets seed for repeatable results
set.seed(1)

#Number of rows for sample
#Take sample of <= 80% of data, no replacement
n <- nrow(iris)
Shuffle <- sort(sample(n,floor(.8*n)))

#80% of iris in train, 20% of iris in test (randomly selected)
train <- iris[Shuffle,]
test <- iris[-Shuffle,]

#Full iris data is within R and dosen't need to be turned into a csv

#Writes data to csv files
write.csv(train, "./Iris/Iris_train.csv")
write.csv(test, "./Iris/Iris_test.csv")

#Removes all variables used in file
rm(iris, test, train, n, Shuffle)