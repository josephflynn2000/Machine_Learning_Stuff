#----Import Stock Data --------------------------------------------------------#

#import stock data from CSV file format
train <- read.csv("./Stock/SPY_train.csv")
test <- read.csv("./Stock/SPY_test.csv")

full <- read.csv("./Stock/SPY_full.csv")

#----Simple Linear Regression -------------------------------------------------#
#----Step 1: 