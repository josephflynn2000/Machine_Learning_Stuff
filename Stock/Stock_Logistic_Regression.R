#----Import Stock Data --------------------------------------------------------#

#import stock data from CSV file format
train <- read.csv("./Stock/SPY_train.csv")
test <- read.csv("./Stock/SPY_test.csv")

full <- read.csv("./Stock/SPY_full.csv")

#----Check for Missing Values -------------------------------------------------#

print(sum(is.na(train)))
print(sum(is.na(test)))


log <- glm(formula = as.factor(direction) ~ lag1 + lag2 + lag3 + lag4 + lag5 + volume, family = binomial ,data=full)
log2 <- glm(formula = as.factor(direction) ~ lag1 + lag3 + lag4 + lag5 + volume, family = binomial ,data=full)
log3 <- glm(formula = as.factor(direction) ~ lag1 + lag3 + lag4 + volume, family = binomial ,data=full)
p <- predict(log2, type = "response")
