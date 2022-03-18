#Required packages
library(tidyverse)
library(lubridate)
library(quantmod)
library(class)
library(zoo)
library(xts)

########################### STEP1: IMPORT STOCK DATA ###########################

#Downloads SPY data from yahoo finance from 2006 to today
stock <- data.frame(getSymbols(Symbols = "SPY", from = "2006-01-01", auto.assign = FALSE))

############################## STEP 2: CLEAN DATA ##############################

#Checks for Na values. If none, proceed
sum(is.na(stock))

########################### STEP3: DATA MANIPULATION ###########################

# To make life easier, Change date from index to its own column

#Save the index (currently in Dates)
SPY.Date <- as.Date(rownames(stock), format = "%Y-%m-%d")

#Add dates into a separate column
stock <- add_column(stock, SPY.Date, .before = "SPY.Open")


#opens some memory and delete temporary date
rm(SPY.Date)

# Add percent gain or loss

#Add percent gain or loss column
stock$SPY.Per_Rent <- 100*((stock$SPY.Adjusted/lag(stock$SPY.Adjusted))-1)

#Add lagged Adjusted Close

#lags1to5
stock$Lag1 <- lag(stock$SPY.Per_Rent,1)
stock$Lag2 <- lag(stock$SPY.Per_Rent,2)
stock$Lag3 <- lag(stock$SPY.Per_Rent,3)
stock$Lag4 <- lag(stock$SPY.Per_Rent,4)
stock$Lag5 <- lag(stock$SPY.Per_Rent,5)

#Add indicator for stock movement

#Up for a positive return and down for any non-positive return
stock$Direction <- ifelse(stock$SPY.Per_Rent>0,"Up","Down")

#Add daily range

#Daily range is the difference between high and low for a day
stock$SPY.Range <- stock$SPY.High-stock$SPY.Low

#Remove Na's from Manipulation process

#The first 5 rows contain NA values, so remove them
stock <- stock[-c(1:6),]

#Reset index names
rownames(stock) <- 1:nrow(stock)

############################## STEP 4: SPLIT DATA ##############################
#Set seed for repeatable results
set.seed(1)

#Number of rows for sample
#Take sample of <= 80% of data, no replacement
n <- nrow(stock)
Shuffle = sort(sample(n,floor(.8*n)))

#80% of stock in train, 20% of stock in test (randomly selected)
train <- stock[Shuffle,]
test <- stock[-Shuffle,]

write.csv(train,"./Stock/SPY_train.csv")
write.csv(test,"./Stock/SPY_test.csv")

rm(stock, test, train, n, Shuffle)







