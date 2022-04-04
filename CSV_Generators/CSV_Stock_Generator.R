#Required packages
library(tidyverse)
library(tidyquant)

########################### STEP1: IMPORT STOCK DATA ###########################

#Downloads SPY data from yahoo finance from 2006 to today
stock <- tq_get("SPY", get = "stock.prices")

############################## STEP 2: CLEAN DATA ##############################

#Checks for Na values. If none, proceed
check <- sum(is.na(stock))

if(check != 0){
  print("PROBLEM!!!")
}else{
  rm(check)
}

#Remove symbol since we are working with one stock
stock <- stock[,-grep("symbol",colnames(stock))]

#Removes duplicates
stock <- stock[!duplicated(stock$date),]

########################### STEP3: DATA MANIPULATION ###########################

# Add percent gain or loss

#Add percent gain or loss column (Daily Rate of Return)
stock$daily_ror <- 100*((stock$adjusted/lag(stock$adjusted))-1)

#Add lagged Daily ROR

#lags1to5
stock$lag1 <- lag(stock$daily_ror,1)
stock$lag2 <- lag(stock$daily_ror,2)
stock$lag3 <- lag(stock$daily_ror,3)
stock$lag4 <- lag(stock$daily_ror,4)
stock$lag5 <- lag(stock$daily_ror,5)

#Add indicator for stock movement

#Up for a positive return and down for any non-positive return
stock$direction <- ifelse(stock$daily_ror>0,"Up","Down")

#Add daily range

#Daily range is the difference between high and low for a day
stock$range <- stock$high-stock$low

#Remove Na's from Manipulation process

#Removes rows that contain NA values
stock <- na.omit(stock)

#Write csv (full data)
write.csv(stock,"./Stock/SPY_full.csv", row.names = FALSE)

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

write.csv(train,"./Stock/SPY_train.csv", row.names = FALSE)
write.csv(test,"./Stock/SPY_test.csv", row.names = FALSE)

rm(stock, full, test, train, n, Shuffle)







