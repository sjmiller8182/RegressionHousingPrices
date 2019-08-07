
# quietly load library for reading csv files
suppressMessages(library(tidyverse))
# load the training data set
train <- read_csv('../data/train.csv')
# clean the training data
train$LotFrontage[is.na(train$LotFrontage)] <- 0
train$Alley[is.na(train$Alley)] <- "None"
train$MasVnrType[is.na(train$MasVnrType)] <- "None"
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0
train$BsmtQual[is.na(train$BsmtQual)] <- 0
train$BsmtCond[is.na(train$BsmtCond)] <- 0
train$BsmtExposure[is.na(train$BsmtExposure)] <- 0
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- 0
train$BsmtFinType2[is.na(train$BsmtFinType2)] <- 0
train$Electrical[is.na(train$Electrical)] <- "SBrkr"
train$FireplaceQu[is.na(train$FireplaceQu)] <- "None"
train$GarageType[is.na(train$GarageType)] <- "None"
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- mean(train$GarageYrBlt, na.rm=TRUE)
train$GarageFinish[is.na(train$GarageFinish)] <- "None"
train$GarageQual[is.na(train$GarageQual)] <- "None"
train$GarageCond[is.na(train$GarageCond)] <- "None"
train$PoolQC[is.na(train$PoolQC)] <- "None"
train$Fence[is.na(train$Fence)] <- "None"
train$MiscFeature[is.na(train$MiscFeature)] <- "None"

# load the testing data set
test <- read_csv('../data/test.csv')
# clean the testing data
test$LotFrontage[is.na(test$LotFrontage)] <- 0
test$Alley[is.na(test$Alley)] <- "None"
test$MasVnrType[is.na(test$MasVnrType)] <- "None"
test$MasVnrArea[is.na(test$MasVnrArea)] <- 0
test$BsmtQual[is.na(test$BsmtQual)] <- 0
test$BsmtCond[is.na(test$BsmtCond)] <- 0
test$BsmtExposure[is.na(test$BsmtExposure)] <- 0
test$BsmtFinType1[is.na(test$BsmtFinType1)] <- 0
test$BsmtFinType2[is.na(test$BsmtFinType2)] <- 0
test$Electrical[is.na(test$Electrical)] <- "SBrkr"
test$FireplaceQu[is.na(test$FireplaceQu)] <- "None"
test$GarageType[is.na(test$GarageType)] <- "None"
test$GarageYrBlt[is.na(test$GarageYrBlt)] <- mean(test$GarageYrBlt, na.rm=TRUE)
test$GarageArea[is.na(test$GarageArea)] <- mean(test$GarageArea, na.rm=TRUE)
test$GarageFinish[is.na(test$GarageFinish)] <- "None"
test$GarageQual[is.na(test$GarageQual)] <- "None"
test$GarageCond[is.na(test$GarageCond)] <- "None"
test$PoolQC[is.na(test$PoolQC)] <- "None"
test$Fence[is.na(test$Fence)] <- "None"
test$MiscFeature[is.na(test$MiscFeature)] <- "None"
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] <- mean(test$TotalBsmtSF, na.rm=TRUE)
test$OverallQual[is.na(test$OverallQual)] <- mean(test$OverallQual, na.rm=TRUE)
test$YearRemodAdd[is.na(test$YearRemodAdd)] <- mean(test$YearRemodAdd, na.rm=TRUE)
test$Fireplaces[is.na(test$Fireplaces)] <- mean(test$Fireplaces, na.rm=TRUE)
test$YearBuilt[is.na(test$YearBuilt)] <- mean(test$YearBuilt, na.rm=TRUE)

# save the cleaned data to an image
save(train, test, file = "../data/data.RData")
