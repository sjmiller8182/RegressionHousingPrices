suppressWarnings(library(tidyverse))

train <- read_csv('../data/train.csv')

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
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- mean(train$GarageYrBlt)
train$GarageFinish[is.na(train$GarageFinish)] <- "None"
train$GarageQual[is.na(train$GarageQual)] <- "None"
train$GarageCond[is.na(train$GarageCond)] <- "None"
train$PoolQC[is.na(train$PoolQC)] <- "None"
train$Fence[is.na(train$Fence)] <- "None"
train$MiscFeature[is.na(train$MiscFeature)] <- "None"

save(train,file = "../data/data.RData")
