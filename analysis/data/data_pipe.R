

# Load utils
## quietly load library for reading csv files
suppressMessages(library(tidyverse))

# get test data

## load the training data set

train <- read_csv('../data/train.csv')

## clean the training data

### nominal imputation

train$GarageYrBlt[is.na(train$GarageYrBlt)] <- 0
train$LotFrontage[is.na(train$LotFrontage)] <- mean(train$LotFrontage, na.rm=TRUE)
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0
train$Alley[is.na(train$Alley)] <- 'None'
train$MasVnrType[is.na(train$MasVnrType)] <- 'None'
train$BsmtQual[is.na(train$BsmtQual)] <- 'None'
train$BsmtCond[is.na(train$BsmtCond)] <- 'None'
train$BsmtExposure[is.na(train$BsmtExposure)] <- 'None'
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- 'None'
train$BsmtFinType2[is.na(train$BsmtFinType2)] <- 'None'
train$FireplaceQu[is.na(train$FireplaceQu)] <- 'None'
train$GarageType[is.na(train$GarageType)] <- 'None'
train$GarageFinish[is.na(train$GarageFinish)] <- 'None'
train$GarageQual[is.na(train$GarageQual)] <- 'None'
train$GarageCond[is.na(train$GarageCond)] <- 'None'
train$PoolQC[is.na(train$PoolQC)] <- 'None'
train$Fence[is.na(train$Fence)] <- 'None'
train$MiscFeature[is.na(train$MiscFeature)] <- 'None'
train$Electrical[is.na(train$Electrical)] <- 'SBrkr'
train$BldgType[is.na(train$BldgType)] <- '1Fam'
train$BsmtExposure[is.na(train$BsmtExposure)] <- 'None'

### factorize

train$Neighborhood <- as.factor(train$Neighborhood)
train$BldgType <- as.factor(train$BldgType)
train$HouseStyle <- as.factor(train$HouseStyle)
train$RoofStyle <- as.factor(train$RoofStyle)
train$RoofMatl <- as.factor(train$RoofMatl)
train$Exterior1st <- as.factor(train$Exterior1st)
train$Exterior2nd <- as.factor(train$Exterior2nd)
train$ExterQual <- as.factor(train$ExterQual)
train$ExterCond <- as.factor(train$ExterCond)
train$Foundation <- as.factor(train$Foundation)
train$Heating <- as.factor(train$Heating)
train$HeatingQC <- as.factor(train$HeatingQC)
train$CentralAir <- as.factor(train$CentralAir)
train$KitchenQual <- as.factor(train$KitchenQual)
train$Functional <- as.factor(train$Functional)
train$PavedDrive <- as.factor(train$PavedDrive)
train$SaleType <- as.factor(train$SaleType)
train$Utilities <- as.factor(train$Utilities)

# The MSSubClass variable appears numeric but should be treated as a factor
# 150 forced to another choice as this wasn't found in the training data set
train$MSSubClass <- dplyr::recode(train$MSSubClass,
                                 `30` = "30F",
                                 `180` = "180F",
                                 `45` = "45F",
                                 `190` = "190F",
                                 `90` = "190F",
                                 `160` = "160F",
                                 `50` = "50F",
                                 `40` = "40F", 
                                 `85` = "85F",
                                 `70` = "70F",
                                 `80` = "80F",
                                 `20` = "20F", 
                                 `75` = "75F", 
                                 `120` = "120F",
                                 `60` = "60F", 
                                 `150` = "75F")
train$MSSubClass <- as.factor(train$MSSubClass)

### recode/order factors

train$GarageQual <- dplyr::recode(train$GarageQual,
                                 'None' = 0,
                                 'Po' = 1,
                                 'Fa' = 2,
                                 'TA' = 3,
                                 'Gd' = 4,
                                 'Ex' = 5)
train$GarageFinish <- dplyr::recode(train$GarageFinish,
                                   'None' = 0,
                                   'Unf' = 1,
                                   'RFn' = 2,
                                   'Fin' = 3)
train$GarageType <- dplyr::recode(train$GarageType,
                                 'None' = 0,
                                 'CarPort' = 1,
                                 '2Types' = 2, 
                                 'Basment' = 3,
                                 'Detchd' = 4,
                                 'Attchd' = 5,
                                 'BuiltIn' = 6)
train$BsmtQual <- dplyr::recode(train$BsmtQual,
                               'None' = 0,
                               'Po' = 1,
                               'Fa' = 2,
                               'TA' = 3,
                               'Gd' = 4,
                               'Ex' = 5)
train$BsmtFinType1 <- dplyr::recode(train$BsmtFinType1,
                                   'None' = 0,
                                   'Unf' = 1,
                                   'LwQ' = 2,
                                   'Rec' = 3, 
                                   'BLQ' = 4, 
                                   'ALQ' = 5,
                                   'GLQ' = 6)
train$BsmtFinType2 <- dplyr::recode(train$BsmtFinType2,
                                   'None' = 0,
                                   'Unf' = 1,
                                   'LwQ' = 2,
                                   'Rec' = 3, 
                                   'BLQ' = 4,
                                   'ALQ' = 5,
                                   'GLQ' = 6)
train$BsmtExposure <- dplyr::recode(train$BsmtExposure,
                                   'None' = 0,
                                   'No' = 1, 
                                   'Mn' = 2, 
                                   'Av' = 3, 
                                   'Gd' = 4)
train$LandSlope <- dplyr::recode(train$LandSlope,
                                'Sev' = 0, 
                                'Mod' = 1, 
                                'Gtl' = 2)
train$MSZoning <- dplyr::recode(train$MSZoning,
                               'C (all)' = 0,
                               'RM' = 1, 
                               'RH' = 2, 
                               'RL' = 3,
                               'FV' = 4)
train$PoolQC <- dplyr::recode(train$PoolQC,
                             'None' = 0,
                             'Po' = 1, 
                             'Fa' = 2, 
                             'TA' = 3, 
                             'Gd' = 4, 
                             'Ex' = 5)
train$FireplaceQu <- dplyr::recode(train$FireplaceQu, 
                                  'None' = 0,
                                  'Po' = 1,
                                  'Fa' = 2,
                                  'TA' = 3, 
                                  'Gd' = 4,
                                  'Ex' = 5)
train$Street <- ordered(train$Street, levels = c("Grvl",
                                                 "Pave"))
train$Street <- ordered(train$Street, levels = c("Grvl", 
                                               "Pave"))

### drop off highly correlated variables

#1)  Remove Basement Condition as it is too correlated with Basement Quality
#2)  Remove Garage Condition as it is too correlated with Garage Quality
#3)  Remove utilities from the train/test data sets as it doesn't have enough observations in each the 2 levels

train = subset(train, select = -c(Utilities, BsmtCond, GarageCond) )




# get test data

## load the training data set

test <- read_csv('../data/test.csv')

## clean the training data

### nominal imputation

test$GarageYrBlt[is.na(test$GarageYrBlt)] <- 0
test$LotFrontage[is.na(test$LotFrontage)] <- mean(test$LotFrontage, na.rm=TRUE)
test$MasVnrArea[is.na(test$MasVnrArea)] <- 0
test$Alley[is.na(test$Alley)] <- 'None'
test$MasVnrType[is.na(test$MasVnrType)] <- 'None'
test$BsmtQual[is.na(test$BsmtQual)] <- 'None'
test$BsmtCond[is.na(test$BsmtCond)] <- 'None'
test$BsmtExposure[is.na(test$BsmtExposure)] <- 'None'
test$BsmtFinType1[is.na(test$BsmtFinType1)] <- 'None'
test$BsmtFinType2[is.na(test$BsmtFinType2)] <- 'None'
test$FireplaceQu[is.na(test$FireplaceQu)] <- 'None'
test$GarageType[is.na(test$GarageType)] <- 'None'
test$GarageFinish[is.na(test$GarageFinish)] <- 'None'
test$GarageQual[is.na(test$GarageQual)] <- 'None'
test$GarageCond[is.na(test$GarageCond)] <- 'None'
test$PoolQC[is.na(test$PoolQC)] <- 'None'
test$Fence[is.na(test$Fence)] <- 'None'
test$MiscFeature[is.na(test$MiscFeature)] <- 'None'
test$Electrical[is.na(test$Electrical)] <- 'SBrkr'
test$BldgType[is.na(test$BldgType)] <- '1Fam'
test$BsmtExposure[is.na(test$BsmtExposure)] <- 'None'

### factorize

test$Neighborhood <- as.factor(test$Neighborhood)
test$BldgType <- as.factor(test$BldgType)
test$HouseStyle <- as.factor(test$HouseStyle)
test$RoofStyle <- as.factor(test$RoofStyle)
test$RoofMatl <- as.factor(test$RoofMatl)
test$Exterior1st <- as.factor(test$Exterior1st)
test$Exterior2nd <- as.factor(test$Exterior2nd)
test$ExterQual <- as.factor(test$ExterQual)
test$ExterCond <- as.factor(test$ExterCond)
test$Foundation <- as.factor(test$Foundation)
test$Heating <- as.factor(test$Heating)
test$HeatingQC <- as.factor(test$HeatingQC)
test$CentralAir <- as.factor(test$CentralAir)
test$KitchenQual <- as.factor(test$KitchenQual)
test$Functional <- as.factor(test$Functional)
test$PavedDrive <- as.factor(test$PavedDrive)
test$SaleType <- as.factor(test$SaleType)
test$Utilities <- as.factor(test$Utilities)

# The MSSubClass variable appears numeric but should be treated as a factor
# 150 forced to another choice as this wasn't found in the training data set
test$MSSubClass <- dplyr::recode(test$MSSubClass,
                                 `30` = "30F",
                                 `180` = "180F",
                                 `45` = "45F",
                                 `190` = "190F",
                                 `90` = "190F",
                                 `160` = "160F",
                                 `50` = "50F",
                                 `40` = "40F", 
                                 `85` = "85F",
                                 `70` = "70F",
                                 `80` = "80F",
                                 `20` = "20F", 
                                 `75` = "75F", 
                                 `120` = "120F",
                                 `60` = "60F", 
                                 `150` = "75F")
test$MSSubClass <- as.factor(test$MSSubClass)

### recode/order factors

test$GarageQual <- dplyr::recode(test$GarageQual,
                                 'None' = 0,
                                 'Po' = 1,
                                 'Fa' = 2,
                                 'TA' = 3,
                                 'Gd' = 4,
                                 'Ex' = 5)
test$GarageFinish <- dplyr::recode(test$GarageFinish,
                                   'None' = 0,
                                   'Unf' = 1,
                                   'RFn' = 2,
                                   'Fin' = 3)
test$GarageType <- dplyr::recode(test$GarageType,
                                 'None' = 0,
                                 'CarPort' = 1,
                                 '2Types' = 2, 
                                 'Basment' = 3,
                                 'Detchd' = 4,
                                 'Attchd' = 5,
                                 'BuiltIn' = 6)
test$BsmtQual <- dplyr::recode(test$BsmtQual,
                               'None' = 0,
                               'Po' = 1,
                               'Fa' = 2,
                               'TA' = 3,
                               'Gd' = 4,
                               'Ex' = 5)
test$BsmtFinType1 <- dplyr::recode(test$BsmtFinType1,
                                   'None' = 0,
                                   'Unf' = 1,
                                   'LwQ' = 2,
                                   'Rec' = 3, 
                                   'BLQ' = 4, 
                                   'ALQ' = 5,
                                   'GLQ' = 6)
test$BsmtFinType2 <- dplyr::recode(test$BsmtFinType2,
                                   'None' = 0,
                                   'Unf' = 1,
                                   'LwQ' = 2,
                                   'Rec' = 3, 
                                   'BLQ' = 4,
                                   'ALQ' = 5,
                                   'GLQ' = 6)
test$BsmtExposure <- dplyr::recode(test$BsmtExposure,
                                   'None' = 0,
                                   'No' = 1, 
                                   'Mn' = 2, 
                                   'Av' = 3, 
                                   'Gd' = 4)
test$LandSlope <- dplyr::recode(test$LandSlope,
                                'Sev' = 0, 
                                'Mod' = 1, 
                                'Gtl' = 2)
test$MSZoning <- dplyr::recode(test$MSZoning,
                               'C (all)' = 0,
                               'RM' = 1, 
                               'RH' = 2, 
                               'RL' = 3,
                               'FV' = 4)
test$PoolQC <- dplyr::recode(test$PoolQC,
                             'None' = 0,
                             'Po' = 1, 
                             'Fa' = 2, 
                             'TA' = 3, 
                             'Gd' = 4, 
                             'Ex' = 5)
test$FireplaceQu <- dplyr::recode(test$FireplaceQu, 
                                  'None' = 0,
                                  'Po' = 1,
                                  'Fa' = 2,
                                  'TA' = 3, 
                                  'Gd' = 4,
                                  'Ex' = 5)
train$Street <- ordered(train$Street, levels = c("Grvl",
                                                 "Pave"))
test$Street <- ordered(test$Street, levels = c("Grvl", 
                                               "Pave"))

### drop off highly correlated variables

#1)  Remove Basement Condition as it is too correlated with Basement Quality
#2)  Remove Garage Condition as it is too correlated with Garage Quality
#3)  Remove utilities from the train/test data sets as it doesn't have enough observations in each the 2 levels

test = subset(test, select = -c(Utilities, BsmtCond, GarageCond) )

# save the cleaned data to an image
save(train, test, file = "../data/data.RData")
