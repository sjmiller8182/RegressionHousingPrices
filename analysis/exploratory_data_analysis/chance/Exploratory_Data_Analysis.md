Exploratory Data Analysis
================
Chance Robinson
August 4, 2019

  - [Setup](#setup)
  - [Exploratory Analysis](#exploratory-analysis)
      - [Load the data into R](#load-the-data-into-r)
      - [Data Cleaning](#data-cleaning)
          - [Handle null values for continuous
            variables](#handle-null-values-for-continuous-variables)
          - [Handle null values for categorical
            variables](#handle-null-values-for-categorical-variables)
          - [set ordinal factors](#set-ordinal-factors)
          - [Set normal factor variables](#set-normal-factor-variables)

# Setup

``` r
# libraries
library(knitr)
library(tidyverse)
library(naniar)
library(Hmisc)
library(GGally)
# Correlation
library(corrr)
# Forward, Backward and Stepwise Regression
library(MASS)

library(caret)

# helper files
source('../../helper/data_munging.R')
```

# Exploratory Analysis

### Load the data into R

``` r
train <- read_csv('../../data/train.csv')
test <- read_csv('../../data/test.csv')
```

## Data Cleaning

### Handle null values for continuous variables

  - The `GarageYrBlt (Garage Year Built)` was found to be null when tied
    rows with an empty Garage Type for all cases of the training data.
    It is likely that a Garage Type of NA would represent a property
    with no garage, and therefore we are setting the `GarageYrBlt` to 0.
  - The `LotFrontage` used an impute strategry with the mean value from
    each data set, as we assumed all houses should have some value for
    this data point.
  - For `MasVnrArea`, we assumed that these properties had no masonry
    veneer and replaced the NAs with 0.

<!-- end list -->

``` r
# Garage Year Built {"train": 81, "test": 78}

train$GarageYrBlt[is.na(train$GarageYrBlt)] <- 0
test$GarageYrBlt[is.na(test$GarageYrBlt)] <- 0

# Lot Frontage {"train": 259, "test": 227}

train$LotFrontage[is.na(train$LotFrontage)] <- mean(train$LotFrontage, na.rm=TRUE)
test$LotFrontage[is.na(test$LotFrontage)] <- mean(test$LotFrontage, na.rm=TRUE)


# MasVnrArea {"train": 8, "test": 15}
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0
test$MasVnrArea[is.na(test$MasVnrArea)] <- 0
```

``` r
# Reduce Neighborhood into 3 categories
train %>%
  group_by(Neighborhood) %>%
  summarise(mean = mean(log(SalePrice)), n = n()) %>%
  arrange(desc(mean))
```

    ## # A tibble: 25 x 3
    ##    Neighborhood  mean     n
    ##    <chr>        <dbl> <int>
    ##  1 NoRidge       12.7    41
    ##  2 NridgHt       12.6    77
    ##  3 StoneBr       12.6    25
    ##  4 Timber        12.4    38
    ##  5 Veenker       12.3    11
    ##  6 Somerst       12.3    86
    ##  7 ClearCr       12.2    28
    ##  8 Crawfor       12.2    51
    ##  9 Blmngtn       12.2    17
    ## 10 CollgCr       12.2   150
    ## # ... with 15 more rows

### Handle null values for categorical variables

  - Alley
  - MasVnrType
  - BsmtQual
  - BsmtCond (Removed)
  - BsmtExposure
  - BsmtFinType1
  - BsmtFinType2
  - FireplaceQu
  - GarageType
  - GarageFinish
  - GarageQual
  - GarageCond
  - PoolQC
  - Fence
  - MiscFeature
  - Electrical (SBrkr Standard Circuit Breakers & Romex)

### set ordinal factors

``` r
# Reduce Neighborhood into 3 categories
# train %>%
#   group_by(MSSubClass) %>%
#   summarise(mean = mean(log(SalePrice)), n = n()) %>%
#   arrange(mean)

# Remove utilities from dataframe as it doesn't have enough observations in the 2 levels
# Remove Basement Condition as it is highly correlated to Basement Quality
train = subset(train, select = -c(Utilities, BsmtCond, GarageCond) )
test = subset(test, select = -c(Utilities, BsmtCond, GarageCond) )

# train$MSSubClass <- dplyr::recode(train$MSSubClass, `30` = 1L, `180` = 2L, `45` = 3L, `190` = 4L, `90` = 5L, `160` = 6L, `50` = 7L, `40` = 8L, `85` = 9L, `70` = 10L,
#                                  `80` = 11L, `20` = 12L, `75` = 13L, `120` = 14L, `60` = 15L, `150` = 7L)
#  
# test$MSSubClass <- dplyr::recode(test$MSSubClass, `30` = 1L, `180` = 2L, `45` = 3L, `190` = 4L, `90` = 5L, `160` = 6L, `50` = 7L, `40` = 8L, `85` = 9L, `70` = 10L,
#                                   `80` = 11L, `20` = 12L, `75` = 13L, `120` = 14L, `60` = 15L, `150` = 7L)


train$MSSubClass <- dplyr::recode(train$MSSubClass, `30` = "30F", `180` = "180F", `45` = "45F", `190` = "190F",
                                  `90` = "190F", `160` = "160F", `50` = "50F", `40` = "40F", `85` = "85F", `70` = "70F",
                                 `80` = "80F", `20` = "20F", `75` = "75F", `120` = "120F", `60` = "60F", `150` = "75F")

test$MSSubClass <- dplyr::recode(test$MSSubClass, `30` = "30F", `180` = "180F", `45` = "45F", `190` = "190F",
                                  `90` = "190F", `160` = "160F", `50` = "50F", `40` = "40F", `85` = "85F", `70` = "70F",
                                 `80` = "80F", `20` = "20F", `75` = "75F", `120` = "120F", `60` = "60F", `150` = "75F")


train$MSSubClass <- as.factor(train$MSSubClass)
test$MSSubClass <- as.factor(test$MSSubClass)


train$BsmtQual <- dplyr::recode(train$BsmtQual, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
test$BsmtQual <- dplyr::recode(test$BsmtQual, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

# train$BsmtCond <- dplyr::recode(train$BsmtCond, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
# test$BsmtCond <- dplyr::recode(test$BsmtCond, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)


train$GarageQual <- dplyr::recode(train$GarageQual, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
test$GarageQual <- dplyr::recode(test$GarageQual, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)


# train$GarageCond <- dplyr::recode(train$GarageCond, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
# test$GarageCond <- dplyr::recode(test$GarageCond, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)


train$PoolQC <- dplyr::recode(train$PoolQC, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
test$PoolQC <- dplyr::recode(test$PoolQC, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)


train$FireplaceQu <- dplyr::recode(train$FireplaceQu, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
test$FireplaceQu <- dplyr::recode(test$FireplaceQu, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)


train$BsmtFinType1 <- dplyr::recode(train$BsmtFinType1, 'None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
test$BsmtFinType1 <- dplyr::recode(test$BsmtFinType1, 'None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)


train$BsmtFinType2 <- dplyr::recode(train$BsmtFinType2, 'None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
test$BsmtFinType2 <- dplyr::recode(test$BsmtFinType2, 'None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
```

### Set normal factor variables

``` r
train$MSZoning <- ordered(train$MSZoning, levels = c("C (all)", "RM", "RH", "RL", "FV"))
test$MSZoning <- ordered(test$MSZoning, levels = c("C (all)", "RM", "RH", "RL", "FV"))

train$Street <- ordered(train$Street, levels = c("Grvl", "Pave"))
test$Street <- ordered(test$Street, levels = c("Grvl", "Pave"))

train$LotShape <- ordered(train$LotShape, levels = c("Reg", "IR1", "IR2", "IR3"))
test$LotShape <- ordered(test$LotShape, levels = c("Reg", "IR1", "IR2", "IR3"))

train$LandContour <- ordered(train$LandContour, levels = c("Bnk", "Lvl", "Low", "HLS"))
test$LandContour <- ordered(test$LandContour, levels = c("Bnk", "Lvl", "Low", "HLS"))

train$LotConfig <- ordered(train$LotConfig, levels = c("Inside", "Corner", "CulDSac", "FR2", "FR3"))
test$LotConfig <- ordered(test$LotConfig, levels = c("Inside", "Corner", "CulDSac", "FR2", "FR3"))

train$LandSlope <- ordered(train$LandSlope, levels = c("Gtl", "Mod", "Sev"))
test$LandSlope <- ordered(test$LandSlope, levels = c("Gtl", "Mod", "Sev"))

train$Condition1 <- ordered(train$Condition1, levels = c("Artery", "Feedr", "RRAe", "Norm", "RRAn", "RRNe", "RRNn", "PosA", "PosN"))
test$Condition1 <- ordered(test$Condition1, levels = c("Artery", "Feedr", "RRAe", "Norm", "RRAn", "RRNe", "RRNn", "PosA", "PosN"))

train$Condition2 <- ordered(train$Condition2, levels = c("Artery", "RRNn","RRAn","Feedr", "Norm", "RRAe", "PosN", "PosA"))
test$Condition2 <- ordered(test$Condition2, levels = c("Artery", "RRNn","RRAn","Feedr", "Norm", "RRAe", "PosN", "PosA"))

train$Alley <- ordered(train$Alley, levels = c("None", "Grvl", "Pave"))
test$Alley <- ordered(test$Alley, levels = c("None", "Grvl", "Pave"))

train$MasVnrType <- ordered(train$MasVnrType, levels = c("None", "CBlock", "BrkFace", "BrkCmn", "Stone"))
test$MasVnrType <- ordered(test$MasVnrType, levels = c("None", "CBlock", "BrkFace", "BrkCmn", "Stone"))

# train$BsmtExposure <- ordered(train$BsmtExposure, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
# test$BsmtExposure <- ordered(test$BsmtExposure, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))


train$GarageType <- ordered(train$GarageType, levels = c("None", "CarPort", "2Types", "Basment", "Detchd", "Attchd", "BuiltIn"))
test$GarageType <- ordered(test$GarageType, levels = c("None", "CarPort", "2Types", "Basment", "Detchd", "Attchd", "BuiltIn"))

train$GarageFinish <- ordered(train$GarageFinish, levels = c("None", "Unf", "RFn", "Fin"))
test$GarageFinish <- ordered(test$GarageFinish, levels = c("None", "Unf", "RFn", "Fin"))

train$Fence <- ordered(train$Fence, levels = c("None", "MnWw", "GdWo", "MnPrv", "GdPrv"))
test$Fence <- ordered(test$Fence, levels = c("None", "MnWw", "GdWo", "MnPrv", "GdPrv"))

train$Electrical <- ordered(train$Electrical, levels = c("Mix", "FuseP", "FuseF", "FuseA", "SBrkr"))
test$Electrical <- ordered(test$Electrical, levels = c("Mix", "FuseP", "FuseF", "FuseA", "SBrkr"))

train$MiscFeature <- ordered(train$MiscFeature, levels = c("None", "Othr", "Shed", "Gar2", "TenC"))
test$MiscFeature <- ordered(test$MiscFeature, levels = c("None", "Othr", "Shed", "Gar2", "TenC"))


train$SaleCondition <- factor(train$SaleCondition, levels = c("Abnorml", "AdjLand", "Alloca", "Partial", "Family", "Normal"))
test$SaleCondition <- factor(test$SaleCondition, levels = c("Abnorml", "AdjLand", "Alloca", "Partial", "Family", "Normal"))

train$BldgType[is.na(train$BldgType)] <- '1Fam'
test$BldgType[is.na(test$BldgType)] <- '1Fam'


# str(train)
```

``` r
# describe(train)
```

#### Describe all categorical variables after handling nulls

``` r
# # Scatterplot of Numeric Variable
# train.numeric %>% filter(WoodDeckSF != 0) %>% ggplot(aes(x = WoodDeckSF, y = log(SalePrice))) +
#   geom_point() + geom_smooth(method = 'lm')


# Bar Chart of Categorical Variable
ggplot(test) + geom_bar(aes(x = Neighborhood))
```

![](Exploratory_Data_Analysis_files/figure-gfm/describe_factors-1.png)<!-- -->

``` r
# Scatterplot of Categroical Variable
train %>% ggplot(aes(x = Condition2, y = log(SalePrice))) +
  geom_point() + geom_smooth(method = 'lm')
```

![](Exploratory_Data_Analysis_files/figure-gfm/describe_factors-2.png)<!-- -->

``` r
# Scatterplot of Categroical Variable
train %>% ggplot(aes(x = Condition2, y = log(SalePrice))) +
  geom_boxplot()
```

![](Exploratory_Data_Analysis_files/figure-gfm/describe_factors-3.png)<!-- -->

``` r
# 
# ggplot(train,aes(y=log(SalePrice),x=GrLivArea,color=factor(Neighborhood)))+geom_point()+stat_smooth(method="lm",se=FALSE)


# # Scatterplot of Categroical Variable
# train %>% ggplot(aes(x = factor(Neighborhood), y = log(SalePrice))) +
#   geom_boxplot()


# head(train$SalePrice)
# 
# 
# describe(test$Condition2)
```

#### Correlation Table

``` r
sales.price.cor <- train %>%
  select_if(is.numeric) %>%
  correlate() %>%
  focus(SalePrice)
```

    ## 
    ## Correlation method: 'pearson'
    ## Missing treated using: 'pairwise.complete.obs'

``` r
sales.price.cor %>%
  arrange(desc(SalePrice))
```

    ## # A tibble: 42 x 2
    ##    rowname      SalePrice
    ##    <chr>            <dbl>
    ##  1 OverallQual      0.791
    ##  2 GrLivArea        0.709
    ##  3 GarageCars       0.640
    ##  4 GarageArea       0.623
    ##  5 TotalBsmtSF      0.614
    ##  6 1stFlrSF         0.606
    ##  7 BsmtQual         0.585
    ##  8 FullBath         0.561
    ##  9 TotRmsAbvGrd     0.534
    ## 10 YearBuilt        0.523
    ## # ... with 32 more rows

#### base model

``` r
model.formula <- log(SalePrice) ~ MSSubClass + MSZoning + LotFrontage + 
    LotArea + Street + LotConfig + LandSlope + Neighborhood + 
    Condition1 + Condition2 + OverallQual + OverallCond + YearBuilt + 
    YearRemodAdd + RoofMatl + Exterior1st + MasVnrArea + ExterCond + 
    Foundation + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + 
    BsmtUnfSF + Heating + HeatingQC + CentralAir + `1stFlrSF` + 
    `2ndFlrSF` + LowQualFinSF + BsmtFullBath + FullBath + HalfBath + 
    KitchenAbvGr + KitchenQual + Functional + Fireplaces + GarageYrBlt + 
    GarageCars + GarageArea + GarageQual + WoodDeckSF + OpenPorchSF + 
    EnclosedPorch + `3SsnPorch` + ScreenPorch + PoolArea + SaleType + 
    SaleCondition

# model.formula <- log(SalePrice) ~ MSZoning + LotFrontage + LotArea +
#     Street + LotConfig + LandSlope + 
#     Condition2 + BldgType + OverallQual + OverallCond + YearBuilt +
#     YearRemodAdd + RoofMatl + Foundation +
#     BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF +
#     HeatingQC + CentralAir + `1stFlrSF` + `2ndFlrSF` +
#     LowQualFinSF + BsmtFullBath + FullBath + HalfBath + KitchenAbvGr +
#     KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + GarageYrBlt +
#     GarageCars + GarageArea + WoodDeckSF +
#     EnclosedPorch + `3SsnPorch` + ScreenPorch + PoolArea + PoolQC +
#     SaleType + OverallQual:Neighborhood

## forward model ##
# model.formula <- log(SalePrice) ~ OverallQual + Neighborhood + GrLivArea + 
#     BsmtFinType1 + GarageCars + OverallCond + RoofMatl + TotalBsmtSF + 
#     YearBuilt + Condition2 + MSZoning + BsmtUnfSF + SaleCondition + 
#     Functional + BldgType + CentralAir + LotArea + KitchenQual + 
#     ScreenPorch + Condition1 + Fireplaces + Heating + BsmtExposure + 
#     Exterior1st + YearRemodAdd + LandSlope + GarageArea + WoodDeckSF + 
#     LotConfig + Foundation + LotFrontage + HeatingQC + PoolQC + 
#     BsmtFullBath + EnclosedPorch + PoolArea + SaleType + HalfBath + 
#     GarageCond + BsmtQual + FullBath + Street + KitchenAbvGr + 
#     `3SsnPorch` + GarageQual + ExterCond + GarageYrBlt + TotRmsAbvGrd

## backward model ##

# model.formula <- log(SalePrice) ~ MSSubClass + MSZoning + LotFrontage + 
#     LotArea + Street + LotConfig + LandSlope + Neighborhood + 
#     Condition1 + Condition2 + OverallQual + OverallCond + YearBuilt + 
#     YearRemodAdd + RoofMatl + Exterior1st + MasVnrArea + ExterCond + 
#     Foundation + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + 
#     BsmtUnfSF + Heating + HeatingQC + CentralAir + `1stFlrSF` + 
#     `2ndFlrSF` + LowQualFinSF + BsmtFullBath + FullBath + HalfBath + 
#     KitchenAbvGr + KitchenQual + Functional + Fireplaces + GarageYrBlt + 
#     GarageCars + GarageArea + GarageQual + WoodDeckSF + OpenPorchSF + 
#     EnclosedPorch + `3SsnPorch` + ScreenPorch + PoolArea + SaleType + 
#     SaleCondition



## stepwise model ##

# model.formula <- log(SalePrice) ~ OverallQual + Neighborhood + GrLivArea + 
#     GarageCars + OverallCond + RoofMatl + TotalBsmtSF + YearBuilt + 
#     Condition2 + MSZoning + BsmtUnfSF + SaleCondition + Functional + 
#     BldgType + CentralAir + LotArea + KitchenQual + ScreenPorch + 
#     Condition1 + Fireplaces + Heating + BsmtExposure + Exterior1st + 
#     YearRemodAdd + LandSlope + GarageArea + WoodDeckSF + LotConfig + 
#     Foundation + LotFrontage + HeatingQC + PoolQC + BsmtFullBath + 
#     EnclosedPorch + PoolArea + SaleType + BsmtFinSF1 + GarageCond + 
#     HalfBath + Street + KitchenAbvGr + FullBath + `3SsnPorch` + 
#     ExterCond + GarageQual


# Remove utilities from dataframe as it doesn't have enough observations in the 2 levels
# dat <- subset(train, select = -c(Utilities, Id) )


base.model <- lm(model.formula,
               data = train)

fit1 <- lm(log(SalePrice) ~ ., data=train)

fit2 <- lm(log(SalePrice) ~ 1, data=train)

# summary(fit1)
```

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
    ## fit may be misleading
    
    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
    ## fit may be misleading
    
    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
    ## fit may be misleading
    
    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
    ## fit may be misleading
    
    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
    ## fit may be misleading
    
    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
    ## fit may be misleading
    
    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
    ## fit may be misleading

    ## Linear Regression 
    ## 
    ## 1460 samples
    ##   49 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 1315, 1312, 1314, 1314, 1314, 1315, ... 
    ## Resampling results:
    ## 
    ##   RMSE       Rsquared   MAE       
    ##   0.1865735  0.7926647  0.08984226
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE
