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
          - [Model Definitions](#model-definitions)
          - [Base Model](#base-model)
          - [Forward Model](#forward-model)
          - [Backward Model](#backward-model)
          - [Stepwise Model](#stepwise-model)
      - [Cross Validation](#cross-validation)
          - [Custom Model CV](#custom-model-cv)
          - [Forward Model CV](#forward-model-cv)
          - [Backward Model CV](#backward-model-cv)
          - [Stepwise Model CV](#stepwise-model-cv)

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

``` r
' Calculates PRESS from `caret` CV model
#'
```

    ## [1] " Calculates PRESS from `caret` CV model\n#"

``` r
#' @param model.cv Calculates press from a model 
#' produced by `caret`
#'
PRESS.cv <- function(model.cv) {
  meanN <- 0
  folds <- model.cv$control$index
  for (i in seq(1:length(folds))){
    meanN <- meanN + length(folds[[i]])
  }
  meanN <- meanN / length(folds)
  meanN * ((model.cv$results$RMSE)^2)
}
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
  - GarageCond (Removed)
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


# Remove Basement Condition as it is highly correlated to Basement Quality
# train = subset(train, select = -c(Utilities, BsmtCond, GarageCond) )
# test = subset(test, select = -c(Utilities, BsmtCond, GarageCond) )

# Remove utilities from dataframe as it doesn't have enough observations in the 2 levels
train = subset(train, select = -c(Utilities ) )
test = subset(test, select = -c(Utilities ) )

########################################################################################################################################################################

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


########################################################################################################################################################################
train$GarageQual <- dplyr::recode(train$GarageQual, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
test$GarageQual <- dplyr::recode(test$GarageQual, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

train$GarageFinish <- dplyr::recode(train$GarageFinish, 'None' = 0, 'Unf' = 1, 'RFn' = 2, 'Fin' = 3)
test$GarageFinish <- dplyr::recode(test$GarageFinish, 'None' = 0, 'Unf' = 1, 'RFn' = 2, 'Fin' = 3)

train$GarageType <- dplyr::recode(train$GarageType, 'None' = 0, 'CarPort' = 1, '2Types' = 2, 'Basment' = 3, 'Detchd' = 4, 'Attchd' = 5, 'BuiltIn' = 6)
test$GarageType <- dplyr::recode(test$GarageType, 'None' = 0, 'CarPort' = 1, '2Types' = 2, 'Basment' = 3, 'Detchd' = 4, 'Attchd' = 5, 'BuiltIn' = 6)


train$GarageCond <- dplyr::recode(train$GarageCond, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
test$GarageCond <- dplyr::recode(test$GarageCond, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

########################################################################################################################################################################


train$BsmtQual <- dplyr::recode(train$BsmtQual, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
test$BsmtQual <- dplyr::recode(test$BsmtQual, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

train$BsmtCond <- dplyr::recode(train$BsmtCond, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
test$BsmtCond <- dplyr::recode(test$BsmtCond, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)


train$BsmtFinType1 <- dplyr::recode(train$BsmtFinType1, 'None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
test$BsmtFinType1 <- dplyr::recode(test$BsmtFinType1, 'None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)


train$BsmtFinType2 <- dplyr::recode(train$BsmtFinType2, 'None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
test$BsmtFinType2 <- dplyr::recode(test$BsmtFinType2, 'None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)


train$BsmtExposure <- dplyr::recode(train$BsmtExposure, 'None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)
test$BsmtExposure <- dplyr::recode(test$BsmtExposure, 'None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)
########################################################################################################################################################################

train$LandSlope <- dplyr::recode(train$LandSlope, 'Sev' = 0, 'Mod' = 1, 'Gtl' = 2)
test$LandSlope <- dplyr::recode(test$LandSlope, 'Sev' = 0, 'Mod' = 1, 'Gtl' = 2)


train$MSZoning <- dplyr::recode(train$MSZoning, 'C (all)' = 0, 'RM' = 1, 'RH' = 2, 'RL' = 3, 'FV' = 4)
test$MSZoning <- dplyr::recode(test$MSZoning, 'C (all)' = 0, 'RM' = 1, 'RH' = 2, 'RL' = 3, 'FV' = 4)


train$PoolQC <- dplyr::recode(train$PoolQC, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
test$PoolQC <- dplyr::recode(test$PoolQC, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)


train$FireplaceQu <- dplyr::recode(train$FireplaceQu, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
test$FireplaceQu <- dplyr::recode(test$FireplaceQu, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

########################################################################################################################################################################
```

### Set normal factor variables

``` r
train$Street <- ordered(train$Street, levels = c("Grvl", "Pave"))
test$Street <- ordered(test$Street, levels = c("Grvl", "Pave"))

train$LotShape <- ordered(train$LotShape, levels = c("Reg", "IR1", "IR2", "IR3"))
test$LotShape <- ordered(test$LotShape, levels = c("Reg", "IR1", "IR2", "IR3"))

train$LandContour <- ordered(train$LandContour, levels = c("Bnk", "Lvl", "Low", "HLS"))
test$LandContour <- ordered(test$LandContour, levels = c("Bnk", "Lvl", "Low", "HLS"))

train$LotConfig <- ordered(train$LotConfig, levels = c("Inside", "Corner", "CulDSac", "FR2", "FR3"))
test$LotConfig <- ordered(test$LotConfig, levels = c("Inside", "Corner", "CulDSac", "FR2", "FR3"))

train$Condition1 <- ordered(train$Condition1, levels = c("Artery", "Feedr", "RRAe", "Norm", "RRAn", "RRNe", "RRNn", "PosA", "PosN"))
test$Condition1 <- ordered(test$Condition1, levels = c("Artery", "Feedr", "RRAe", "Norm", "RRAn", "RRNe", "RRNn", "PosA", "PosN"))

train$Condition2 <- ordered(train$Condition2, levels = c("Artery", "RRNn","RRAn","Feedr", "Norm", "RRAe", "PosN", "PosA"))
test$Condition2 <- ordered(test$Condition2, levels = c("Artery", "RRNn","RRAn","Feedr", "Norm", "RRAe", "PosN", "PosA"))

train$Alley <- ordered(train$Alley, levels = c("None", "Grvl", "Pave"))
test$Alley <- ordered(test$Alley, levels = c("None", "Grvl", "Pave"))

train$MasVnrType <- ordered(train$MasVnrType, levels = c("None", "CBlock", "BrkFace", "BrkCmn", "Stone"))
test$MasVnrType <- ordered(test$MasVnrType, levels = c("None", "CBlock", "BrkFace", "BrkCmn", "Stone"))


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
# EDA for categorical varialbes
train %>% ggplot(aes(x = Condition1, y = log(SalePrice))) +
  geom_point() + geom_smooth(method = 'lm')
```

![](Exploratory_Data_Analysis_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# remove suspect points from training data
train <- train %>% filter(GrLivArea < 4000)
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
# ggplot(test) + geom_bar(aes(x = Neighborhood))

# Scatterplot of Categroical Variable
# train %>% ggplot(aes(x = Condition2, y = log(SalePrice))) +
#   geom_point() + geom_smooth(method = 'lm')

# Scatterplot of Categroical Variable
# train %>% ggplot(aes(x = Condition2, y = log(SalePrice))) +
#   geom_boxplot()
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

    ## # A tibble: 49 x 2
    ##    rowname      SalePrice
    ##    <chr>            <dbl>
    ##  1 OverallQual      0.801
    ##  2 GrLivArea        0.721
    ##  3 GarageCars       0.649
    ##  4 TotalBsmtSF      0.647
    ##  5 GarageArea       0.637
    ##  6 1stFlrSF         0.625
    ##  7 BsmtQual         0.592
    ##  8 FullBath         0.559
    ##  9 GarageFinish     0.557
    ## 10 TotRmsAbvGrd     0.537
    ## # ... with 39 more rows

### Model Definitions

``` r
custom.model.formula <- log(SalePrice) ~  OverallQual + GrLivArea + Neighborhood + 
    BsmtFinSF1 + MSSubClass + OverallCond + YearBuilt + GarageCars + 
    TotalBsmtSF + SaleCondition + LotArea + MSZoning + Functional + 
    CentralAir + KitchenQual + Condition1 + FireplaceQu + BsmtExposure + 
    BsmtFullBath + ScreenPorch + Exterior1st + YearRemodAdd + 
    GarageQual + WoodDeckSF + OpenPorchSF + Street + LotConfig + 
    LotFrontage + Foundation + Heating + KitchenAbvGr + EnclosedPorch + 
    HalfBath + FullBath + MasVnrType + BsmtFinSF2 + HeatingQC + 
    GarageArea + SaleType + ExterCond + PoolArea + BsmtFinType1 + 
    GarageYrBlt + Electrical + `3SsnPorch` + LowQualFinSF + MSZoning:Neighborhood + OverallQual:Neighborhood + YearBuilt:Neighborhood 

### forward model ###
fwd.model.formula <- log(SalePrice) ~ OverallQual + GrLivArea + Neighborhood + 
    BsmtFinSF1 + MSSubClass + OverallCond + YearBuilt + GarageCars + 
    TotalBsmtSF + SaleCondition + LotArea + MSZoning + Functional + 
    CentralAir + KitchenQual + Condition1 + FireplaceQu + BsmtExposure + 
    BsmtFullBath + ScreenPorch + Exterior1st + YearRemodAdd + 
    GarageQual + WoodDeckSF + OpenPorchSF + Street + LotConfig + 
    LotFrontage + Foundation + Heating + KitchenAbvGr + EnclosedPorch + 
    HalfBath + FullBath + MasVnrType + BsmtFinSF2 + HeatingQC + 
    GarageArea + SaleType + ExterCond + PoolArea + BsmtFinType1 + 
    GarageYrBlt + Electrical + `3SsnPorch` + LowQualFinSF

### backward model ###

bkw.model.formula <- log(SalePrice) ~ MSSubClass + MSZoning + LotFrontage +
    LotArea + Street + LotConfig + LandSlope + Neighborhood +
    Condition1 + OverallQual + OverallCond + YearBuilt + YearRemodAdd +
    RoofMatl + Exterior1st + MasVnrType + ExterCond + Foundation +
    BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF +
    Heating + HeatingQC + CentralAir + Electrical + `1stFlrSF` +
    `2ndFlrSF` + LowQualFinSF + BsmtFullBath + FullBath + HalfBath +
    KitchenAbvGr + KitchenQual + Functional + FireplaceQu + GarageYrBlt +
    GarageCars + GarageArea + GarageQual + WoodDeckSF + OpenPorchSF +
    EnclosedPorch + ScreenPorch + PoolArea + SaleType + SaleCondition


### stepwise model ###

stw.model.formula <- log(SalePrice) ~ OverallQual + GrLivArea + Neighborhood + 
    BsmtFinSF1 + MSSubClass + OverallCond + YearBuilt + GarageCars + 
    TotalBsmtSF + SaleCondition + LotArea + MSZoning + Functional + 
    CentralAir + KitchenQual + Condition1 + FireplaceQu + BsmtExposure + 
    BsmtFullBath + ScreenPorch + Exterior1st + YearRemodAdd + 
    GarageQual + WoodDeckSF + OpenPorchSF + Street + LotConfig + 
    LotFrontage + Foundation + Heating + KitchenAbvGr + EnclosedPorch + 
    HalfBath + FullBath + MasVnrType + BsmtFinSF2 + HeatingQC + 
    GarageArea + SaleType + ExterCond + PoolArea + BsmtFinType1 + 
    GarageYrBlt + Electrical + `3SsnPorch` + LowQualFinSF


custom.model <- lm(custom.model.formula,
               data = train)


fwd.model <- lm(fwd.model.formula,
               data = train)

bkw.model <- lm(bkw.model.formula,
               data = train)

stw.model <- lm(stw.model.formula,
               data = train)


# Fit the model with all parameters
fit1 <- lm(log(SalePrice) ~ ., data=train)

# Fit the model with only 1 parameter
fit2 <- lm(log(SalePrice) ~ 1, data=train)
```

### Base Model

    ## Warning in predict.lm(custom.model, test): prediction from a rank-deficient
    ## fit may be misleading

    ## 
    ## Call:
    ## lm(formula = custom.model.formula, data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.65975 -0.04416  0.00283  0.04887  0.40619 
    ## 
    ## Coefficients: (14 not defined because of singularities)
    ##                                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      1.308e+02  4.501e+01   2.906 0.003728 ** 
    ## OverallQual                      3.086e-02  6.887e-02   0.448 0.654180    
    ## GrLivArea                        2.573e-04  1.463e-05  17.591  < 2e-16 ***
    ## NeighborhoodBlueste             -1.556e+00  5.829e-01  -2.669 0.007700 ** 
    ## NeighborhoodBrDale              -9.979e+01  7.041e+01  -1.417 0.156635    
    ## NeighborhoodBrkSide             -1.233e+02  4.509e+01  -2.734 0.006340 ** 
    ## NeighborhoodClearCr             -1.226e+02  4.513e+01  -2.718 0.006666 ** 
    ## NeighborhoodCollgCr             -1.278e+02  4.506e+01  -2.835 0.004652 ** 
    ## NeighborhoodCrawfor             -1.225e+02  4.505e+01  -2.720 0.006626 ** 
    ## NeighborhoodEdwards             -1.269e+02  4.502e+01  -2.820 0.004879 ** 
    ## NeighborhoodGilbert             -1.222e+02  4.513e+01  -2.707 0.006873 ** 
    ## NeighborhoodIDOTRR              -1.202e+02  4.514e+01  -2.663 0.007844 ** 
    ## NeighborhoodMeadowV             -1.377e+02  5.146e+01  -2.677 0.007530 ** 
    ## NeighborhoodMitchel             -1.218e+02  4.506e+01  -2.704 0.006951 ** 
    ## NeighborhoodNAmes               -1.250e+02  4.504e+01  -2.774 0.005613 ** 
    ## NeighborhoodNoRidge             -1.413e+02  4.685e+01  -3.017 0.002606 ** 
    ## NeighborhoodNPkVill             -1.025e+02  7.185e+01  -1.426 0.154067    
    ## NeighborhoodNridgHt             -1.532e+02  4.689e+01  -3.268 0.001113 ** 
    ## NeighborhoodNWAmes              -1.313e+02  4.520e+01  -2.905 0.003731 ** 
    ## NeighborhoodOldTown             -1.251e+02  4.502e+01  -2.778 0.005544 ** 
    ## NeighborhoodSawyer              -1.251e+02  4.504e+01  -2.778 0.005551 ** 
    ## NeighborhoodSawyerW             -1.283e+02  4.511e+01  -2.844 0.004527 ** 
    ## NeighborhoodSomerst             -1.187e+02  4.565e+01  -2.600 0.009420 ** 
    ## NeighborhoodStoneBr             -1.358e+02  4.523e+01  -3.002 0.002737 ** 
    ## NeighborhoodSWISU               -1.296e+02  4.524e+01  -2.863 0.004261 ** 
    ## NeighborhoodTimber              -1.303e+02  4.513e+01  -2.888 0.003939 ** 
    ## NeighborhoodVeenker             -1.151e+02  4.631e+01  -2.487 0.013025 *  
    ## BsmtFinSF1                       6.070e-05  1.197e-05   5.069 4.59e-07 ***
    ## MSSubClass160F                  -7.791e-02  2.710e-02  -2.875 0.004110 ** 
    ## MSSubClass180F                  -1.022e-03  5.305e-02  -0.019 0.984631    
    ## MSSubClass190F                   2.273e-04  2.982e-02   0.008 0.993919    
    ## MSSubClass20F                    3.649e-02  2.060e-02   1.771 0.076810 .  
    ## MSSubClass30F                   -1.911e-02  2.770e-02  -0.690 0.490338    
    ## MSSubClass40F                    2.220e-02  5.573e-02   0.398 0.690483    
    ## MSSubClass45F                    4.913e-02  3.997e-02   1.229 0.219239    
    ## MSSubClass50F                    3.160e-02  2.542e-02   1.243 0.213966    
    ## MSSubClass60F                    1.272e-02  2.319e-02   0.548 0.583471    
    ## MSSubClass70F                    2.258e-02  2.970e-02   0.760 0.447257    
    ## MSSubClass75F                    1.129e-02  3.982e-02   0.284 0.776827    
    ## MSSubClass80F                    3.791e-02  2.544e-02   1.490 0.136412    
    ## MSSubClass85F                    1.986e-02  3.130e-02   0.635 0.525833    
    ## OverallCond                      3.743e-02  3.702e-03  10.110  < 2e-16 ***
    ## YearBuilt                       -6.041e-02  2.237e-02  -2.700 0.007024 ** 
    ## GarageCars                       2.609e-02  9.613e-03   2.714 0.006736 ** 
    ## TotalBsmtSF                      8.731e-05  1.438e-05   6.069 1.70e-09 ***
    ## SaleConditionAdjLand             1.202e-01  5.749e-02   2.091 0.036698 *  
    ## SaleConditionAlloca              7.405e-02  3.760e-02   1.969 0.049130 *  
    ## SaleConditionPartial            -4.501e-02  6.163e-02  -0.730 0.465340    
    ## SaleConditionFamily              3.062e-02  2.611e-02   1.173 0.241139    
    ## SaleConditionNormal              8.035e-02  1.233e-02   6.517 1.04e-10 ***
    ## LotArea                          2.004e-06  3.615e-07   5.545 3.58e-08 ***
    ## MSZoning                         3.735e-02  5.392e-02   0.693 0.488638    
    ## FunctionalMaj2                  -1.643e-01  6.122e-02  -2.684 0.007362 ** 
    ## FunctionalMin1                   4.932e-02  3.767e-02   1.309 0.190705    
    ## FunctionalMin2                   3.221e-02  3.745e-02   0.860 0.390020    
    ## FunctionalMod                   -5.793e-02  4.348e-02  -1.332 0.182975    
    ## FunctionalSev                   -2.074e-01  1.100e-01  -1.886 0.059551 .  
    ## FunctionalTyp                    7.202e-02  3.316e-02   2.172 0.030058 *  
    ## CentralAirY                      5.963e-02  1.612e-02   3.700 0.000225 ***
    ## KitchenQualFa                   -6.478e-02  2.579e-02  -2.512 0.012139 *  
    ## KitchenQualGd                   -6.674e-02  1.448e-02  -4.610 4.44e-06 ***
    ## KitchenQualTA                   -6.808e-02  1.636e-02  -4.163 3.36e-05 ***
    ## Condition1.L                     6.062e-02  2.933e-02   2.067 0.038957 *  
    ## Condition1.Q                     8.057e-03  3.208e-02   0.251 0.801772    
    ## Condition1.C                    -6.693e-03  3.659e-02  -0.183 0.854889    
    ## Condition1^4                    -7.567e-03  3.046e-02  -0.248 0.803873    
    ## Condition1^5                    -4.788e-03  3.843e-02  -0.125 0.900853    
    ## Condition1^6                    -1.906e-02  3.610e-02  -0.528 0.597644    
    ## Condition1^7                     1.029e-01  4.747e-02   2.167 0.030395 *  
    ## Condition1^8                    -4.627e-03  4.122e-02  -0.112 0.910649    
    ## FireplaceQu                      8.030e-03  2.045e-03   3.927 9.08e-05 ***
    ## BsmtExposure                     1.454e-02  3.522e-03   4.129 3.88e-05 ***
    ## BsmtFullBath                     2.371e-02  7.856e-03   3.019 0.002590 ** 
    ## ScreenPorch                      2.539e-04  5.134e-05   4.947 8.57e-07 ***
    ## Exterior1stAsphShn               3.612e-02  1.053e-01   0.343 0.731602    
    ## Exterior1stBrkComm              -2.228e-01  8.148e-02  -2.735 0.006327 ** 
    ## Exterior1stBrkFace               4.984e-02  3.087e-02   1.614 0.106706    
    ## Exterior1stCBlock               -1.227e-01  1.076e-01  -1.141 0.254086    
    ## Exterior1stCemntBd              -3.405e-03  3.193e-02  -0.107 0.915095    
    ## Exterior1stHdBoard              -2.127e-02  2.816e-02  -0.755 0.450273    
    ## Exterior1stImStucc               2.856e-03  1.052e-01   0.027 0.978343    
    ## Exterior1stMetalSd               7.043e-03  2.701e-02   0.261 0.794352    
    ## Exterior1stPlywood              -1.414e-02  2.948e-02  -0.480 0.631606    
    ## Exterior1stStone                -3.120e-04  8.115e-02  -0.004 0.996933    
    ## Exterior1stStucco                6.663e-03  3.461e-02   0.193 0.847377    
    ## Exterior1stVinylSd              -5.095e-03  2.740e-02  -0.186 0.852512    
    ## Exterior1stWd Sdng              -2.004e-02  2.700e-02  -0.742 0.458108    
    ## Exterior1stWdShing              -4.810e-03  3.368e-02  -0.143 0.886468    
    ## YearRemodAdd                     5.102e-04  2.321e-04   2.198 0.028125 *  
    ## GarageQual                       4.194e-02  1.285e-02   3.264 0.001130 ** 
    ## WoodDeckSF                       8.397e-05  2.506e-05   3.351 0.000830 ***
    ## OpenPorchSF                      1.418e-04  4.860e-05   2.918 0.003584 ** 
    ## Street.L                         2.604e-02  3.684e-02   0.707 0.479813    
    ## LotConfig.L                     -7.155e-02  3.907e-02  -1.831 0.067317 .  
    ## LotConfig.Q                     -6.993e-02  3.334e-02  -2.098 0.036147 *  
    ## LotConfig.C                     -2.561e-03  2.195e-02  -0.117 0.907139    
    ## LotConfig^4                      3.402e-02  1.371e-02   2.480 0.013254 *  
    ## LotFrontage                      6.099e-04  1.806e-04   3.377 0.000754 ***
    ## FoundationCBlock                 1.187e-02  1.385e-02   0.857 0.391707    
    ## FoundationPConc                  2.962e-02  1.489e-02   1.989 0.046954 *  
    ## FoundationSlab                   9.009e-03  3.226e-02   0.279 0.780075    
    ## FoundationStone                  1.125e-01  4.541e-02   2.478 0.013353 *  
    ## FoundationWood                  -1.193e-01  6.305e-02  -1.893 0.058587 .  
    ## HeatingGasA                      7.175e-02  1.083e-01   0.663 0.507769    
    ## HeatingGasW                      8.954e-02  1.106e-01   0.810 0.418235    
    ## HeatingGrav                     -4.165e-02  1.169e-01  -0.356 0.721664    
    ## HeatingOthW                      1.573e-02  1.330e-01   0.118 0.905818    
    ## HeatingWall                      9.229e-02  1.252e-01   0.737 0.461012    
    ## KitchenAbvGr                    -6.246e-02  2.264e-02  -2.758 0.005899 ** 
    ## EnclosedPorch                    1.284e-04  5.212e-05   2.463 0.013908 *  
    ## HalfBath                         3.122e-02  8.721e-03   3.580 0.000357 ***
    ## FullBath                         2.404e-02  9.313e-03   2.581 0.009970 ** 
    ## MasVnrType.L                     9.473e-03  1.061e-02   0.893 0.372079    
    ## MasVnrType.Q                     3.106e-02  1.544e-02   2.012 0.044412 *  
    ## MasVnrType.C                     3.881e-02  1.953e-02   1.987 0.047127 *  
    ## BsmtFinSF2                       3.387e-05  1.944e-05   1.742 0.081775 .  
    ## HeatingQCFa                     -1.816e-02  1.912e-02  -0.950 0.342519    
    ## HeatingQCGd                     -1.730e-02  8.849e-03  -1.955 0.050797 .  
    ## HeatingQCPo                     -9.591e-02  1.164e-01  -0.824 0.410038    
    ## HeatingQCTA                     -2.728e-02  8.622e-03  -3.164 0.001593 ** 
    ## GarageArea                       6.555e-05  3.117e-05   2.103 0.035694 *  
    ## SaleTypeCon                      6.956e-02  7.848e-02   0.886 0.375585    
    ## SaleTypeConLD                    1.411e-01  4.092e-02   3.449 0.000581 ***
    ## SaleTypeConLI                   -3.353e-02  5.107e-02  -0.657 0.511611    
    ## SaleTypeConLw                    2.332e-02  5.088e-02   0.458 0.646813    
    ## SaleTypeCWD                      5.879e-02  5.567e-02   1.056 0.291123    
    ## SaleTypeNew                      1.497e-01  6.401e-02   2.338 0.019521 *  
    ## SaleTypeOth                      6.513e-02  6.195e-02   1.051 0.293286    
    ## SaleTypeWD                      -1.641e-02  1.772e-02  -0.927 0.354313    
    ## ExterCondFa                     -8.725e-02  6.649e-02  -1.312 0.189697    
    ## ExterCondGd                     -7.947e-02  6.178e-02  -1.286 0.198573    
    ## ExterCondPo                     -6.928e-03  1.283e-01  -0.054 0.956938    
    ## ExterCondTA                     -6.202e-02  6.186e-02  -1.003 0.316213    
    ## PoolArea                         1.424e-04  7.989e-05   1.783 0.074889 .  
    ## BsmtFinType1                     4.134e-03  2.121e-03   1.949 0.051537 .  
    ## GarageYrBlt                     -4.688e-05  2.070e-05  -2.265 0.023664 *  
    ## Electrical.L                    -7.345e-02  7.963e-02  -0.922 0.356469    
    ## Electrical.Q                     6.096e-02  6.786e-02   0.898 0.369163    
    ## Electrical.C                    -5.733e-02  5.705e-02  -1.005 0.315166    
    ## Electrical^4                     2.606e-02  3.782e-02   0.689 0.490886    
    ## `3SsnPorch`                      1.647e-04  9.428e-05   1.746 0.080979 .  
    ## LowQualFinSF                    -7.766e-05  6.513e-05  -1.192 0.233332    
    ## NeighborhoodBlueste:MSZoning            NA         NA      NA       NA    
    ## NeighborhoodBrDale:MSZoning             NA         NA      NA       NA    
    ## NeighborhoodBrkSide:MSZoning     6.691e-03  5.580e-02   0.120 0.904584    
    ## NeighborhoodClearCr:MSZoning            NA         NA      NA       NA    
    ## NeighborhoodCollgCr:MSZoning    -4.113e-03  5.810e-02  -0.071 0.943572    
    ## NeighborhoodCrawfor:MSZoning    -4.790e-02  6.827e-02  -0.702 0.483066    
    ## NeighborhoodEdwards:MSZoning    -5.009e-03  5.985e-02  -0.084 0.933320    
    ## NeighborhoodGilbert:MSZoning            NA         NA      NA       NA    
    ## NeighborhoodIDOTRR:MSZoning      3.784e-01  7.409e-02   5.108 3.76e-07 ***
    ## NeighborhoodMeadowV:MSZoning            NA         NA      NA       NA    
    ## NeighborhoodMitchel:MSZoning    -6.588e-02  6.113e-02  -1.078 0.281406    
    ## NeighborhoodNAmes:MSZoning      -1.321e-01  9.944e-02  -1.328 0.184312    
    ## NeighborhoodNoRidge:MSZoning            NA         NA      NA       NA    
    ## NeighborhoodNPkVill:MSZoning            NA         NA      NA       NA    
    ## NeighborhoodNridgHt:MSZoning    -7.183e-02  7.989e-02  -0.899 0.368785    
    ## NeighborhoodNWAmes:MSZoning             NA         NA      NA       NA    
    ## NeighborhoodOldTown:MSZoning    -4.141e-03  5.580e-02  -0.074 0.940857    
    ## NeighborhoodSawyer:MSZoning     -2.312e-02  6.925e-02  -0.334 0.738563    
    ## NeighborhoodSawyerW:MSZoning    -5.956e-03  9.246e-02  -0.064 0.948651    
    ## NeighborhoodSomerst:MSZoning    -5.794e-03  6.122e-02  -0.095 0.924618    
    ## NeighborhoodStoneBr:MSZoning            NA         NA      NA       NA    
    ## NeighborhoodSWISU:MSZoning      -2.232e-01  7.697e-02  -2.899 0.003803 ** 
    ## NeighborhoodTimber:MSZoning             NA         NA      NA       NA    
    ## NeighborhoodVeenker:MSZoning            NA         NA      NA       NA    
    ## OverallQual:NeighborhoodBlueste         NA         NA      NA       NA    
    ## OverallQual:NeighborhoodBrDale  -5.576e-02  9.130e-02  -0.611 0.541454    
    ## OverallQual:NeighborhoodBrkSide  5.447e-02  7.040e-02   0.774 0.439238    
    ## OverallQual:NeighborhoodClearCr  3.842e-02  7.211e-02   0.533 0.594294    
    ## OverallQual:NeighborhoodCollgCr  1.085e-02  6.992e-02   0.155 0.876692    
    ## OverallQual:NeighborhoodCrawfor  3.627e-02  7.087e-02   0.512 0.608836    
    ## OverallQual:NeighborhoodEdwards -5.309e-03  6.971e-02  -0.076 0.939307    
    ## OverallQual:NeighborhoodGilbert  2.809e-03  7.102e-02   0.040 0.968455    
    ## OverallQual:NeighborhoodIDOTRR   7.797e-03  7.145e-02   0.109 0.913116    
    ## OverallQual:NeighborhoodMeadowV  2.913e-02  8.596e-02   0.339 0.734734    
    ## OverallQual:NeighborhoodMitchel  2.927e-02  7.164e-02   0.409 0.682921    
    ## OverallQual:NeighborhoodNAmes   -2.612e-02  6.956e-02  -0.376 0.707336    
    ## OverallQual:NeighborhoodNoRidge  6.849e-04  7.287e-02   0.009 0.992502    
    ## OverallQual:NeighborhoodNPkVill         NA         NA      NA       NA    
    ## OverallQual:NeighborhoodNridgHt  1.933e-02  6.979e-02   0.277 0.781821    
    ## OverallQual:NeighborhoodNWAmes  -2.768e-02  7.134e-02  -0.388 0.698037    
    ## OverallQual:NeighborhoodOldTown  2.518e-02  6.946e-02   0.362 0.717067    
    ## OverallQual:NeighborhoodSawyer   4.123e-03  7.286e-02   0.057 0.954875    
    ## OverallQual:NeighborhoodSawyerW -3.739e-03  7.132e-02  -0.052 0.958200    
    ## OverallQual:NeighborhoodSomerst  1.256e-02  7.038e-02   0.178 0.858390    
    ## OverallQual:NeighborhoodStoneBr  6.572e-03  7.711e-02   0.085 0.932085    
    ## OverallQual:NeighborhoodSWISU    1.156e-02  7.162e-02   0.161 0.871744    
    ## OverallQual:NeighborhoodTimber   1.656e-02  7.177e-02   0.231 0.817503    
    ## OverallQual:NeighborhoodVeenker  6.842e-02  7.589e-02   0.902 0.367453    
    ## NeighborhoodBlueste:YearBuilt           NA         NA      NA       NA    
    ## NeighborhoodBrDale:YearBuilt     4.970e-02  3.547e-02   1.401 0.161390    
    ## NeighborhoodBrkSide:YearBuilt    6.131e-02  2.241e-02   2.735 0.006318 ** 
    ## NeighborhoodClearCr:YearBuilt    6.102e-02  2.241e-02   2.724 0.006547 ** 
    ## NeighborhoodCollgCr:YearBuilt    6.368e-02  2.240e-02   2.843 0.004544 ** 
    ## NeighborhoodCrawfor:YearBuilt    6.107e-02  2.239e-02   2.727 0.006478 ** 
    ## NeighborhoodEdwards:YearBuilt    6.331e-02  2.238e-02   2.829 0.004740 ** 
    ## NeighborhoodGilbert:YearBuilt    6.092e-02  2.241e-02   2.718 0.006654 ** 
    ## NeighborhoodIDOTRR:YearBuilt     5.963e-02  2.244e-02   2.657 0.007979 ** 
    ## NeighborhoodMeadowV:YearBuilt    6.865e-02  2.572e-02   2.670 0.007693 ** 
    ## NeighborhoodMitchel:YearBuilt    6.071e-02  2.240e-02   2.711 0.006807 ** 
    ## NeighborhoodNAmes:YearBuilt      6.256e-02  2.239e-02   2.794 0.005280 ** 
    ## NeighborhoodNoRidge:YearBuilt    7.054e-02  2.329e-02   3.029 0.002501 ** 
    ## NeighborhoodNPkVill:YearBuilt    5.094e-02  3.614e-02   1.410 0.158935    
    ## NeighborhoodNridgHt:YearBuilt    7.649e-02  2.332e-02   3.279 0.001069 ** 
    ## NeighborhoodNWAmes:YearBuilt     6.560e-02  2.245e-02   2.923 0.003533 ** 
    ## NeighborhoodOldTown:YearBuilt    6.230e-02  2.238e-02   2.784 0.005447 ** 
    ## NeighborhoodSawyer:YearBuilt     6.240e-02  2.239e-02   2.787 0.005398 ** 
    ## NeighborhoodSawyerW:YearBuilt    6.400e-02  2.243e-02   2.854 0.004392 ** 
    ## NeighborhoodSomerst:YearBuilt    5.917e-02  2.269e-02   2.608 0.009222 ** 
    ## NeighborhoodStoneBr:YearBuilt    6.777e-02  2.247e-02   3.016 0.002609 ** 
    ## NeighborhoodSWISU:YearBuilt      6.498e-02  2.250e-02   2.888 0.003948 ** 
    ## NeighborhoodTimber:YearBuilt     6.496e-02  2.241e-02   2.899 0.003813 ** 
    ## NeighborhoodVeenker:YearBuilt    5.715e-02  2.302e-02   2.482 0.013190 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.09808 on 1256 degrees of freedom
    ## Multiple R-squared:  0.9471, Adjusted R-squared:  0.9387 
    ## F-statistic: 112.9 on 199 and 1256 DF,  p-value: < 2.2e-16

### Forward Model

    ## 
    ## Call:
    ## lm(formula = fwd.model.formula, data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.67960 -0.04850  0.00175  0.05279  0.49737 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           4.755e+00  7.465e-01   6.369 2.62e-10 ***
    ## OverallQual           4.706e-02  4.160e-03  11.312  < 2e-16 ***
    ## GrLivArea             2.605e-04  1.475e-05  17.662  < 2e-16 ***
    ## NeighborhoodBlueste   7.614e-02  8.448e-02   0.901 0.367570    
    ## NeighborhoodBrDale    4.686e-02  4.863e-02   0.964 0.335440    
    ## NeighborhoodBrkSide   8.548e-02  3.931e-02   2.174 0.029852 *  
    ## NeighborhoodClearCr   7.241e-02  3.790e-02   1.911 0.056263 .  
    ## NeighborhoodCollgCr   5.023e-03  3.089e-02   0.163 0.870850    
    ## NeighborhoodCrawfor   1.453e-01  3.622e-02   4.012 6.36e-05 ***
    ## NeighborhoodEdwards  -1.993e-02  3.446e-02  -0.578 0.563114    
    ## NeighborhoodGilbert   9.481e-03  3.295e-02   0.288 0.773581    
    ## NeighborhoodIDOTRR   -9.340e-03  4.504e-02  -0.207 0.835753    
    ## NeighborhoodMeadowV  -7.376e-02  5.170e-02  -1.427 0.153933    
    ## NeighborhoodMitchel  -1.535e-02  3.457e-02  -0.444 0.657141    
    ## NeighborhoodNAmes     7.739e-03  3.319e-02   0.233 0.815668    
    ## NeighborhoodNoRidge   5.158e-02  3.522e-02   1.465 0.143252    
    ## NeighborhoodNPkVill   5.453e-02  4.843e-02   1.126 0.260379    
    ## NeighborhoodNridgHt   6.940e-02  3.127e-02   2.219 0.026638 *  
    ## NeighborhoodNWAmes   -2.252e-03  3.414e-02  -0.066 0.947413    
    ## NeighborhoodOldTown   3.557e-02  3.992e-02   0.891 0.373090    
    ## NeighborhoodSawyer    1.666e-02  3.478e-02   0.479 0.632148    
    ## NeighborhoodSawyerW   2.211e-02  3.359e-02   0.658 0.510501    
    ## NeighborhoodSomerst   3.823e-02  3.235e-02   1.182 0.237510    
    ## NeighborhoodStoneBr   1.284e-01  3.519e-02   3.649 0.000274 ***
    ## NeighborhoodSWISU     3.271e-02  4.113e-02   0.795 0.426489    
    ## NeighborhoodTimber    2.444e-02  3.446e-02   0.709 0.478198    
    ## NeighborhoodVeenker   7.530e-02  4.515e-02   1.668 0.095643 .  
    ## BsmtFinSF1            6.378e-05  1.212e-05   5.261 1.67e-07 ***
    ## MSSubClass160F       -9.228e-02  2.476e-02  -3.727 0.000202 ***
    ## MSSubClass180F       -1.420e-02  4.380e-02  -0.324 0.745735    
    ## MSSubClass190F        2.009e-02  2.752e-02   0.730 0.465604    
    ## MSSubClass20F         2.468e-02  1.694e-02   1.457 0.145291    
    ## MSSubClass30F        -2.486e-02  2.530e-02  -0.983 0.325995    
    ## MSSubClass40F         1.201e-02  5.619e-02   0.214 0.830805    
    ## MSSubClass45F         4.473e-02  3.787e-02   1.181 0.237739    
    ## MSSubClass50F         2.339e-02  2.243e-02   1.043 0.297198    
    ## MSSubClass60F        -4.103e-03  2.049e-02  -0.200 0.841329    
    ## MSSubClass70F         3.755e-02  2.741e-02   1.370 0.170881    
    ## MSSubClass75F         2.105e-02  3.856e-02   0.546 0.585210    
    ## MSSubClass80F         1.928e-02  2.238e-02   0.862 0.388974    
    ## MSSubClass85F         9.968e-03  2.941e-02   0.339 0.734702    
    ## OverallCond           3.944e-02  3.688e-03  10.695  < 2e-16 ***
    ## YearBuilt             2.123e-03  3.246e-04   6.541 8.76e-11 ***
    ## GarageCars            2.895e-02  9.759e-03   2.966 0.003067 ** 
    ## TotalBsmtSF           8.561e-05  1.451e-05   5.901 4.60e-09 ***
    ## SaleConditionAdjLand  1.020e-01  5.972e-02   1.709 0.087731 .  
    ## SaleConditionAlloca   6.275e-02  3.621e-02   1.733 0.083347 .  
    ## SaleConditionPartial -3.346e-02  6.349e-02  -0.527 0.598250    
    ## SaleConditionFamily   2.607e-02  2.673e-02   0.975 0.329576    
    ## SaleConditionNormal   8.639e-02  1.244e-02   6.944 5.96e-12 ***
    ## LotArea               1.971e-06  3.471e-07   5.678 1.67e-08 ***
    ## MSZoning              3.778e-02  7.421e-03   5.091 4.07e-07 ***
    ## FunctionalMaj2       -2.094e-01  6.186e-02  -3.385 0.000734 ***
    ## FunctionalMin1        3.961e-02  3.623e-02   1.093 0.274499    
    ## FunctionalMin2        1.313e-02  3.607e-02   0.364 0.715845    
    ## FunctionalMod        -5.078e-02  4.267e-02  -1.190 0.234230    
    ## FunctionalSev        -2.281e-01  1.138e-01  -2.004 0.045306 *  
    ## FunctionalTyp         6.029e-02  3.134e-02   1.924 0.054628 .  
    ## CentralAirY           6.976e-02  1.620e-02   4.306 1.79e-05 ***
    ## KitchenQualFa        -5.857e-02  2.600e-02  -2.253 0.024437 *  
    ## KitchenQualGd        -7.043e-02  1.379e-02  -5.107 3.76e-07 ***
    ## KitchenQualTA        -6.195e-02  1.596e-02  -3.881 0.000109 ***
    ## Condition1.L          5.582e-02  2.960e-02   1.886 0.059519 .  
    ## Condition1.Q         -7.188e-03  3.294e-02  -0.218 0.827288    
    ## Condition1.C         -6.137e-03  3.676e-02  -0.167 0.867435    
    ## Condition1^4          6.145e-03  3.112e-02   0.197 0.843495    
    ## Condition1^5          4.535e-03  3.983e-02   0.114 0.909363    
    ## Condition1^6          4.577e-03  3.527e-02   0.130 0.896757    
    ## Condition1^7          1.032e-01  4.740e-02   2.176 0.029696 *  
    ## Condition1^8          3.760e-04  4.255e-02   0.009 0.992951    
    ## FireplaceQu           6.948e-03  2.078e-03   3.343 0.000851 ***
    ## BsmtExposure          1.360e-02  3.524e-03   3.860 0.000119 ***
    ## BsmtFullBath          2.238e-02  8.027e-03   2.788 0.005384 ** 
    ## ScreenPorch           2.436e-04  5.256e-05   4.634 3.95e-06 ***
    ## Exterior1stAsphShn    4.772e-02  1.097e-01   0.435 0.663560    
    ## Exterior1stBrkComm   -1.887e-01  8.425e-02  -2.240 0.025238 *  
    ## Exterior1stBrkFace    6.397e-02  3.098e-02   2.065 0.039127 *  
    ## Exterior1stCBlock    -1.141e-01  1.101e-01  -1.037 0.300114    
    ## Exterior1stCemntBd    1.955e-02  3.234e-02   0.604 0.545699    
    ## Exterior1stHdBoard   -5.356e-03  2.826e-02  -0.190 0.849714    
    ## Exterior1stImStucc    7.308e-03  1.092e-01   0.067 0.946668    
    ## Exterior1stMetalSd    2.495e-02  2.733e-02   0.913 0.361370    
    ## Exterior1stPlywood   -2.224e-03  2.964e-02  -0.075 0.940193    
    ## Exterior1stStone     -1.854e-03  8.162e-02  -0.023 0.981884    
    ## Exterior1stStucco     2.910e-02  3.520e-02   0.827 0.408642    
    ## Exterior1stVinylSd    1.985e-02  2.760e-02   0.719 0.472291    
    ## Exterior1stWd Sdng   -5.116e-03  2.728e-02  -0.188 0.851256    
    ## Exterior1stWdShing    1.425e-02  3.431e-02   0.415 0.678034    
    ## YearRemodAdd          7.128e-04  2.325e-04   3.066 0.002210 ** 
    ## GarageQual            4.239e-02  1.323e-02   3.205 0.001385 ** 
    ## WoodDeckSF            9.396e-05  2.523e-05   3.724 0.000204 ***
    ## OpenPorchSF           1.257e-04  4.921e-05   2.554 0.010760 *  
    ## Street.L              1.202e-01  3.397e-02   3.537 0.000419 ***
    ## LotConfig.L          -4.929e-02  3.485e-02  -1.414 0.157546    
    ## LotConfig.Q          -5.685e-02  2.989e-02  -1.902 0.057372 .  
    ## LotConfig.C          -3.304e-03  2.011e-02  -0.164 0.869523    
    ## LotConfig^4           2.364e-02  1.329e-02   1.778 0.075568 .  
    ## LotFrontage           5.400e-04  1.827e-04   2.955 0.003179 ** 
    ## FoundationCBlock     -3.271e-03  1.371e-02  -0.239 0.811401    
    ## FoundationPConc       2.607e-02  1.497e-02   1.742 0.081790 .  
    ## FoundationSlab        6.773e-03  3.233e-02   0.209 0.834093    
    ## FoundationStone       6.923e-02  4.634e-02   1.494 0.135403    
    ## FoundationWood       -1.524e-01  6.431e-02  -2.369 0.017965 *  
    ## HeatingGasA           1.396e-01  1.083e-01   1.289 0.197488    
    ## HeatingGasW           1.719e-01  1.111e-01   1.548 0.121921    
    ## HeatingGrav           1.370e-02  1.173e-01   0.117 0.907014    
    ## HeatingOthW           1.031e-01  1.342e-01   0.769 0.442167    
    ## HeatingWall           1.819e-01  1.249e-01   1.457 0.145329    
    ## KitchenAbvGr         -6.415e-02  2.291e-02  -2.800 0.005190 ** 
    ## EnclosedPorch         1.097e-04  5.347e-05   2.051 0.040466 *  
    ## HalfBath              2.844e-02  8.844e-03   3.216 0.001332 ** 
    ## FullBath              2.280e-02  9.290e-03   2.455 0.014233 *  
    ## MasVnrType.L          9.339e-03  1.043e-02   0.896 0.370682    
    ## MasVnrType.Q          3.014e-02  1.580e-02   1.908 0.056670 .  
    ## MasVnrType.C          3.873e-02  2.022e-02   1.915 0.055673 .  
    ## BsmtFinSF2            3.522e-05  1.991e-05   1.769 0.077195 .  
    ## HeatingQCFa          -1.394e-02  1.952e-02  -0.714 0.475096    
    ## HeatingQCGd          -1.785e-02  9.009e-03  -1.981 0.047831 *  
    ## HeatingQCPo          -6.760e-02  1.182e-01  -0.572 0.567483    
    ## HeatingQCTA          -2.966e-02  8.814e-03  -3.365 0.000786 ***
    ## GarageArea            5.952e-05  3.197e-05   1.862 0.062826 .  
    ## SaleTypeCon           7.669e-02  7.818e-02   0.981 0.326785    
    ## SaleTypeConLD         1.061e-01  4.149e-02   2.557 0.010657 *  
    ## SaleTypeConLI        -1.508e-02  5.072e-02  -0.297 0.766312    
    ## SaleTypeConLw         1.924e-02  5.219e-02   0.369 0.712388    
    ## SaleTypeCWD           7.443e-02  5.683e-02   1.310 0.190470    
    ## SaleTypeNew           1.594e-01  6.589e-02   2.419 0.015701 *  
    ## SaleTypeOth           9.053e-02  6.396e-02   1.415 0.157176    
    ## SaleTypeWD           -6.254e-03  1.813e-02  -0.345 0.730232    
    ## ExterCondFa          -1.119e-01  6.792e-02  -1.648 0.099595 .  
    ## ExterCondGd          -8.582e-02  6.321e-02  -1.358 0.174775    
    ## ExterCondPo          -1.002e-01  1.306e-01  -0.767 0.443187    
    ## ExterCondTA          -6.541e-02  6.316e-02  -1.036 0.300557    
    ## PoolArea              1.330e-04  8.220e-05   1.618 0.106007    
    ## BsmtFinType1          3.247e-03  2.167e-03   1.498 0.134341    
    ## GarageYrBlt          -4.607e-05  2.122e-05  -2.171 0.030080 *  
    ## Electrical.L         -1.799e-01  8.191e-02  -2.196 0.028263 *  
    ## Electrical.Q          1.225e-01  6.998e-02   1.751 0.080175 .  
    ## Electrical.C         -8.451e-02  5.910e-02  -1.430 0.152970    
    ## Electrical^4          3.500e-02  3.923e-02   0.892 0.372464    
    ## `3SsnPorch`           1.348e-04  9.743e-05   1.383 0.166755    
    ## LowQualFinSF         -9.088e-05  6.708e-05  -1.355 0.175698    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1027 on 1314 degrees of freedom
    ## Multiple R-squared:  0.9392, Adjusted R-squared:  0.9327 
    ## F-statistic:   144 on 141 and 1314 DF,  p-value: < 2.2e-16

### Backward Model

    ## 
    ## Call:
    ## lm(formula = bkw.model.formula, data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.68387 -0.04748  0.00159  0.05217  0.49173 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           4.609e+00  7.484e-01   6.158 9.78e-10 ***
    ## MSSubClass160F       -9.018e-02  2.623e-02  -3.439 0.000603 ***
    ## MSSubClass180F       -1.785e-02  4.381e-02  -0.407 0.683741    
    ## MSSubClass190F        1.676e-02  2.772e-02   0.605 0.545595    
    ## MSSubClass20F         2.094e-02  1.696e-02   1.235 0.217207    
    ## MSSubClass30F        -2.710e-02  2.528e-02  -1.072 0.283928    
    ## MSSubClass40F         1.861e-02  5.783e-02   0.322 0.747669    
    ## MSSubClass45F         4.617e-02  3.783e-02   1.221 0.222420    
    ## MSSubClass50F         2.562e-02  2.310e-02   1.109 0.267513    
    ## MSSubClass60F        -6.831e-04  2.300e-02  -0.030 0.976310    
    ## MSSubClass70F         4.051e-02  2.845e-02   1.424 0.154684    
    ## MSSubClass75F         2.089e-02  3.956e-02   0.528 0.597618    
    ## MSSubClass80F         1.233e-02  2.260e-02   0.545 0.585538    
    ## MSSubClass85F         6.431e-03  2.964e-02   0.217 0.828273    
    ## MSZoning              3.880e-02  7.424e-03   5.226 2.02e-07 ***
    ## LotFrontage           5.578e-04  1.839e-04   3.033 0.002471 ** 
    ## LotArea               2.103e-06  3.760e-07   5.594 2.70e-08 ***
    ## Street.L              1.132e-01  3.415e-02   3.315 0.000943 ***
    ## LotConfig.L          -5.073e-02  3.482e-02  -1.457 0.145318    
    ## LotConfig.Q          -5.720e-02  2.987e-02  -1.915 0.055688 .  
    ## LotConfig.C          -4.834e-03  2.008e-02  -0.241 0.809763    
    ## LotConfig^4           2.192e-02  1.339e-02   1.637 0.101856    
    ## LandSlope             1.921e-02  1.327e-02   1.448 0.147974    
    ## NeighborhoodBlueste   8.562e-02  8.441e-02   1.014 0.310608    
    ## NeighborhoodBrDale    5.096e-02  4.860e-02   1.049 0.294489    
    ## NeighborhoodBrkSide   8.965e-02  3.927e-02   2.283 0.022581 *  
    ## NeighborhoodClearCr   6.629e-02  3.860e-02   1.717 0.086147 .  
    ## NeighborhoodCollgCr   8.350e-03  3.088e-02   0.270 0.786926    
    ## NeighborhoodCrawfor   1.533e-01  3.627e-02   4.228 2.53e-05 ***
    ## NeighborhoodEdwards  -1.652e-02  3.444e-02  -0.480 0.631549    
    ## NeighborhoodGilbert   1.209e-02  3.291e-02   0.367 0.713365    
    ## NeighborhoodIDOTRR   -3.143e-03  4.501e-02  -0.070 0.944344    
    ## NeighborhoodMeadowV  -7.053e-02  5.164e-02  -1.366 0.172237    
    ## NeighborhoodMitchel  -9.123e-03  3.459e-02  -0.264 0.792025    
    ## NeighborhoodNAmes     1.067e-02  3.319e-02   0.321 0.747948    
    ## NeighborhoodNoRidge   5.340e-02  3.524e-02   1.515 0.129914    
    ## NeighborhoodNPkVill   6.032e-02  4.844e-02   1.245 0.213200    
    ## NeighborhoodNridgHt   7.142e-02  3.128e-02   2.283 0.022574 *  
    ## NeighborhoodNWAmes   -1.312e-03  3.410e-02  -0.038 0.969323    
    ## NeighborhoodOldTown   4.163e-02  3.991e-02   1.043 0.297120    
    ## NeighborhoodSawyer    1.916e-02  3.477e-02   0.551 0.581722    
    ## NeighborhoodSawyerW   2.546e-02  3.355e-02   0.759 0.447999    
    ## NeighborhoodSomerst   3.951e-02  3.238e-02   1.220 0.222621    
    ## NeighborhoodStoneBr   1.345e-01  3.518e-02   3.824 0.000137 ***
    ## NeighborhoodSWISU     3.729e-02  4.111e-02   0.907 0.364628    
    ## NeighborhoodTimber    2.697e-02  3.453e-02   0.781 0.434895    
    ## NeighborhoodVeenker   7.073e-02  4.533e-02   1.560 0.118901    
    ## Condition1.L          5.627e-02  2.987e-02   1.883 0.059856 .  
    ## Condition1.Q         -1.622e-02  3.293e-02  -0.493 0.622415    
    ## Condition1.C         -1.397e-02  3.659e-02  -0.382 0.702718    
    ## Condition1^4          8.247e-03  3.115e-02   0.265 0.791205    
    ## Condition1^5          1.417e-02  3.953e-02   0.358 0.720122    
    ## Condition1^6          9.752e-03  3.527e-02   0.276 0.782248    
    ## Condition1^7          1.036e-01  4.734e-02   2.188 0.028863 *  
    ## Condition1^8         -3.377e-03  4.225e-02  -0.080 0.936301    
    ## OverallQual           4.453e-02  4.227e-03  10.534  < 2e-16 ***
    ## OverallCond           4.003e-02  3.695e-03  10.832  < 2e-16 ***
    ## YearBuilt             2.186e-03  3.253e-04   6.721 2.69e-11 ***
    ## YearRemodAdd          7.060e-04  2.331e-04   3.028 0.002510 ** 
    ## RoofMatlMembran       2.775e-01  1.146e-01   2.421 0.015614 *  
    ## RoofMatlMetal         1.205e-01  1.101e-01   1.094 0.274061    
    ## RoofMatlRoll         -2.657e-02  1.105e-01  -0.241 0.809941    
    ## RoofMatlTar&Grv       1.588e-02  3.909e-02   0.406 0.684625    
    ## RoofMatlWdShake       2.440e-02  5.385e-02   0.453 0.650563    
    ## RoofMatlWdShngl       1.038e-01  5.035e-02   2.062 0.039449 *  
    ## Exterior1stAsphShn    3.468e-02  1.115e-01   0.311 0.755838    
    ## Exterior1stBrkComm   -1.909e-01  8.685e-02  -2.197 0.028163 *  
    ## Exterior1stBrkFace    6.810e-02  3.180e-02   2.142 0.032390 *  
    ## Exterior1stCBlock    -1.167e-01  1.102e-01  -1.059 0.289594    
    ## Exterior1stCemntBd    2.262e-02  3.312e-02   0.683 0.494699    
    ## Exterior1stHdBoard   -1.598e-03  2.917e-02  -0.055 0.956308    
    ## Exterior1stImStucc    1.220e-02  1.093e-01   0.112 0.911164    
    ## Exterior1stMetalSd    2.809e-02  2.825e-02   0.994 0.320367    
    ## Exterior1stPlywood   -3.523e-03  3.052e-02  -0.115 0.908119    
    ## Exterior1stStone     -2.563e-02  8.658e-02  -0.296 0.767226    
    ## Exterior1stStucco     3.292e-02  3.589e-02   0.917 0.359172    
    ## Exterior1stVinylSd    2.165e-02  2.853e-02   0.759 0.448117    
    ## Exterior1stWd Sdng   -1.346e-03  2.813e-02  -0.048 0.961842    
    ## Exterior1stWdShing    1.118e-02  3.505e-02   0.319 0.749688    
    ## MasVnrType.L          1.107e-02  1.049e-02   1.055 0.291494    
    ## MasVnrType.Q          2.964e-02  1.581e-02   1.875 0.060974 .  
    ## MasVnrType.C          3.877e-02  2.020e-02   1.919 0.055144 .  
    ## ExterCondFa          -1.141e-01  6.788e-02  -1.681 0.093064 .  
    ## ExterCondGd          -9.271e-02  6.318e-02  -1.467 0.142505    
    ## ExterCondPo          -9.913e-02  1.310e-01  -0.757 0.449423    
    ## ExterCondTA          -7.148e-02  6.313e-02  -1.132 0.257718    
    ## FoundationCBlock     -2.007e-03  1.371e-02  -0.146 0.883642    
    ## FoundationPConc       2.832e-02  1.497e-02   1.892 0.058754 .  
    ## FoundationSlab        1.885e-03  3.322e-02   0.057 0.954763    
    ## FoundationStone       7.259e-02  4.636e-02   1.566 0.117616    
    ## FoundationWood       -1.345e-01  6.351e-02  -2.118 0.034385 *  
    ## BsmtExposure          1.418e-02  3.636e-03   3.898 0.000102 ***
    ## BsmtFinType1          3.476e-03  2.169e-03   1.602 0.109329    
    ## BsmtFinSF1            1.434e-04  1.935e-05   7.411 2.25e-13 ***
    ## BsmtFinSF2            1.101e-04  2.423e-05   4.544 6.02e-06 ***
    ## BsmtUnfSF             7.867e-05  1.708e-05   4.605 4.52e-06 ***
    ## HeatingGasA           1.456e-01  1.083e-01   1.345 0.178846    
    ## HeatingGasW           1.795e-01  1.112e-01   1.615 0.106643    
    ## HeatingGrav           1.633e-02  1.172e-01   0.139 0.889250    
    ## HeatingOthW           1.161e-01  1.341e-01   0.866 0.386836    
    ## HeatingWall           1.860e-01  1.247e-01   1.492 0.136040    
    ## HeatingQCFa          -1.192e-02  1.955e-02  -0.610 0.541923    
    ## HeatingQCGd          -1.725e-02  9.025e-03  -1.911 0.056200 .  
    ## HeatingQCPo          -6.975e-02  1.181e-01  -0.591 0.554849    
    ## HeatingQCTA          -2.982e-02  8.838e-03  -3.374 0.000762 ***
    ## CentralAirY           6.920e-02  1.622e-02   4.267 2.12e-05 ***
    ## Electrical.L         -1.739e-01  8.185e-02  -2.124 0.033863 *  
    ## Electrical.Q          1.190e-01  6.991e-02   1.702 0.089082 .  
    ## Electrical.C         -8.385e-02  5.914e-02  -1.418 0.156464    
    ## Electrical^4          3.242e-02  3.928e-02   0.825 0.409254    
    ## `1stFlrSF`            2.705e-04  2.006e-05  13.490  < 2e-16 ***
    ## `2ndFlrSF`            2.553e-04  1.850e-05  13.796  < 2e-16 ***
    ## LowQualFinSF          1.536e-04  6.697e-05   2.294 0.021964 *  
    ## BsmtFullBath          2.137e-02  8.035e-03   2.659 0.007935 ** 
    ## FullBath              2.518e-02  9.315e-03   2.703 0.006956 ** 
    ## HalfBath              2.909e-02  8.978e-03   3.240 0.001226 ** 
    ## KitchenAbvGr         -6.366e-02  2.298e-02  -2.770 0.005692 ** 
    ## KitchenQualFa        -5.843e-02  2.605e-02  -2.243 0.025032 *  
    ## KitchenQualGd        -7.151e-02  1.383e-02  -5.172 2.68e-07 ***
    ## KitchenQualTA        -6.318e-02  1.601e-02  -3.947 8.34e-05 ***
    ## FunctionalMaj2       -2.103e-01  6.180e-02  -3.402 0.000688 ***
    ## FunctionalMin1        3.796e-02  3.625e-02   1.047 0.295272    
    ## FunctionalMin2        1.322e-02  3.606e-02   0.367 0.713942    
    ## FunctionalMod        -6.801e-02  4.369e-02  -1.557 0.119793    
    ## FunctionalSev        -2.484e-01  1.199e-01  -2.072 0.038459 *  
    ## FunctionalTyp         5.903e-02  3.140e-02   1.880 0.060346 .  
    ## FireplaceQu           7.083e-03  2.092e-03   3.386 0.000729 ***
    ## GarageYrBlt          -4.282e-05  2.132e-05  -2.008 0.044814 *  
    ## GarageCars            2.747e-02  9.762e-03   2.814 0.004968 ** 
    ## GarageArea            6.275e-05  3.197e-05   1.963 0.049868 *  
    ## GarageQual            4.032e-02  1.331e-02   3.029 0.002504 ** 
    ## WoodDeckSF            9.075e-05  2.534e-05   3.581 0.000355 ***
    ## OpenPorchSF           1.216e-04  4.946e-05   2.458 0.014092 *  
    ## EnclosedPorch         1.153e-04  5.353e-05   2.155 0.031375 *  
    ## ScreenPorch           2.419e-04  5.283e-05   4.579 5.12e-06 ***
    ## PoolArea              1.362e-04  8.342e-05   1.633 0.102778    
    ## SaleTypeCon           8.033e-02  7.814e-02   1.028 0.304146    
    ## SaleTypeConLD         1.037e-01  4.147e-02   2.501 0.012507 *  
    ## SaleTypeConLI        -1.586e-02  5.067e-02  -0.313 0.754397    
    ## SaleTypeConLw         2.088e-02  5.217e-02   0.400 0.688974    
    ## SaleTypeCWD           7.230e-02  5.679e-02   1.273 0.203202    
    ## SaleTypeNew           1.494e-01  6.705e-02   2.228 0.026025 *  
    ## SaleTypeOth           8.296e-02  6.397e-02   1.297 0.194869    
    ## SaleTypeWD           -5.446e-03  1.817e-02  -0.300 0.764461    
    ## SaleConditionAdjLand  9.255e-02  6.048e-02   1.530 0.126221    
    ## SaleConditionAlloca   5.720e-02  3.644e-02   1.570 0.116743    
    ## SaleConditionPartial -2.603e-02  6.473e-02  -0.402 0.687597    
    ## SaleConditionFamily   2.453e-02  2.671e-02   0.918 0.358704    
    ## SaleConditionNormal   8.326e-02  1.248e-02   6.671 3.74e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1026 on 1307 degrees of freedom
    ## Multiple R-squared:  0.9397, Adjusted R-squared:  0.9329 
    ## F-statistic: 137.6 on 148 and 1307 DF,  p-value: < 2.2e-16

### Stepwise Model

    ## 
    ## Call:
    ## lm(formula = stw.model.formula, data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.67960 -0.04850  0.00175  0.05279  0.49737 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           4.755e+00  7.465e-01   6.369 2.62e-10 ***
    ## OverallQual           4.706e-02  4.160e-03  11.312  < 2e-16 ***
    ## GrLivArea             2.605e-04  1.475e-05  17.662  < 2e-16 ***
    ## NeighborhoodBlueste   7.614e-02  8.448e-02   0.901 0.367570    
    ## NeighborhoodBrDale    4.686e-02  4.863e-02   0.964 0.335440    
    ## NeighborhoodBrkSide   8.548e-02  3.931e-02   2.174 0.029852 *  
    ## NeighborhoodClearCr   7.241e-02  3.790e-02   1.911 0.056263 .  
    ## NeighborhoodCollgCr   5.023e-03  3.089e-02   0.163 0.870850    
    ## NeighborhoodCrawfor   1.453e-01  3.622e-02   4.012 6.36e-05 ***
    ## NeighborhoodEdwards  -1.993e-02  3.446e-02  -0.578 0.563114    
    ## NeighborhoodGilbert   9.481e-03  3.295e-02   0.288 0.773581    
    ## NeighborhoodIDOTRR   -9.340e-03  4.504e-02  -0.207 0.835753    
    ## NeighborhoodMeadowV  -7.376e-02  5.170e-02  -1.427 0.153933    
    ## NeighborhoodMitchel  -1.535e-02  3.457e-02  -0.444 0.657141    
    ## NeighborhoodNAmes     7.739e-03  3.319e-02   0.233 0.815668    
    ## NeighborhoodNoRidge   5.158e-02  3.522e-02   1.465 0.143252    
    ## NeighborhoodNPkVill   5.453e-02  4.843e-02   1.126 0.260379    
    ## NeighborhoodNridgHt   6.940e-02  3.127e-02   2.219 0.026638 *  
    ## NeighborhoodNWAmes   -2.252e-03  3.414e-02  -0.066 0.947413    
    ## NeighborhoodOldTown   3.557e-02  3.992e-02   0.891 0.373090    
    ## NeighborhoodSawyer    1.666e-02  3.478e-02   0.479 0.632148    
    ## NeighborhoodSawyerW   2.211e-02  3.359e-02   0.658 0.510501    
    ## NeighborhoodSomerst   3.823e-02  3.235e-02   1.182 0.237510    
    ## NeighborhoodStoneBr   1.284e-01  3.519e-02   3.649 0.000274 ***
    ## NeighborhoodSWISU     3.271e-02  4.113e-02   0.795 0.426489    
    ## NeighborhoodTimber    2.444e-02  3.446e-02   0.709 0.478198    
    ## NeighborhoodVeenker   7.530e-02  4.515e-02   1.668 0.095643 .  
    ## BsmtFinSF1            6.378e-05  1.212e-05   5.261 1.67e-07 ***
    ## MSSubClass160F       -9.228e-02  2.476e-02  -3.727 0.000202 ***
    ## MSSubClass180F       -1.420e-02  4.380e-02  -0.324 0.745735    
    ## MSSubClass190F        2.009e-02  2.752e-02   0.730 0.465604    
    ## MSSubClass20F         2.468e-02  1.694e-02   1.457 0.145291    
    ## MSSubClass30F        -2.486e-02  2.530e-02  -0.983 0.325995    
    ## MSSubClass40F         1.201e-02  5.619e-02   0.214 0.830805    
    ## MSSubClass45F         4.473e-02  3.787e-02   1.181 0.237739    
    ## MSSubClass50F         2.339e-02  2.243e-02   1.043 0.297198    
    ## MSSubClass60F        -4.103e-03  2.049e-02  -0.200 0.841329    
    ## MSSubClass70F         3.755e-02  2.741e-02   1.370 0.170881    
    ## MSSubClass75F         2.105e-02  3.856e-02   0.546 0.585210    
    ## MSSubClass80F         1.928e-02  2.238e-02   0.862 0.388974    
    ## MSSubClass85F         9.968e-03  2.941e-02   0.339 0.734702    
    ## OverallCond           3.944e-02  3.688e-03  10.695  < 2e-16 ***
    ## YearBuilt             2.123e-03  3.246e-04   6.541 8.76e-11 ***
    ## GarageCars            2.895e-02  9.759e-03   2.966 0.003067 ** 
    ## TotalBsmtSF           8.561e-05  1.451e-05   5.901 4.60e-09 ***
    ## SaleConditionAdjLand  1.020e-01  5.972e-02   1.709 0.087731 .  
    ## SaleConditionAlloca   6.275e-02  3.621e-02   1.733 0.083347 .  
    ## SaleConditionPartial -3.346e-02  6.349e-02  -0.527 0.598250    
    ## SaleConditionFamily   2.607e-02  2.673e-02   0.975 0.329576    
    ## SaleConditionNormal   8.639e-02  1.244e-02   6.944 5.96e-12 ***
    ## LotArea               1.971e-06  3.471e-07   5.678 1.67e-08 ***
    ## MSZoning              3.778e-02  7.421e-03   5.091 4.07e-07 ***
    ## FunctionalMaj2       -2.094e-01  6.186e-02  -3.385 0.000734 ***
    ## FunctionalMin1        3.961e-02  3.623e-02   1.093 0.274499    
    ## FunctionalMin2        1.313e-02  3.607e-02   0.364 0.715845    
    ## FunctionalMod        -5.078e-02  4.267e-02  -1.190 0.234230    
    ## FunctionalSev        -2.281e-01  1.138e-01  -2.004 0.045306 *  
    ## FunctionalTyp         6.029e-02  3.134e-02   1.924 0.054628 .  
    ## CentralAirY           6.976e-02  1.620e-02   4.306 1.79e-05 ***
    ## KitchenQualFa        -5.857e-02  2.600e-02  -2.253 0.024437 *  
    ## KitchenQualGd        -7.043e-02  1.379e-02  -5.107 3.76e-07 ***
    ## KitchenQualTA        -6.195e-02  1.596e-02  -3.881 0.000109 ***
    ## Condition1.L          5.582e-02  2.960e-02   1.886 0.059519 .  
    ## Condition1.Q         -7.188e-03  3.294e-02  -0.218 0.827288    
    ## Condition1.C         -6.137e-03  3.676e-02  -0.167 0.867435    
    ## Condition1^4          6.145e-03  3.112e-02   0.197 0.843495    
    ## Condition1^5          4.535e-03  3.983e-02   0.114 0.909363    
    ## Condition1^6          4.577e-03  3.527e-02   0.130 0.896757    
    ## Condition1^7          1.032e-01  4.740e-02   2.176 0.029696 *  
    ## Condition1^8          3.760e-04  4.255e-02   0.009 0.992951    
    ## FireplaceQu           6.948e-03  2.078e-03   3.343 0.000851 ***
    ## BsmtExposure          1.360e-02  3.524e-03   3.860 0.000119 ***
    ## BsmtFullBath          2.238e-02  8.027e-03   2.788 0.005384 ** 
    ## ScreenPorch           2.436e-04  5.256e-05   4.634 3.95e-06 ***
    ## Exterior1stAsphShn    4.772e-02  1.097e-01   0.435 0.663560    
    ## Exterior1stBrkComm   -1.887e-01  8.425e-02  -2.240 0.025238 *  
    ## Exterior1stBrkFace    6.397e-02  3.098e-02   2.065 0.039127 *  
    ## Exterior1stCBlock    -1.141e-01  1.101e-01  -1.037 0.300114    
    ## Exterior1stCemntBd    1.955e-02  3.234e-02   0.604 0.545699    
    ## Exterior1stHdBoard   -5.356e-03  2.826e-02  -0.190 0.849714    
    ## Exterior1stImStucc    7.308e-03  1.092e-01   0.067 0.946668    
    ## Exterior1stMetalSd    2.495e-02  2.733e-02   0.913 0.361370    
    ## Exterior1stPlywood   -2.224e-03  2.964e-02  -0.075 0.940193    
    ## Exterior1stStone     -1.854e-03  8.162e-02  -0.023 0.981884    
    ## Exterior1stStucco     2.910e-02  3.520e-02   0.827 0.408642    
    ## Exterior1stVinylSd    1.985e-02  2.760e-02   0.719 0.472291    
    ## Exterior1stWd Sdng   -5.116e-03  2.728e-02  -0.188 0.851256    
    ## Exterior1stWdShing    1.425e-02  3.431e-02   0.415 0.678034    
    ## YearRemodAdd          7.128e-04  2.325e-04   3.066 0.002210 ** 
    ## GarageQual            4.239e-02  1.323e-02   3.205 0.001385 ** 
    ## WoodDeckSF            9.396e-05  2.523e-05   3.724 0.000204 ***
    ## OpenPorchSF           1.257e-04  4.921e-05   2.554 0.010760 *  
    ## Street.L              1.202e-01  3.397e-02   3.537 0.000419 ***
    ## LotConfig.L          -4.929e-02  3.485e-02  -1.414 0.157546    
    ## LotConfig.Q          -5.685e-02  2.989e-02  -1.902 0.057372 .  
    ## LotConfig.C          -3.304e-03  2.011e-02  -0.164 0.869523    
    ## LotConfig^4           2.364e-02  1.329e-02   1.778 0.075568 .  
    ## LotFrontage           5.400e-04  1.827e-04   2.955 0.003179 ** 
    ## FoundationCBlock     -3.271e-03  1.371e-02  -0.239 0.811401    
    ## FoundationPConc       2.607e-02  1.497e-02   1.742 0.081790 .  
    ## FoundationSlab        6.773e-03  3.233e-02   0.209 0.834093    
    ## FoundationStone       6.923e-02  4.634e-02   1.494 0.135403    
    ## FoundationWood       -1.524e-01  6.431e-02  -2.369 0.017965 *  
    ## HeatingGasA           1.396e-01  1.083e-01   1.289 0.197488    
    ## HeatingGasW           1.719e-01  1.111e-01   1.548 0.121921    
    ## HeatingGrav           1.370e-02  1.173e-01   0.117 0.907014    
    ## HeatingOthW           1.031e-01  1.342e-01   0.769 0.442167    
    ## HeatingWall           1.819e-01  1.249e-01   1.457 0.145329    
    ## KitchenAbvGr         -6.415e-02  2.291e-02  -2.800 0.005190 ** 
    ## EnclosedPorch         1.097e-04  5.347e-05   2.051 0.040466 *  
    ## HalfBath              2.844e-02  8.844e-03   3.216 0.001332 ** 
    ## FullBath              2.280e-02  9.290e-03   2.455 0.014233 *  
    ## MasVnrType.L          9.339e-03  1.043e-02   0.896 0.370682    
    ## MasVnrType.Q          3.014e-02  1.580e-02   1.908 0.056670 .  
    ## MasVnrType.C          3.873e-02  2.022e-02   1.915 0.055673 .  
    ## BsmtFinSF2            3.522e-05  1.991e-05   1.769 0.077195 .  
    ## HeatingQCFa          -1.394e-02  1.952e-02  -0.714 0.475096    
    ## HeatingQCGd          -1.785e-02  9.009e-03  -1.981 0.047831 *  
    ## HeatingQCPo          -6.760e-02  1.182e-01  -0.572 0.567483    
    ## HeatingQCTA          -2.966e-02  8.814e-03  -3.365 0.000786 ***
    ## GarageArea            5.952e-05  3.197e-05   1.862 0.062826 .  
    ## SaleTypeCon           7.669e-02  7.818e-02   0.981 0.326785    
    ## SaleTypeConLD         1.061e-01  4.149e-02   2.557 0.010657 *  
    ## SaleTypeConLI        -1.508e-02  5.072e-02  -0.297 0.766312    
    ## SaleTypeConLw         1.924e-02  5.219e-02   0.369 0.712388    
    ## SaleTypeCWD           7.443e-02  5.683e-02   1.310 0.190470    
    ## SaleTypeNew           1.594e-01  6.589e-02   2.419 0.015701 *  
    ## SaleTypeOth           9.053e-02  6.396e-02   1.415 0.157176    
    ## SaleTypeWD           -6.254e-03  1.813e-02  -0.345 0.730232    
    ## ExterCondFa          -1.119e-01  6.792e-02  -1.648 0.099595 .  
    ## ExterCondGd          -8.582e-02  6.321e-02  -1.358 0.174775    
    ## ExterCondPo          -1.002e-01  1.306e-01  -0.767 0.443187    
    ## ExterCondTA          -6.541e-02  6.316e-02  -1.036 0.300557    
    ## PoolArea              1.330e-04  8.220e-05   1.618 0.106007    
    ## BsmtFinType1          3.247e-03  2.167e-03   1.498 0.134341    
    ## GarageYrBlt          -4.607e-05  2.122e-05  -2.171 0.030080 *  
    ## Electrical.L         -1.799e-01  8.191e-02  -2.196 0.028263 *  
    ## Electrical.Q          1.225e-01  6.998e-02   1.751 0.080175 .  
    ## Electrical.C         -8.451e-02  5.910e-02  -1.430 0.152970    
    ## Electrical^4          3.500e-02  3.923e-02   0.892 0.372464    
    ## `3SsnPorch`           1.348e-04  9.743e-05   1.383 0.166755    
    ## LowQualFinSF         -9.088e-05  6.708e-05  -1.355 0.175698    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1027 on 1314 degrees of freedom
    ## Multiple R-squared:  0.9392, Adjusted R-squared:  0.9327 
    ## F-statistic:   144 on 141 and 1314 DF,  p-value: < 2.2e-16

## Cross Validation

### Custom Model CV

    ## Linear Regression 
    ## 
    ## 1456 samples
    ##   46 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 1310, 1309, 1311, 1312, 1310, 1311, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared   MAE       
    ##   0.112956  0.9199085  0.07836509
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

    ## [1] 16.71946

### Forward Model CV

    ## Linear Regression 
    ## 
    ## 1456 samples
    ##   46 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 1311, 1310, 1311, 1310, 1310, 1310, ... 
    ## Resampling results:
    ## 
    ##   RMSE       Rsquared   MAE       
    ##   0.1151777  0.9166196  0.07925884
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

    ## [1] 17.38363

### Backward Model CV

    ## Linear Regression 
    ## 
    ## 1456 samples
    ##   48 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 1312, 1309, 1310, 1311, 1312, 1309, ... 
    ## Resampling results:
    ## 
    ##   RMSE       Rsquared   MAE       
    ##   0.1159665  0.9163436  0.07987755
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

    ## [1] 17.62257

### Stepwise Model CV

    ## Linear Regression 
    ## 
    ## 1456 samples
    ##   46 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 1311, 1311, 1310, 1309, 1310, 1310, ... 
    ## Resampling results:
    ## 
    ##   RMSE       Rsquared   MAE       
    ##   0.1153389  0.9159369  0.07920589
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

    ## [1] 17.43234
