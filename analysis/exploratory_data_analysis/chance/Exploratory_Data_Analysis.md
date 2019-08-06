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

# Setup

``` r
# libraries
library(knitr)
library(tidyverse)
library(naniar)
library(Hmisc)
library(GGally)
# Correlations
library(corrr)
# Stepwise Regression
library(MASS)

# helper files
source('../../helper/data_munging.R')
```

# Exploratory Analysis

### Load the data into R

``` r
train <- read_csv('../../data/train.csv')
test <- read_csv('../../data/test.csv')

train <- train %>% mutate(logSalePrice = log(SalePrice))
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

### Handle null values for categorical variables

  - Alley
  - MasVnrType
  - BsmtQual
  - BsmtCond
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

<!-- end list -->

``` r
train$Alley[is.na(train$Alley)] <- 'None'
test$Alley[is.na(test$Alley)] <- 'None'

train$MasVnrType[is.na(train$MasVnrType)] <- 'None'
test$MasVnrType[is.na(test$MasVnrType)] <- 'None'

train$BsmtQual[is.na(train$BsmtQual)] <- 'None'
test$BsmtQual[is.na(test$BsmtQual)] <- 'None'

train$BsmtCond[is.na(train$BsmtCond)] <- 'None'
test$BsmtCond[is.na(test$BsmtCond)] <- 'None'

train$BsmtExposure[is.na(train$BsmtExposure)] <- 'None'
test$BsmtExposure[is.na(test$BsmtExposure)] <- 'None'

train$BsmtFinType1[is.na(train$BsmtFinType1)] <- 'None'
test$BsmtFinType1[is.na(test$BsmtFinType1)] <- 'None'

train$BsmtFinType2[is.na(train$BsmtFinType2)] <- 'None'
test$BsmtFinType2[is.na(test$BsmtFinType2)] <- 'None'

train$FireplaceQu[is.na(train$FireplaceQu)] <- 'None'
test$FireplaceQu[is.na(test$FireplaceQu)] <- 'None'

train$GarageType[is.na(train$GarageType)] <- 'None'
test$GarageType[is.na(test$GarageType)] <- 'None'

train$GarageFinish[is.na(train$GarageFinish)] <- 'None'
test$GarageFinish[is.na(test$GarageFinish)] <- 'None'

train$GarageQual[is.na(train$GarageQual)] <- 'None'
test$GarageQual[is.na(test$GarageQual)] <- 'None'

train$GarageCond[is.na(train$GarageCond)] <- 'None'
test$GarageCond[is.na(test$GarageCond)] <- 'None'

train$PoolQC[is.na(train$PoolQC)] <- 'None'
test$PoolQC[is.na(test$PoolQC)] <- 'None'

train$Fence[is.na(train$Fence)] <- 'None'
test$Fence[is.na(test$Fence)] <- 'None'

train$MiscFeature[is.na(train$MiscFeature)] <- 'None'
test$MiscFeature[is.na(test$MiscFeature)] <- 'None'

train$Electrical[is.na(train$Electrical)] <- 'SBrkr'
test$Electrical[is.na(test$Electrical)] <- 'SBrkr'
```

``` r
train$Alley <- ordered(train$Alley, levels = c("None", "Grvl", "Pave"))
test$Alley <- ordered(test$Alley, levels = c("None", "Grvl", "Pave"))

train$MasVnrType <- ordered(train$MasVnrType, levels = c("None", "CBlock", "BrkFace", "BrkCmn", "Stone"))
test$MasVnrType <- ordered(test$MasVnrType, levels = c("None", "CBlock", "BrkFace", "BrkCmn", "Stone"))

train$BsmtQual <- ordered(train$BsmtQual, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
test$BsmtQual <- ordered(test$BsmtQual, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))

train$BsmtCond <- ordered(train$BsmtCond, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
test$BsmtCond <- ordered(test$BsmtCond, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))

train$BsmtExposure <- ordered(train$BsmtExposure, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
test$BsmtExposure <- ordered(test$BsmtExposure, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))

train$BsmtFinType1 <- ordered(train$BsmtFinType1, levels = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
test$BsmtFinType1 <- ordered(test$BsmtFinType1, levels = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))

train$BsmtFinType2 <- ordered(train$BsmtFinType2, levels = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
test$BsmtFinType2 <- ordered(test$BsmtFinType2, levels = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))

train$FireplaceQu <- ordered(train$FireplaceQu, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
test$FireplaceQu <- ordered(test$FireplaceQu, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))

train$GarageType <- ordered(train$GarageType, levels = c("None", "CarPort", "2Types", "Basment", "Detchd", "Attchd", "BuiltIn"))
test$GarageType <- ordered(test$GarageType, levels = c("None", "CarPort", "2Types", "Basment", "Detchd", "Attchd", "BuiltIn"))

train$GarageFinish <- ordered(train$GarageFinish, levels = c("None", "Unf", "RFn", "Fin"))
test$GarageFinish <- ordered(test$GarageFinish, levels = c("None", "Unf", "RFn", "Fin"))

train$GarageQual <- ordered(train$GarageQual, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
test$GarageQual <- ordered(test$GarageQual, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))

train$GarageCond <- ordered(train$GarageCond, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
test$GarageCond <- ordered(test$GarageCond, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))

train$PoolQC <- ordered(train$PoolQC, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
test$PoolQC <- ordered(test$PoolQC, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))

train$Fence <- ordered(train$Fence, levels = c("None", "MnWw", "GdWo", "MnPrv", "GdPrv"))
test$Fence <- ordered(test$Fence, levels = c("None", "MnWw", "GdWo", "MnPrv", "GdPrv"))

train$Electrical <- ordered(train$Electrical, levels = c("Mix", "FuseP", "FuseF", "FuseA", "SBrkr"))
test$Electrical <- ordered(test$Electrical, levels = c("Mix", "FuseP", "FuseF", "FuseA", "SBrkr"))

train$MiscFeature <- ordered(train$MiscFeature, levels = c("None", "Othr", "Shed", "Gar2", "TenC"))
test$MiscFeature <- ordered(test$MiscFeature, levels = c("None", "Othr", "Shed", "Gar2", "TenC"))
```

``` r
train.nonnumeric <- train %>% 
  select_if(is.character)

describe(train.nonnumeric)
```

    ## train.nonnumeric 
    ## 
    ##  27  Variables      1460  Observations
    ## ---------------------------------------------------------------------------
    ## MSZoning 
    ##        n  missing distinct 
    ##     1460        0        5 
    ##                                                   
    ## Value      C (all)      FV      RH      RL      RM
    ## Frequency       10      65      16    1151     218
    ## Proportion   0.007   0.045   0.011   0.788   0.149
    ## ---------------------------------------------------------------------------
    ## Street 
    ##        n  missing distinct 
    ##     1460        0        2 
    ##                       
    ## Value       Grvl  Pave
    ## Frequency      6  1454
    ## Proportion 0.004 0.996
    ## ---------------------------------------------------------------------------
    ## LotShape 
    ##        n  missing distinct 
    ##     1460        0        4 
    ##                                   
    ## Value        IR1   IR2   IR3   Reg
    ## Frequency    484    41    10   925
    ## Proportion 0.332 0.028 0.007 0.634
    ## ---------------------------------------------------------------------------
    ## LandContour 
    ##        n  missing distinct 
    ##     1460        0        4 
    ##                                   
    ## Value        Bnk   HLS   Low   Lvl
    ## Frequency     63    50    36  1311
    ## Proportion 0.043 0.034 0.025 0.898
    ## ---------------------------------------------------------------------------
    ## Utilities 
    ##        n  missing distinct 
    ##     1460        0        2 
    ##                         
    ## Value      AllPub NoSeWa
    ## Frequency    1459      1
    ## Proportion  0.999  0.001
    ## ---------------------------------------------------------------------------
    ## LotConfig 
    ##        n  missing distinct 
    ##     1460        0        5 
    ##                                                   
    ## Value       Corner CulDSac     FR2     FR3  Inside
    ## Frequency      263      94      47       4    1052
    ## Proportion   0.180   0.064   0.032   0.003   0.721
    ## ---------------------------------------------------------------------------
    ## LandSlope 
    ##        n  missing distinct 
    ##     1460        0        3 
    ##                             
    ## Value        Gtl   Mod   Sev
    ## Frequency   1382    65    13
    ## Proportion 0.947 0.045 0.009
    ## ---------------------------------------------------------------------------
    ## Neighborhood 
    ##        n  missing distinct 
    ##     1460        0       25 
    ## 
    ## lowest : Blmngtn Blueste BrDale  BrkSide ClearCr
    ## highest: Somerst StoneBr SWISU   Timber  Veenker
    ## ---------------------------------------------------------------------------
    ## Condition1 
    ##        n  missing distinct 
    ##     1460        0        9 
    ##                                                                          
    ## Value      Artery  Feedr   Norm   PosA   PosN   RRAe   RRAn   RRNe   RRNn
    ## Frequency      48     81   1260      8     19     11     26      2      5
    ## Proportion  0.033  0.055  0.863  0.005  0.013  0.008  0.018  0.001  0.003
    ## ---------------------------------------------------------------------------
    ## Condition2 
    ##        n  missing distinct 
    ##     1460        0        8 
    ##                                                                   
    ## Value      Artery  Feedr   Norm   PosA   PosN   RRAe   RRAn   RRNn
    ## Frequency       2      6   1445      1      2      1      1      2
    ## Proportion  0.001  0.004  0.990  0.001  0.001  0.001  0.001  0.001
    ## ---------------------------------------------------------------------------
    ## BldgType 
    ##        n  missing distinct 
    ##     1460        0        5 
    ##                                              
    ## Value        1Fam 2fmCon Duplex  Twnhs TwnhsE
    ## Frequency    1220     31     52     43    114
    ## Proportion  0.836  0.021  0.036  0.029  0.078
    ## ---------------------------------------------------------------------------
    ## HouseStyle 
    ##        n  missing distinct 
    ##     1460        0        8 
    ##                                                                   
    ## Value      1.5Fin 1.5Unf 1Story 2.5Fin 2.5Unf 2Story SFoyer   SLvl
    ## Frequency     154     14    726      8     11    445     37     65
    ## Proportion  0.105  0.010  0.497  0.005  0.008  0.305  0.025  0.045
    ## ---------------------------------------------------------------------------
    ## RoofStyle 
    ##        n  missing distinct 
    ##     1460        0        6 
    ##                                                           
    ## Value         Flat   Gable Gambrel     Hip Mansard    Shed
    ## Frequency       13    1141      11     286       7       2
    ## Proportion   0.009   0.782   0.008   0.196   0.005   0.001
    ## ---------------------------------------------------------------------------
    ## RoofMatl 
    ##        n  missing distinct 
    ##     1460        0        8 
    ##                                                                           
    ## Value      ClyTile CompShg Membran   Metal    Roll Tar&Grv WdShake WdShngl
    ## Frequency        1    1434       1       1       1      11       5       6
    ## Proportion   0.001   0.982   0.001   0.001   0.001   0.008   0.003   0.004
    ## ---------------------------------------------------------------------------
    ## Exterior1st 
    ##        n  missing distinct 
    ##     1460        0       15 
    ##                                                                           
    ## Value      AsbShng AsphShn BrkComm BrkFace  CBlock CemntBd HdBoard ImStucc
    ## Frequency       20       1       2      50       1      61     222       1
    ## Proportion   0.014   0.001   0.001   0.034   0.001   0.042   0.152   0.001
    ##                                                                   
    ## Value      MetalSd Plywood   Stone  Stucco VinylSd Wd Sdng WdShing
    ## Frequency      220     108       2      25     515     206      26
    ## Proportion   0.151   0.074   0.001   0.017   0.353   0.141   0.018
    ## ---------------------------------------------------------------------------
    ## Exterior2nd 
    ##        n  missing distinct 
    ##     1460        0       16 
    ##                                                                           
    ## Value      AsbShng AsphShn Brk Cmn BrkFace  CBlock CmentBd HdBoard ImStucc
    ## Frequency       20       3       7      25       1      60     207      10
    ## Proportion   0.014   0.002   0.005   0.017   0.001   0.041   0.142   0.007
    ##                                                                           
    ## Value      MetalSd   Other Plywood   Stone  Stucco VinylSd Wd Sdng Wd Shng
    ## Frequency      214       1     142       5      26     504     197      38
    ## Proportion   0.147   0.001   0.097   0.003   0.018   0.345   0.135   0.026
    ## ---------------------------------------------------------------------------
    ## ExterQual 
    ##        n  missing distinct 
    ##     1460        0        4 
    ##                                   
    ## Value         Ex    Fa    Gd    TA
    ## Frequency     52    14   488   906
    ## Proportion 0.036 0.010 0.334 0.621
    ## ---------------------------------------------------------------------------
    ## ExterCond 
    ##        n  missing distinct 
    ##     1460        0        5 
    ##                                         
    ## Value         Ex    Fa    Gd    Po    TA
    ## Frequency      3    28   146     1  1282
    ## Proportion 0.002 0.019 0.100 0.001 0.878
    ## ---------------------------------------------------------------------------
    ## Foundation 
    ##        n  missing distinct 
    ##     1460        0        6 
    ##                                                     
    ## Value      BrkTil CBlock  PConc   Slab  Stone   Wood
    ## Frequency     146    634    647     24      6      3
    ## Proportion  0.100  0.434  0.443  0.016  0.004  0.002
    ## ---------------------------------------------------------------------------
    ## Heating 
    ##        n  missing distinct 
    ##     1460        0        6 
    ##                                               
    ## Value      Floor  GasA  GasW  Grav  OthW  Wall
    ## Frequency      1  1428    18     7     2     4
    ## Proportion 0.001 0.978 0.012 0.005 0.001 0.003
    ## ---------------------------------------------------------------------------
    ## HeatingQC 
    ##        n  missing distinct 
    ##     1460        0        5 
    ##                                         
    ## Value         Ex    Fa    Gd    Po    TA
    ## Frequency    741    49   241     1   428
    ## Proportion 0.508 0.034 0.165 0.001 0.293
    ## ---------------------------------------------------------------------------
    ## CentralAir 
    ##        n  missing distinct 
    ##     1460        0        2 
    ##                       
    ## Value          N     Y
    ## Frequency     95  1365
    ## Proportion 0.065 0.935
    ## ---------------------------------------------------------------------------
    ## KitchenQual 
    ##        n  missing distinct 
    ##     1460        0        4 
    ##                                   
    ## Value         Ex    Fa    Gd    TA
    ## Frequency    100    39   586   735
    ## Proportion 0.068 0.027 0.401 0.503
    ## ---------------------------------------------------------------------------
    ## Functional 
    ##        n  missing distinct 
    ##     1460        0        7 
    ##                                                     
    ## Value       Maj1  Maj2  Min1  Min2   Mod   Sev   Typ
    ## Frequency     14     5    31    34    15     1  1360
    ## Proportion 0.010 0.003 0.021 0.023 0.010 0.001 0.932
    ## ---------------------------------------------------------------------------
    ## PavedDrive 
    ##        n  missing distinct 
    ##     1460        0        3 
    ##                             
    ## Value          N     P     Y
    ## Frequency     90    30  1340
    ## Proportion 0.062 0.021 0.918
    ## ---------------------------------------------------------------------------
    ## SaleType 
    ##        n  missing distinct 
    ##     1460        0        9 
    ##                                                                 
    ## Value        COD   Con ConLD ConLI ConLw   CWD   New   Oth    WD
    ## Frequency     43     2     9     5     5     4   122     3  1267
    ## Proportion 0.029 0.001 0.006 0.003 0.003 0.003 0.084 0.002 0.868
    ## ---------------------------------------------------------------------------
    ## SaleCondition 
    ##        n  missing distinct 
    ##     1460        0        6 
    ##                                                           
    ## Value      Abnorml AdjLand  Alloca  Family  Normal Partial
    ## Frequency      101       4      12      20    1198     125
    ## Proportion   0.069   0.003   0.008   0.014   0.821   0.086
    ## ---------------------------------------------------------------------------

Based on the plots below, the following features appear to be linearly
related to log of sale price:

  - `OverallQual`
  - `YearBuilt`
  - `YearRemodAdd`

<!-- end list -->

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

    ## # A tibble: 38 x 2
    ##    rowname      SalePrice
    ##    <chr>            <dbl>
    ##  1 logSalePrice     0.948
    ##  2 OverallQual      0.791
    ##  3 GrLivArea        0.709
    ##  4 GarageCars       0.640
    ##  5 GarageArea       0.623
    ##  6 TotalBsmtSF      0.614
    ##  7 1stFlrSF         0.606
    ##  8 FullBath         0.561
    ##  9 TotRmsAbvGrd     0.534
    ## 10 YearBuilt        0.523
    ## # ... with 28 more rows

``` r
base.model <- lm(log(SalePrice) ~
                 OverallQual +
                 GrLivArea +
                 GarageArea +
                 TotalBsmtSF +
                 YearBuilt +
                 Neighborhood,
               data = train)
summary(base.model)
```

    ## 
    ## Call:
    ## lm(formula = log(SalePrice) ~ OverallQual + GrLivArea + GarageArea + 
    ##     TotalBsmtSF + YearBuilt + Neighborhood, data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.96494 -0.06923  0.01072  0.08342  0.50679 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          7.314e+00  6.211e-01  11.777  < 2e-16 ***
    ## OverallQual          9.424e-02  5.316e-03  17.728  < 2e-16 ***
    ## GrLivArea            2.248e-04  1.151e-05  19.523  < 2e-16 ***
    ## GarageArea           2.336e-04  2.663e-05   8.770  < 2e-16 ***
    ## TotalBsmtSF          8.749e-05  1.261e-05   6.937 6.05e-12 ***
    ## YearBuilt            1.808e-03  3.132e-04   5.771 9.65e-09 ***
    ## NeighborhoodBlueste -1.077e-01  1.196e-01  -0.901 0.367793    
    ## NeighborhoodBrDale  -2.402e-01  5.681e-02  -4.229 2.50e-05 ***
    ## NeighborhoodBrkSide -9.973e-03  4.924e-02  -0.203 0.839510    
    ## NeighborhoodClearCr  1.970e-01  5.074e-02   3.882 0.000108 ***
    ## NeighborhoodCollgCr  4.921e-02  4.098e-02   1.201 0.230004    
    ## NeighborhoodCrawfor  2.028e-01  4.858e-02   4.174 3.18e-05 ***
    ## NeighborhoodEdwards -7.232e-02  4.473e-02  -1.617 0.106146    
    ## NeighborhoodGilbert  6.099e-02  4.319e-02   1.412 0.158159    
    ## NeighborhoodIDOTRR  -1.964e-01  5.225e-02  -3.759 0.000178 ***
    ## NeighborhoodMeadowV -1.824e-01  5.647e-02  -3.230 0.001268 ** 
    ## NeighborhoodMitchel  1.132e-02  4.578e-02   0.247 0.804695    
    ## NeighborhoodNAmes    2.181e-02  4.260e-02   0.512 0.608713    
    ## NeighborhoodNoRidge  1.484e-01  4.723e-02   3.141 0.001719 ** 
    ## NeighborhoodNPkVill -5.180e-02  6.634e-02  -0.781 0.435051    
    ## NeighborhoodNridgHt  1.563e-01  4.324e-02   3.615 0.000311 ***
    ## NeighborhoodNWAmes   3.495e-02  4.409e-02   0.793 0.428104    
    ## NeighborhoodOldTown -8.674e-02  4.819e-02  -1.800 0.072104 .  
    ## NeighborhoodSawyer   2.390e-02  4.512e-02   0.530 0.596478    
    ## NeighborhoodSawyerW  2.125e-02  4.443e-02   0.478 0.632548    
    ## NeighborhoodSomerst  6.133e-02  4.255e-02   1.441 0.149684    
    ## NeighborhoodStoneBr  1.867e-01  5.044e-02   3.702 0.000222 ***
    ## NeighborhoodSWISU   -1.320e-02  5.573e-02  -0.237 0.812755    
    ## NeighborhoodTimber   1.156e-01  4.683e-02   2.468 0.013713 *  
    ## NeighborhoodVeenker  2.233e-01  6.209e-02   3.596 0.000334 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1593 on 1430 degrees of freedom
    ## Multiple R-squared:  0.8441, Adjusted R-squared:  0.8409 
    ## F-statistic: 266.9 on 29 and 1430 DF,  p-value: < 2.2e-16

``` r
test$predicted.log.price <- predict.lm(base.model, test)

test$predicted.log.price[is.na(test$predicted.log.price)] <- mean(test$predicted.log.price, na.rm=TRUE)
  
submit <- test %>%
  mutate(SalePrice = exp(predicted.log.price)) %>%
  subset(select = c(Id, SalePrice))
head(submit)
```

    ## # A tibble: 6 x 2
    ##      Id SalePrice
    ##   <dbl>     <dbl>
    ## 1  1461   133380.
    ## 2  1462   151535.
    ## 3  1463   165388.
    ## 4  1464   180504.
    ## 5  1465   236442.
    ## 6  1466   177134.

``` r
# write.csv(submit, file = "./kaggle_submission.csv")
```
