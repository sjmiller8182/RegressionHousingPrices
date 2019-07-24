Exploratory\_Data\_Analysis
================
Stuart Miller
July 23, 2019

# Setup

``` r
# libraries
library(tidyverse)
library(naniar)
library(Hmisc)

# helper files
source('../helper/data_munging.R')
```

# Initial Data Exploration

### Load the data into R

``` r
train <- read_csv('../data/train.csv')
test <- read_csv('../data/test.csv')
```

## Structure of the data

There are 79 features to the data in total. 43 of these features are
categorical and 36 of these features are numeric. The training set
consists of 1460 observations and the test set consists of 1459
observations. The training and test sets include an `Id` column, which
is not a feature of the dataset.

``` r
# split the training set into numeric and non-numeric sets 
train.numeric <- train %>% select_if(is.numeric)
train.nonnumeric <- train %>% select_if(is.character)

# get the dimensions
print('dimensions of the numeric features (training)')
```

    ## [1] "dimensions of the numeric features (training)"

``` r
dim(train.numeric)
```

    ## [1] 1460   38

``` r
print('dimensions of the non-numeric features (training)')
```

    ## [1] "dimensions of the non-numeric features (training)"

``` r
dim(train.nonnumeric)
```

    ## [1] 1460   43

``` r
test.numeric <- test %>% select_if(is.numeric)
test.nonnumeric <- test %>% select_if(is.character)

# get the dimensions
print('dimensions of the numeric features (testing)')
```

    ## [1] "dimensions of the numeric features (testing)"

``` r
dim(test.numeric)
```

    ## [1] 1459   37

``` r
print('dimensions of the non-numeric features (testing)')
```

    ## [1] "dimensions of the non-numeric features (testing)"

``` r
dim(test.nonnumeric)
```

    ## [1] 1459   43

``` r
# Print out the data structure
#str(train)
```

## Missing Data

``` r
train.nabular <- train %>% bind_shadow()
train.missing.summary <- train.nabular %>% miss_var_summary()
#train.missing.summary

train.missing.summary %>%
  filter(n_miss > 0) %>%
  ggplot(aes(x = reorder(variable, -n_miss), y = pct_miss)) + geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  labs(title = 'Percent Missing Values of Features (Training Set)',
       y = '% Observations Missing', x = 'Feature Name')
```

![](Exploratory_Data_Analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
test.nabular <- test %>% bind_shadow()
test.missing.summary <- test.nabular %>% miss_var_summary()
#test.missing.summary

test.missing.summary %>%
  filter(n_miss > 0) %>%
  ggplot(aes(x = reorder(variable, -n_miss), y = pct_miss)) + geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  labs(title = 'Percent Missing Values of Features (Test Set)',
       y = '% Observations Missing', x = 'Feature Name')
```

![](Exploratory_Data_Analysis_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

**Summary of the Data Missing per Feature from the
Datasets**

| Variable     | Number Missing (Train) | Percent Missing (Train) | Number Missing (Test) | Percent Missing (Test) |
| ------------ | ---------------------- | ----------------------- | --------------------- | ---------------------- |
| PoolQC       | 1453                   | 99.5%                   | 1456                  | 99.7%                  |
| MiscFeature  | 1406                   | 96.3%                   | 1408                  | 96.5%                  |
| Alley        | 1369                   | 93.7%                   | 1352                  | 92.6%                  |
| Fence        | 1179                   | 80.7%                   | 1169                  | 80.1%                  |
| FireplaceQu  | 690                    | 47.2%                   | 730                   | 50.0%                  |
| LotFrontage  | 259                    | 17.8%                   | 227                   | 15.5%                  |
| GarageType   | 81                     | 5.55%                   | 76                    | 5.21%                  |
| GarageYrBlt  | 81                     | 5.55%                   | 78                    | 5.34%                  |
| GarageFinish | 81                     | 5.55%                   | 78                    | 5.34%                  |
| GarageQual   | 81                     | 5.55%                   | 78                    | 5.34%                  |
| GarageCond   | 81                     | 5.55%                   | 78                    | 5.34%                  |
| BsmtExposure | 38                     | 2.60%                   | 44                    | 3.01%                  |
| BsmtFinType2 | 38                     | 2.60%                   | 42                    | 2.87%                  |
| BsmtQual     | 37                     | 2.53%                   | 44                    | 3.01%                  |
| BsmtCond     | 37                     | 2.53%                   | 45                    | 3.08%                  |
| BsmtFinType1 | 37                     | 2.53%                   | 42                    | 2.87%                  |
| MasVnrType   | 8                      | 0.55%                   | 16                    | 0.27%                  |
| MasVnrArea   | 8                      | 0.55%                   | 15                    | 0.13%                  |
| Electrical   | 1                      | 0.07%                   | N/A                   | N/A                    |
| MSZoning     | N/A                    | N/A                     | 4                     | 0.27%                  |
| Utilities    | N/A                    | N/A                     | 2                     | 0.14%                  |
| BsmtFullBath | N/A                    | N/A                     | 2                     | 0.14%                  |
| BsmtHalfBath | N/A                    | N/A                     | 2                     | 0.14%                  |
| Functional   | N/A                    | N/A                     | 2                     | 0.14%                  |
| Exterior1st  | N/A                    | N/A                     | 1                     | 0.07%                  |
| Exterior2nd  | N/A                    | N/A                     | 1                     | 0.07%                  |
| BsmtFinSF1   | N/A                    | N/A                     | 1                     | 0.07%                  |
| BsmtFinSF2   | N/A                    | N/A                     | 1                     | 0.07%                  |
| BsmtUnfSF    | N/A                    | N/A                     | 1                     | 0.07%                  |
| TotalBsmtSF  | N/A                    | N/A                     | 1                     | 0.07%                  |
| KitchenQual  | N/A                    | N/A                     | 1                     | 0.07%                  |
| GarageCars   | N/A                    | N/A                     | 1                     | 0.07%                  |
| GarageArea   | N/A                    | N/A                     | 1                     | 0.07%                  |
| SaleType     | N/A                    | N/A                     | 1                     | 0.07%                  |

**Summary of Missing Data**

5 of the features, `PoolQC`, `MiscFeature`, `Alley`, `Fence`, and
`FireplaceQu`, have very high missing rates in both sets of data. It
seems unlikely that these features would provide value as imbuing values
for the missing values would likely bias the training set towards the
imbued value. The feature LotFrontage is missing at a rate of ~17% and ~
15% in the training and testing set respectively. This is also a fairly
high missing rate, but the usefulness of the feature should be
investigated. Fortunately, the rate of missingness for each feature is
approximately the same between the training and testing
datasets.

## Correlation of Numeric Features (Training Set)

``` r
train.dropna <- train.numeric %>% drop_na() %>% select(-one_of(c('SalePrice')))
res2<-rcorr(as.matrix(train.dropna))
flattenedCor <- flattenCorrMatrix(res2$r, res2$P) %>% arrange(desc(cor))
flattenedCor[1:15,]
```

    ##             row       column       cor p
    ## 1    GarageCars   GarageArea 0.8394149 0
    ## 2   TotalBsmtSF     1stFlrSF 0.8359994 0
    ## 3     GrLivArea TotRmsAbvGrd 0.8243121 0
    ## 4     YearBuilt  GarageYrBlt 0.8235195 0
    ## 5      2ndFlrSF    GrLivArea 0.6882916 0
    ## 6    BsmtFinSF1 BsmtFullBath 0.6517267 0
    ## 7  BedroomAbvGr TotRmsAbvGrd 0.6502846 0
    ## 8  YearRemodAdd  GarageYrBlt 0.6458085 0
    ## 9     YearBuilt YearRemodAdd 0.6231713 0
    ## 10     2ndFlrSF TotRmsAbvGrd 0.6177759 0
    ## 11    GrLivArea     FullBath 0.6148873 0
    ## 12  OverallQual    GrLivArea 0.6074661 0
    ## 13     2ndFlrSF     HalfBath 0.6063367 0
    ## 14  GarageYrBlt   GarageCars 0.6009034 0
    ## 15  OverallQual   GarageCars 0.5938029 0

**Table of Correlated Features from Training Set (r \> 0.5)**

| Feature 1    | Feature 2    | Correlation |
| ------------ | ------------ | ----------- |
| GarageCars   | GarageArea   | 0.8394149   |
| TotalBsmtSF  | 1stFlrSF     | 0.8359994   |
| GrLivArea    | TotRmsAbvGrd | 0.8243121   |
| YearBuilt    | GarageYrBlt  | 0.8235195   |
| 2ndFlrSF     | GrLivArea    | 0.6882916   |
| BsmtFinSF1   | BsmtFullBath | 0.6517267   |
| BedroomAbvGr | TotRmsAbvGrd | 0.6502846   |
| YearRemodAdd | GarageYrBlt  | 0.6458085   |
| YearBuilt    | YearRemodAdd | 0.6231713   |
| 2ndFlrSF     | TotRmsAbvGrd | 0.6177759   |
| GrLivArea    | FullBath     | 0.6148873   |
| OverallQual  | GrLivArea    | 0.6074661   |
| 2ndFlrSF     | HalfBath     | 0.6063367   |
| GarageYrBlt  | GarageCars   | 0.6009034   |
| OverallQual  | GarageCars   | 0.5938029   |
| GarageYrBlt  | GarageArea   | 0.5926352   |
| OverallQual  | YearBuilt    | 0.5893845   |
| OverallQual  | FullBath     | 0.5768747   |
| OverallQual  | YearRemodAdd | 0.5707571   |
| OverallQual  | TotalBsmtSF  | 0.5639597   |
| 1stFlrSF     | GrLivArea    | 0.5613723   |
| OverallQual  | GarageYrBlt  | 0.5604251   |
| OverallQual  | GarageArea   | 0.5506589   |
| FullBath     | TotRmsAbvGrd | 0.5404489   |
| YearBuilt    | GarageCars   | 0.5325628   |
| BsmtFinSF1   | TotalBsmtSF  | 0.5309165   |
| TotalBsmtSF  | GarageArea   | 0.5220512   |
| 1stFlrSF     | GarageArea   | 0.521183    |
| FullBath     | GarageCars   | 0.520857    |
| OverallQual  | 1stFlrSF     | 0.5144529   |
