Ames Iowa Housing Prices (Analysis Two)
================
Paul Adams
August 3, 2019

``` r
library(tidyverse)
library(MASS)
library(caret)
library(knitr)

load('../../data/data.RData')
```

## Model

“By Hand” constuction.

Based on the correlation heatmap the following parameters have a
reasonible correlation with the log of `SalePrice` without high mutual
correlation.

  - `GarageArea`
  - `OverallQual`
  - `GrLivArea`
  - `TotalBsmtSF`
  - `BsmtQual`
  - `YearRemodAdd`
  - `Fireplaces`
  - `YearBuilt`

Based on EDA, `TotalBsmtSF` varies by `BsmtQual`, so it will be included
as an interaction. Living room areas and year built tend to vary by
neighborhood, so neighborhood will be included as an interation on these
variable.

This model produces an adjusted R-squared of 0.882, which is an estimate
of the variability in logSalePrice that is explained by the model.

    ## 
    ## Call:
    ## lm(formula = model.formula, data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.82439 -0.06507  0.00779  0.07637  0.72432 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    2.899e+00  7.109e-01   4.078 4.81e-05 ***
    ## GarageArea                     2.281e-04  2.357e-05   9.677  < 2e-16 ***
    ## OverallQual                    7.060e-02  5.004e-03  14.108  < 2e-16 ***
    ## BsmtQualEx                     2.973e-01  5.158e-02   5.765 1.00e-08 ***
    ## BsmtQualFa                     2.949e-01  8.824e-02   3.342 0.000852 ***
    ## BsmtQualGd                     3.970e-02  3.162e-02   1.256 0.209395    
    ## BsmtQualTA                     3.102e-02  3.230e-02   0.960 0.337088    
    ## YearRemodAdd                   2.341e-03  2.443e-04   9.580  < 2e-16 ***
    ## Fireplaces                     6.248e-02  7.036e-03   8.880  < 2e-16 ***
    ## GrLivArea:NeighborhoodBlmngtn  5.299e-04  2.488e-04   2.130 0.033322 *  
    ## GrLivArea:NeighborhoodBlueste  5.470e-04  5.933e-04   0.922 0.356723    
    ## GrLivArea:NeighborhoodBrDale   7.048e-04  2.278e-04   3.094 0.002017 ** 
    ## GrLivArea:NeighborhoodBrkSide  4.708e-04  4.836e-05   9.735  < 2e-16 ***
    ## GrLivArea:NeighborhoodClearCr  2.002e-04  5.616e-05   3.565 0.000377 ***
    ## GrLivArea:NeighborhoodCollgCr  2.230e-04  2.800e-05   7.963 3.45e-15 ***
    ## GrLivArea:NeighborhoodCrawfor  2.392e-04  3.644e-05   6.563 7.39e-11 ***
    ## GrLivArea:NeighborhoodEdwards -3.474e-05  2.736e-05  -1.270 0.204403    
    ## GrLivArea:NeighborhoodGilbert  2.149e-04  5.273e-05   4.076 4.84e-05 ***
    ## GrLivArea:NeighborhoodIDOTRR   6.315e-04  8.059e-05   7.836 9.17e-15 ***
    ## GrLivArea:NeighborhoodMeadowV  1.530e-04  7.323e-05   2.089 0.036867 *  
    ## GrLivArea:NeighborhoodMitchel  1.338e-04  5.352e-05   2.500 0.012541 *  
    ## GrLivArea:NeighborhoodNAmes    1.210e-04  2.382e-05   5.079 4.31e-07 ***
    ## GrLivArea:NeighborhoodNoRidge  2.698e-04  3.809e-05   7.083 2.22e-12 ***
    ## GrLivArea:NeighborhoodNPkVill  2.148e-04  2.046e-04   1.050 0.294117    
    ## GrLivArea:NeighborhoodNridgHt  3.261e-04  3.876e-05   8.412  < 2e-16 ***
    ## GrLivArea:NeighborhoodNWAmes   2.418e-04  3.911e-05   6.182 8.30e-10 ***
    ## GrLivArea:NeighborhoodOldTown  2.513e-04  2.487e-05  10.103  < 2e-16 ***
    ## GrLivArea:NeighborhoodSawyer   1.301e-04  4.737e-05   2.746 0.006112 ** 
    ## GrLivArea:NeighborhoodSawyerW  2.939e-04  3.635e-05   8.083 1.35e-15 ***
    ## GrLivArea:NeighborhoodSomerst  3.163e-04  5.154e-05   6.138 1.09e-09 ***
    ## GrLivArea:NeighborhoodStoneBr  3.372e-04  4.987e-05   6.762 2.00e-11 ***
    ## GrLivArea:NeighborhoodSWISU    2.199e-04  4.443e-05   4.950 8.33e-07 ***
    ## GrLivArea:NeighborhoodTimber   2.899e-04  5.800e-05   4.998 6.51e-07 ***
    ## GrLivArea:NeighborhoodVeenker  3.857e-04  1.386e-04   2.783 0.005455 ** 
    ## TotalBsmtSF:BsmtQual0                 NA         NA      NA       NA    
    ## TotalBsmtSF:BsmtQualEx        -4.715e-06  2.727e-05  -0.173 0.862735    
    ## TotalBsmtSF:BsmtQualFa        -2.458e-04  1.097e-04  -2.241 0.025203 *  
    ## TotalBsmtSF:BsmtQualGd         1.317e-04  1.688e-05   7.801 1.19e-14 ***
    ## TotalBsmtSF:BsmtQualTA         1.326e-04  2.196e-05   6.040 1.97e-09 ***
    ## NeighborhoodBlmngtn:YearBuilt  1.453e-03  3.622e-04   4.012 6.33e-05 ***
    ## NeighborhoodBlueste:YearBuilt  1.423e-03  5.271e-04   2.699 0.007030 ** 
    ## NeighborhoodBrDale:YearBuilt   1.342e-03  3.429e-04   3.913 9.57e-05 ***
    ## NeighborhoodBrkSide:YearBuilt  1.564e-03  3.205e-04   4.878 1.19e-06 ***
    ## NeighborhoodClearCr:YearBuilt  1.802e-03  3.204e-04   5.624 2.26e-08 ***
    ## NeighborhoodCollgCr:YearBuilt  1.723e-03  3.170e-04   5.433 6.52e-08 ***
    ## NeighborhoodCrawfor:YearBuilt  1.782e-03  3.223e-04   5.528 3.86e-08 ***
    ## NeighborhoodEdwards:YearBuilt  1.862e-03  3.209e-04   5.801 8.16e-09 ***
    ## NeighborhoodGilbert:YearBuilt  1.725e-03  3.190e-04   5.407 7.54e-08 ***
    ## NeighborhoodIDOTRR:YearBuilt   1.390e-03  3.236e-04   4.297 1.85e-05 ***
    ## NeighborhoodMeadowV:YearBuilt  1.656e-03  3.209e-04   5.160 2.82e-07 ***
    ## NeighborhoodMitchel:YearBuilt  1.762e-03  3.175e-04   5.549 3.43e-08 ***
    ## NeighborhoodNAmes:YearBuilt    1.796e-03  3.179e-04   5.650 1.94e-08 ***
    ## NeighborhoodNoRidge:YearBuilt  1.706e-03  3.194e-04   5.342 1.07e-07 ***
    ## NeighborhoodNPkVill:YearBuilt  1.689e-03  3.466e-04   4.874 1.22e-06 ***
    ## NeighborhoodNridgHt:YearBuilt  1.654e-03  3.168e-04   5.223 2.03e-07 ***
    ## NeighborhoodNWAmes:YearBuilt   1.701e-03  3.205e-04   5.306 1.30e-07 ***
    ## NeighborhoodOldTown:YearBuilt  1.654e-03  3.181e-04   5.201 2.28e-07 ***
    ## NeighborhoodSawyer:YearBuilt   1.778e-03  3.194e-04   5.566 3.13e-08 ***
    ## NeighborhoodSawyerW:YearBuilt  1.657e-03  3.185e-04   5.201 2.27e-07 ***
    ## NeighborhoodSomerst:YearBuilt  1.652e-03  3.171e-04   5.208 2.20e-07 ***
    ## NeighborhoodStoneBr:YearBuilt  1.679e-03  3.217e-04   5.218 2.08e-07 ***
    ## NeighborhoodSWISU:YearBuilt    1.715e-03  3.256e-04   5.269 1.59e-07 ***
    ## NeighborhoodTimber:YearBuilt   1.678e-03  3.203e-04   5.239 1.86e-07 ***
    ## NeighborhoodVeenker:YearBuilt  1.669e-03  3.368e-04   4.954 8.15e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1372 on 1397 degrees of freedom
    ## Multiple R-squared:  0.8871, Adjusted R-squared:  0.8821 
    ## F-statistic:   177 on 62 and 1397 DF,  p-value: < 2.2e-16

## Model Cross Validation

The RMSE and adjusted R-squared are estimated with 10 fold cross
validation.

RMSE: 0.15 Adj R-squared: 0.86

``` r
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model.cv <- train(model.formula, 
                    data = train,
                    method = 'lm',
                    trControl = train.control)
# print model summary
model.cv
```

    ## Linear Regression 
    ## 
    ## 1460 samples
    ##    9 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 1315, 1313, 1313, 1314, 1313, 1315, ... 
    ## Resampling results:
    ## 
    ##   RMSE       Rsquared   MAE      
    ##   0.1474066  0.8613325  0.1030492
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE
