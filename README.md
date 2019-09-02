# Regression Housing Prices

Created by [Stuart Miller](https://github.com/sjmiller8182), [Paul Adams](https://github.com/PaulAdams4361), and [Chance Robinson](https://github.com/RobinsonCW).

## Purpose

Project using multiple linear regression to model prices of houses in Ames, IA.

## Analysis Files

* [Regression Analysis Paper (pdf)](https://github.com/sjmiller8182/RegressionHousingPrices/blob/master/analysis/HousePriceRegressionAnalysis.pdf): A paper covering the analysis. 
* [Regression Analysis (Rmd)](https://github.com/sjmiller8182/RegressionHousingPrices/blob/master/analysis/HousePriceRegressionAnalysis.Rmd): A markdown file with the analysis.

## Data

The [data description](https://github.com/sjmiller8182/RegressionHousingPrices/blob/master/analysis/data/data_description.txt) contains general information features of the dataset. The dataset [readme](https://github.com/sjmiller8182/RegressionHousingPrices/blob/master/analysis/data/readme.md) provides a reference for the dataset. The data was pre-split into training and testing sets by Kaggle.com.

* [`test.csv`](https://github.com/sjmiller8182/RegressionHousingPrices/blob/master/analysis/data/test.csv): the testing dataset.
* [`train.csv`](https://github.com/sjmiller8182/RegressionHousingPrices/blob/master/analysis/data/train.csv): the training dataset.

## Codebook

The [Codebook](https://github.com/sjmiller8182/RegressionHousingPrices/blob/master/CodeBook.md) provides additional details on the regarding the computational environment, code, and data.

## Repo Structure
    .
    ├── analysis                            # Primary analysis files
    |    ├── exploratory_data_analysis      # Rmarkdown files for EDA
    |    ├── studies                        # Rmarkdown files for primary analysis questions
    |    ├── helper                         # Files containing helper functions
    │    └── data                           # Raw data and merge automation files
    ├── CodeBook.md                         # Information regarding the computational environment, code, and data
    ├── LICENSE                             # All code and analysis is licensed under the MIT license. Data is provided by Kaggle.com.
    └── README.md
