#' Flattens a correlation and p-value matrix
#' 
#' @description
#' Uses the output of Hmisc::rcorr to create a flattened
#' table of pearson's r correlation coefficients and
#' p-values between features.
#'
#' Function sourced from http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
#'
#' @param cormat A correlation matrix.
#' @param pmat A p-value matrix.
#'
#' @example
#' res2<-rcorr(as.matrix(df))
#' flattenCorrMatrix(res2$r, res2$P) %>% arrange(desc(cor))
#' 
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
    )
}

#' Creates dummy variables (columns) for given column
#'
#' @param data A dataframe.
#' @param column A categorical column in data.
#' @param reference A value in the column to use a reference.
#' @param as.onehot Set to TRUE to use onehot encoding.
#'
get.dummies <- function(data, column, reference, as.onehot = FALSE) {
  # get the levels of the factor in column
  lev <- levels(data[[column]])
  # do not remove reference for onehot encoding
  if (!as.onehot) {
    # remove the reference value
    lev <- lev[lev != reference]
  }
  # add encodings
  for (fct in lev){
    new_col <- paste(column, fct, sep = '_')
    data[new_col] <- as.numeric(data[, column] == fct)
    print(new_col)
  }
  data
}
