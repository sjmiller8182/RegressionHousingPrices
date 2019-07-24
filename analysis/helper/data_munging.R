#' Flattens a correlation and p-value matrix
#' 
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
