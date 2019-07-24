#' Calculate PRESS Score
#' 
#' @description
#' Calculate PRESS score from predicted values and
#' true values. This is the total mean squared error
#'
#' @param y_true The true values corresponding to the input.
#' @param y_hat The predicted/fitted values of the model.
#'
PRESS <- function(y_true, y_hat) {
  sum((y_true - y_hat)^2)
}

#' Calculate MSE Score
#' 
#' @description
#' Calculate the mean of the square prediction error or
#' mean squared error (MSE).
#'
#' @param y_true The true values corresponding to the input.
#' @param y_hat The predicted/fitted values of the model.
#'
MSE <- function(y_true, y_hat) {
  sum((y_true - y_hat)^2)/length(y_hat)
}
