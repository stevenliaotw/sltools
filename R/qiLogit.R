#' A slTools function to simulate first differences for logit models
#' 
#' This function uses the \code{computeLogitEV} function to simulate expected values, predicted values, and first differences for logit models.
#' @param data imputed datasets in amelia object
#' @param x a vector of values for predictors
#' @param x1 a second vector of values for predictors
#' @param coef a matrix of coefficients
#' @param vcov a list of variance covariance matrices
#' @param num number of simulations
#' @export 
qiLogit <- function(data = NULL, x = NULL, x1 = NULL, coef = NULL, vcov = NULL, num = 10000) {
  qi1 <- computeLogitEV(data, x, coef, vcov, num)
  qi2 <- computeLogitEV(data, x1, coef, vcov, num)
  
  list("Expected Values: E(Y|X)"  = qi1$ev,
       "Expected Values: E(Y|X1)" = qi2$ev,
       "Predicted Values: Y|X"    = qi1$pv,
       "Predicted Values: Y|X1"   = qi2$pv,
       "First Differences: E(Y|X1) - E(Y|X)" = qi2$ev - qi1$ev
  )
}