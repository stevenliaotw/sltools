#' A sltools function to simulate first differences for negative binomial models
#' 
#' This function uses the \code{computeNegBinEV} function to simulate expected values, predicted values, and first differences for negative binomial models.
#' @param data imputed datasets in amelia object
#' @param x a vector of values for predictors
#' @param x1 a second vector of values for predictors
#' @param coef a matrix of coefficients
#' @param vcov a list of variance covariance matrices
#' @param num number of simulations
#' @param theta a vector of theta parameters from each fitted negative binomial model results
#' @export 
qiNegBin <- function(data = NULL, x = NULL, x1 = NULL, coef = NULL, vcov = NULL, num = 10000, theta = NULL) {
  qi1 <- computeNegBinEV(data, x, coef, vcov, num, theta)
  qi2 <- computeNegBinEV(data, x1, coef, vcov, num, theta)
  
  # Return quantities of interest, paired off with their titles
  list("Expected Values: E(Y|X)"  = qi1$ev,
       "Expected Values: E(Y|X1)" = qi2$ev,
       "Predicted Values: Y|X"    = qi1$pv,
       "Predicted Values: Y|X1"   = qi2$pv,
       "First Differences: E(Y|X1) - E(Y|X)" = qi2$ev - qi1$ev
  )
}
