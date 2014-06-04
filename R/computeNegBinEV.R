#' A slTools function to simulate quantities of interest for negative binomial models
#' 
#' This function follows Zelig's simulation method and computes expected and predicted values for negative binomial models fitted to multiply imputed datasets. 
#' @param data imputed datasets in amelia object
#' @param x a vector of values for predictors
#' @param coef a matrix of coefficients
#' @param vcov a list of variance covariance matrices
#' @param num number of simulations
#' @param theta a vector of theta parameters from each fitted negative binomial model results
#' @export 

computeNegBinEV <- function(data = NULL, x = NULL, coef = NULL, vcov = NULL, num = NULL, theta = NULL) {
  #simulate parameter values from the multivariate normal distribution, num draws
  set.seed(1234)
  beta.draws <- NULL
  for(i in 1:length(data)) {
    beta.draws <- rbind(beta.draws, mvrnorm(num, as.numeric(coef[i,]), vcov[[i]]))
  }
  
  #use some matrix algebra to calculate systematic component for each draw
  sys.comp <- as.matrix(x) %*% t(beta.draws)
  
  #for each draw caculate expected value (predicted probabilities) for negative binomial
  ev <- exp(sys.comp)
  
  # compute predicted values
  pv <- matrix(NA, nrow = nrow(ev), ncol = ncol(ev))
  for (i in 1:ncol(ev))
    pv[,i] <- rnegbin(nrow(ev), mu = ev[,i], theta = mean(theta))
  
  # Return
  list(ev = ev, pv = pv) 
}
