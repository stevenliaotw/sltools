#' A slTools function to simulate quantities of interest for logit models
#' 
#' This function follows Zelig's simulation method and computes expected and predicted values for logit models fitted to multiply imputed datasets. 
#' @param data imputed datasets in amelia object
#' @param x a vector of values for predictors
#' @param coef a matrix of coefficients
#' @param vcov a list of variance covariance matrices
#' @param num number of simulations
#' @export 

computeLogitEV <- function(data = NULL, x = NULL, coef = NULL, vcov = NULL, num = NULL) {
  #simulate parameter values from the multivariate normal distribution, num draws
  set.seed(1234)
  beta.draws <- NULL
  for(i in 1:length(data)) {
    beta.draws <- rbind(beta.draws, mvrnorm(num, as.numeric(coef[i,]), vcov[[i]]))
  }
  
  #use some matrix algebra to calculate systematic component for each draw
  sys.comp <- as.matrix(x) %*% t(beta.draws)
  
  #for each draw caculate expected value (predicted probabilities) for logit
  ev <- 1/(1 + exp(-sys.comp))
  
  # simulate predicted values based on ev
  pv <- matrix(NA, nrow = nrow(ev), ncol = ncol(ev))
  set.seed(1234)
  for (i in 1:ncol(ev))
    pv[,i] <- rbinom(nrow(ev), prob = ev[,i], size = 1)
  
  # Return
  list(ev = ev, pv = pv) 
}
