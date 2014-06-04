#' A sltools function to simulate quantities of interest for hurdle models
#' 
#' This function simulates quantities of interests from a list of hurdle model outputs fitted to amelia MI objects.
#' @param data imputed datasets in amelia object
#' @param x a vector of values for predictors
#' @param x1 a second vector of values for predictors
#' @param hurdle.out a list of hurdle model outputs
#' @param num number of simulations
#' @export 

simHurdle <- function(data = NULL, x = NULL, x1 = NULL, hurdle.out = NULL, num = NULL){
  library(plyr)
  qi.zero <- qiLogit(data = data, x = x, x1 = x1,
                     coef = as.data.frame(laply(hurdle.out, function(x) coef(x, model = "zero"))), 
                     vcov = llply(hurdle.out, function(x) vcov(x, model = "zero")), 
                     num = num)
  fd.zero <- data.frame(var_0 = quantile(qi.zero[[1]], probs = c(.025, .5, .975)),
                        var_1 = quantile(qi.zero[[2]], probs = c(.025, .5, .975)),
                        var_fd = quantile(qi.zero[[5]], probs = c(.025, .5, .975)))
  qi.negbin <- qiNegBin(data = data, x = x, x1 = x1,
                        coef = as.data.frame(laply(hurdle.out, function(x) coef(x, model = "count"))), 
                        vcov = llply(hurdle.out, function(x) vcov(x, model = "count")), 
                        num = num, theta = ldply(hurdle.out, function(x) x$theta)$count)
  fd.negbin <- data.frame(var_0 = quantile(qi.negbin[[1]], probs = c(.025, .5, .975)),
                          var_1 = quantile(qi.negbin[[2]], probs = c(.025, .5, .975)),
                          var_fd = quantile(qi.negbin[[5]], probs = c(.025, .5, .975)))
  list("qi.zero" = qi.zero,
       "qi.count" = qi.negbin,
       "fd.zero" = fd.zero,
       "fd.count" = fd.negbin)
}
