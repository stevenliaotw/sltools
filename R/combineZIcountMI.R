#' Combine zero-inflated count model results fitted to multiply imputed datasets
#' 
#' This sltools function allows you to combine multiple imputation zero-inflated model results from the pscl package, extract results for the texreg package, and also output summary tables.#' @param fitted.obj a list of hurdle fitted model outputs from the pscl package
#' @export

combineZIcountMI <- function(fitted.obj = NULL) {
  # extract parameters
  m <- length(fitted.obj)
  coef.count <- as.data.frame(laply(fitted.obj, function(x) coef(x, model = "count"))) 
  se.count <- as.data.frame(laply(fitted.obj, function(x) sqrt(diag(vcov(x, model = "count"))))) 
  coef.zero <- as.data.frame(laply(fitted.obj, function(x) coef(x, model = "zero")))
  se.zero <- as.data.frame(laply(fitted.obj, function(x) sqrt(diag(vcov(x, model = "zero")))))
  n <- sapply(fitted.obj, function(x) x$n)
  loglik <- sapply(fitted.obj, function(x) logLik(x))
  aic <- sapply(fitted.obj, function(x) AIC(x))
  theta <- sapply(fitted.obj, function(x) x$theta)
  vcov.count <- llply(fitted.obj, function(x) vcov(x, model = "count"))
  vcov.zero <- llply(fitted.obj, function(x) vcov(x, model = "zero"))
  
  # tranpose
  coef.count <- t(coef.count)
  se.count <- t(se.count)
  coef.zero <- t(coef.zero)
  se.zero <- t(se.zero)
  
  # get rows
  rows <- nrow(coef.count)
  
  # calculate mean coef
  Q.count <- apply(coef.count, 1, mean)
  Q.zero <- apply(coef.zero, 1, mean)
  
  # calculate within imputation variance based on Rubin's rules
  U.count <- apply(se.count^2, 1, mean)
  U.zero <- apply(se.zero^2, 1, mean)
  
  # calculate between imputation variance based on Rubin's rules
  B.count <- apply((coef.count - Q.count)^2, 1, sum)/(m - 1)
  B.zero <- apply((coef.zero - Q.zero)^2, 1, sum)/(m - 1)
  
  # calculate overall variance Schafer (1997)
  var.count <- U.count + (1 + 1/m)*B.count
  var.zero <- U.zero + (1 + 1/m)*B.zero
  
  # calculate degrees of freedom
  nu.count <- (m - 1)*(1 + U.count/((1 + 1/m)*B.count))^2
  nu.zero <- (m - 1)*(1 + U.zero/((1 + 1/m)*B.zero))^2
  
  # create table for parameters
  coef.table <- matrix(NA, nrow = rows, ncol = 8)
  dimnames(coef.table) <- list(rownames(coef.count),
                               c("Zero:Coef", "Zero:Std. Error", "Zero:t-stat", "Zero:p-value", "Count:Coef", "Count:Std. Error", "Count:t-stat", "Count:p-value"))
  coef.table[,1] <- Q.zero
  coef.table[,2] <- sqrt(var.zero)
  coef.table[,3] <- Q.zero/sqrt(var.zero)
  coef.table[,4] <- pt(abs(Q.zero/sqrt(var.zero)), df = nu.zero, lower.tail = F)*2 # calculate pvalue based on t score and degree of freedom
  coef.table[,5] <- Q.count
  coef.table[,6] <- sqrt(var.count)
  coef.table[,7] <- Q.count/sqrt(var.count)
  coef.table[,8] <- pt(abs(Q.count/sqrt(var.count)), df = nu.count, lower.tail = F)*2
  
  # extract results for texreg table
  texreg.count <- texregMI(names = rownames(coef.table), 
                           coef = coef.table[,5], 
                           se = coef.table[,6], 
                           pval = coef.table[,8], 
                           n = median(n),
                           loglik = mean(loglik),
                           aic = mean(aic))
  texreg.zero <- texregMI(names = rownames(coef.table), 
                          coef = coef.table[,1], 
                          se = coef.table[,2], 
                          pval = coef.table[,4], 
                          n = median(n),
                          loglik = mean(loglik),
                          aic = mean(aic))
  
  # make list
  list("var.names" = rownames(coef.table),
       "zero.coef" = coef.table[,1],
       "zero.se" = coef.table[,2],
       "zero.pvalue" = coef.table[,4],
       "count.coef" = coef.table[,5],
       "count.se" = coef.table[,6],
       "count.pvalue" = coef.table[,8],
       "n" = median(n),
       "loglik" = mean(loglik),
       "aic" = mean(aic),
       "theta" = mean(theta),
       "sum.table" = coef.table,
       "texreg.zero" = texreg.zero,
       "texreg.count" = texreg.count
  )
}
