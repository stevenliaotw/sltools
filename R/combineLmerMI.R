#' Combine lmer model results fitted to multiply imputed datasets
#' 
#' This sltools function allows you to combine lmer results from the lme4 package, extract results for the texreg package, and also output a summary table.
#' @param fitted.obj a list of lmer fitted model outputs from the lme4 package
#' @export
#' @import lme4
 
combineLmerMI <- function(fitted.obj = NULL) {
  # extract parameters
  m <- length(fitted.obj)
  coef <- as.data.frame(laply(fitted.obj, function(x) fixef(x))) 
  se <- as.data.frame(laply(fitted.obj, function(x) vcov(x)@factors$correlation@sd)) 
  n <- sapply(fitted.obj, function(x) nobs(x))
  loglik <- sapply(fitted.obj, function(x) logLik(x))
  aic <- sapply(fitted.obj, function(x) AIC(x))
  vcov <- llply(fitted.obj, function(x) vcov(x))
  
  # tranpose
  coef <- t(coef)
  se <- t(se)
  
  # get rows
  rows <- nrow(coef)
  
  # calculate mean coef
  Q <- apply(coef, 1, mean)
  
  # calculate within imputation variance based on Rubin's rules
  U <- apply(se^2, 1, mean)
  
  # calculate between imputation variance based on Rubin's rules
  B <- apply((coef - Q)^2, 1, sum)/(m - 1)
  
  # calculate overall variance Schafer (1997)
  var <- U + (1 + 1/m)*B
  
  # calculate degrees of freedom
  nu <- (m - 1)*(1 + U/((1 + 1/m)*B))^2
  
  # make estimates table
  coef.table <- matrix(NA, nrow = rows, ncol = 4)
  dimnames(coef.table) <- list(rownames(coef),
                               c("Coef", "Std. Error", "t-stat", "p-value"))
  coef.table[,1] <- Q
  coef.table[,2] <- sqrt(var)
  coef.table[,3] <- Q/sqrt(var)
  coef.table[,4] <- pt(abs(Q/sqrt(var)), df = nu, lower.tail = F)*2 # calculate pvalue based on t score and degree of freedom
  
  # extract results for texreg table
  texreg <- texregMI(names = rownames(coef.table), 
                     coef = coef.table[,1], 
                     se = coef.table[,2], 
                     pval = coef.table[,4], 
                     n = median(n),
                     loglik = mean(loglik),
                     aic = mean(aic))
  
  # make list
  list("var.names" = rownames(coef.table),
       "coef" = coef.table[,1],
       "se" = coef.table[,2],
       "pvalue" = coef.table[,4],
       "n" = median(n),
       "loglik" = mean(loglik),
       "aic" = mean(aic),
       "sum.table" = coef.table,
       "texreg" = texreg
  )
}
