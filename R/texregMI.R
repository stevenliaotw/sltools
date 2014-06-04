#' texreg extracting function for models fitted to multiply imputed data
#' 
#' This function extracts parameters from multiple imputation objects and stores them in a new texreg object called "texregMI," which feeds into the texreg package.
#' @param names vector of variable names
#' @param coef vector of coefficients
#' @param se vector of standard errors
#' @param pval vector of p-values
#' @param n scalar of sample size
#' @param loglik scalar of log-likelihood
#' @param aic scalar of AIC
#' @name texregMI
#' @rdname texregMI
#' @export texregMI

# First, create a class definition for your regression objects. Let's call them "texregMI":
library(texreg)
setClass(Class = "texregMI", 
         representation = representation(names = "character",
                                         coef = "numeric",
                                         se = "numeric",
                                         pval = "numeric",
                                         n = "numeric",
                                         loglik = "numeric",
                                         aic = "numeric")
)

# Next, create a constructor that allows you to create new objects:
texregMI <- function(names, coef, se, pval, n, loglik, aic) {
  new("texregMI", names = names, coef = coef, se = se, pval = pval, n = n, loglik = loglik, aic = aic)
}

# Then write an extension that translates texregMI objects into texreg objects:
extract.texregMI <- function(model) {
    tr <- createTexreg(
    coef.names = model@names, 
    coef = model@coef, 
    se = model@se, 
    pvalues = model@pval, 
    gof.names = c("Num obs.", "Log Likelihood", "AIC"), 
    gof = c(model@n, model@loglik, model@aic), 
    gof.decimal = c(FALSE, TRUE, TRUE)
  )
  return(tr)
}

# Tell texreg that this extension should actually be used for texregMI objects:
setMethod("extract", signature = className("texregMI"), 
          definition = extract.texregMI)