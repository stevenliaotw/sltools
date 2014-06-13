#' texreg extracting function for relogit models fitted to multiply imputed data
#' 
#' This function extracts parameters from multiple imputation relogit objects and stores them in a new texreg object called "texregMIrelogit," which feeds into the texreg package.
#' @param names vector of variable names
#' @param coef vector of coefficients
#' @param se vector of standard errors
#' @param pval vector of p-values
#' @param n scalar of sample size
#' @param aic scalar of AIC
#' @name texregMIrelogit
#' @rdname texregMIrelogit
#' @export texregMIrelogit

#library(texreg)
# First, create a class definition for relogit regression objects. Let's call them "texregMIrelogit":
setClass(Class = "texregMIrelogit", 
         representation = representation(names = "character",
                                         coef = "numeric",
                                         se = "numeric",
                                         pval = "numeric",
                                         n = "numeric",
                                         aic = "numeric")
)

# Next, create a constructor that allows you to create new objects:
texregMIrelogit <- function(names, coef, se, pval, n, aic) {
  new("texregMIrelogit", names = names, coef = coef, se = se, pval = pval, n = n, aic = aic)
}

# Then write an extension that translates texregMIrelogit objects into texreg objects:
extract.texregMIrelogit <- function(model) {
  tr <- createTexreg(
    coef.names = model@names, 
    coef = model@coef, 
    se = model@se, 
    pvalues = model@pval, 
    gof.names = c("Num obs.", "AIC"), 
    gof = c(model@n, model@aic), 
    gof.decimal = c(FALSE, TRUE)
  )
  return(tr)
}

# Tell texreg that this extension should actually be used for texregMI objects:
setMethod("extract", signature = className("texregMIrelogit"), 
          definition = extract.texregMIrelogit)