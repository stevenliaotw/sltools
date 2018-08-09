#' texreg extracting function for Zelig linear models fitted with robust SEs
#' 
#' This function extracts parameters from Zelig 3.5.5 lm objects with robust SEs and stores them in a new texreg object called "texregLmRobust," which feeds into the texreg package.
#' @param names vector of variable names
#' @param coef vector of coefficients
#' @param se vector of standard errors
#' @param pval vector of p-values
#' @param n scalar of sample size
#' @name texregLmRobust
#' @rdname texregLmRobust
#' @export texregLmRobust
#' @import methods

# First, create a class definition for your regression objects. Let's call them "texregMI":
#library(texreg)
setClass(Class = "texregLmRobust", 
         representation = representation(names = "character",
                                         coef = "numeric",
                                         se = "numeric",
                                         pval = "numeric",
                                         n = "numeric",
                                         r.squared = "numeric",
                                         adj.r.squared = "numeric")
)

# Next, create a constructor that allows you to create new objects:
texregLmRobust <- function(names, coef, se, pval, n, r.squared, adj.r.squared) {
  new("texregLmRobust", names = names, coef = coef, se = se, pval = pval, n = n, r.squared = r.squared, adj.r.squared = adj.r.squared)
}

# Then write an extension that translates texregLmRobust objects into texreg objects:
extract.texregLmRobust <- function(model) {
  tr <- createTexreg(
    coef.names = model@names, 
    coef = model@coef, 
    se = model@se, 
    pvalues = model@pval, 
    gof.names = c("Num. obs.", "R$^2$", "Adj. R$^2$"), 
    gof = c(model@n, model@r.squared, model@adj.r.squared), 
    gof.decimal = c(FALSE, TRUE, TRUE)
  )
  return(tr)
}

# Tell texreg that this extension should actually be used for texregLmRobust objects:
setMethod("extract", signature = className("texregLmRobust"), 
          definition = extract.texregLmRobust)