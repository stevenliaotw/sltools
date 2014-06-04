## Extract texregMIrelogit object parameters
library(texreg)
# First, create a class definition for relogit regression objects. Let's call them "texregMIrelogit":
setClass(Class = "texregMIrelogit", 
         representation=representation(names = "character",
                                       coef = "numeric",
                                       se = "numeric",
                                       pval = "numeric",
                                       n = "numeric",
                                       aic = "numeric")
)

# Then write an extension that translates texregMIrelogit objects into texreg objects:
extract.texregMIrelogit <- function(model) {
  tr <- createTexreg(
    coef.names = model@names, 
    coef = model@coef, 
    se = model@se, 
    pvalues = model@pval, 
    gof.names = c("Num obs.", "AIC"), 
    gof = c(model@n, model@aic), 
    gof.decimal = c(FALSE,TRUE)
  )
  return(tr)
}

# Tell texreg that this extension should actually be used for texregMI objects:
setMethod("extract", signature = className("texregMIrelogit"), 
          definition = extract.texregMIrelogit)