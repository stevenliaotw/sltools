#' Plotting Coefficients and Confidence Intervals
#' 
#' This sltools function allows you to extract coefficients from a list of models and create side-by-side coefplots.
#' @param models a list of models
#' @param alpha scalar of alpha
#' @param model.names list of model names
#' @param var.order vector of variable ordering
#' @param var.names list of variable names
#' @param omit.intercept whether to omit intercept, default = TRUE
#' @param omit.factor whether to omit factors, default = TRUE
#' @export

coefficientPlot <- function(models, alpha = 0.05, model.names = "", var.order = "", var.names = NULL, omit.intercept = TRUE, omit.factor = TRUE){
  
  Multiplier <- qnorm(1 - alpha / 2)
  
  CoefficientTables <- ldply(1:length(models), function(x) {
    out <- as.data.frame(summary(models[[x]])$coef)
    out$variable <- rownames(summary(models[[x]])$coef)
    
    if(model.names[1] == ""){
      out$model <- paste("Model", x, sep = "")
    } else {
      out$model <- model.names[[x]]
    }
    
    return(out)
  })
  
  colnames(CoefficientTables) <- c("Estimate", "StandardError", "TValue", "PValue", "IV", "ModelName")
  
  # exclude rows of intercepts
  if(omit.intercept == TRUE){
    CoefficientTables <- filter(CoefficientTables, !grepl("Intercept", IV)) 
  } else {
    CoefficientTables <- CoefficientTables
  }
  
  # exclude rows of intercepts and factor
  if(omit.factor == TRUE){
    CoefficientTables <- filter(CoefficientTables, !grepl("factor", IV))
  } else {
    CoefficientTables <- CoefficientTables
  }
  
  # re-order the levels of variables in the order of appearance in the data.frame
  if(var.order[1] == ""){
    CoefficientTables$IV <- factor(CoefficientTables$IV, levels = rev(unique(as.character(CoefficientTables$IV))))
  } else {
    CoefficientTables$IV <- factor(CoefficientTables$IV, levels = rev(var.order))
  }
  
  # re-order the levels of model names based on user input
  CoefficientTables$ModelName <- factor(CoefficientTables$ModelName, levels = model.names)
  
  #library(ggplot2)
  OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                      ymax = Estimate + Multiplier * StandardError, data = CoefficientTables, geom = "pointrange",
                      ylab = NULL, xlab = NULL)
  OutputPlot <- OutputPlot + scale_x_discrete(labels = rev(var.names))
  OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
  OutputPlot <- OutputPlot + facet_grid(~ ModelName) + coord_flip() + theme_bw()
  print(OutputPlot)
}