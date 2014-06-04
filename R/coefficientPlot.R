#' A sltools coefficient plotting function
#' 
#' This function allows you to extract coefficients from a list of models and create side-by-side coefplots.
#' @param models a list of models
#' @param alpha scalar of alpha
#' @param model.names list of model names
#' @param var.order vector of variable ordering
#' @param var.names list of variable names
#' @param omit.intercept whether to omit intercept, default = TRUE
#' @param omit.factor whether to omit factors, default = TRUE
#' @export

coefficientPlot <- function(models, alpha = 0.05, model.names = "", var.order = "", var.names = NULL, omit.intercept = TRUE, omit.factor = TRUE){
  # models must be a list()
  Multiplier <- qnorm(1 - alpha / 2)
  CoefficientTables <- lapply(models, function(x) coef(x))
  TableRows <- unlist(lapply(CoefficientTables, nrow))
  
  if(model.names[1] == ""){
    ModelNameLabels <- rep(paste("Model", 1:length(TableRows)), TableRows)
  } else {
    ModelNameLabels <- rep(model.names, TableRows)
  }
  
  MatrixofModels <- cbind(do.call(rbind, CoefficientTables), ModelNameLabels)
  MatrixofModels <- data.frame(cbind(rownames(MatrixofModels), MatrixofModels))
  colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "TValue", "PValue", "ModelName")
  MatrixofModels[, -c(1, 6)] <- apply(MatrixofModels[, -c(1, 6)], 2, function(x){as.numeric(as.character(x))})
  
  # exclude rows of intercepts
  if(omit.intercept == TRUE){
    MatrixofModels <- MatrixofModels[!grepl("Intercept", MatrixofModels$IV),]
  } else {
    MatrixofModels <- MatrixofModels
  }
  
  # exclude rows of intercepts and factor
  if(omit.factor == TRUE){
    MatrixofModels <- MatrixofModels[!grepl("factor", MatrixofModels$IV),]
  } else {
    MatrixofModels <- MatrixofModels
  }
  
  # re-order the levels of variables in the order of appearance in the data.frame
  if(var.order[1] == ""){
    MatrixofModels$IV <- factor(MatrixofModels$IV, levels = rev(unique(as.character(MatrixofModels$IV))))
  } else {
    MatrixofModels$IV <- factor(MatrixofModels$IV, levels = rev(var.order))
  }
  
  # re-order the levels of model names based on user input
  MatrixofModels$ModelName <- factor(MatrixofModels$ModelName, levels = model.names)
  library(ggplot2)
  OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                      ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                      ylab = NULL, xlab = NULL)
  OutputPlot <- OutputPlot + scale_x_discrete(labels = rev(var.names.fig))
  OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
  OutputPlot <- OutputPlot + facet_grid(~ ModelName) + coord_flip() + theme_bw()
  return(OutputPlot)
}