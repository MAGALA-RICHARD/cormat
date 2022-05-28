#' mult_regression
#' run multiple regresson on a data frame
#' @author richard magala
#' @export 
#' @return modelcountainer_summaries 

mult_regression <- function(y, df){
  modelcountainer_summaries <- list()
  for(i in 2:ncol(data)) {                 # Head of for-loop
    
    predictors_i <- colnames(df)[2:i]    # Create vector of predictor names
    modelcountainer_summaries [[i - 1]] <- summary(     # Store regression model summary in list
      lm(y ~ ., df))
    return( modelcountainer_summaries )
    
  }
}
  