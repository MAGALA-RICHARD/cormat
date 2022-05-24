#' mat_corr
#' generate matrix table of significant variables
#'
#' This function explores the correlation of a variables with multiple dependent variables.
#' It takes on an array or a data frame. if a vector is passed to the function it will be converted into a data frame
#' Check if all the variables are numeric and not strings

#'
#' @param df data frame or a matrix with predictor variables
#' @param y is a response variable for exploring its relationship with the other predictor variables
#' @param sig.level specifies the significant level for deciding a significant relation between variables default is 0.05.
#' @param showSig returns names of the significant variables at the specifies significant level
#' @return A data frame of correlation, p-values and their t-values
#' @title Correlation exploration using vectors or data frames
#' @export
mat_corr <- function(y, df, showSig = F, sig.level = 0.05) {
  if(!is.data.frame(df)){
    d <- data.frame(df)
  }
  matt <- matrix(NA, nrow =ncol(d), ncol = 3, byrow= T)
  for ( i in 1:ncol(d)){
    test.result <- cor.test(y, df[, i])
    matt[i ,1] = round(test.result$p.value, 4)
    matt[i,2] <- round(test.result$estimate, 3)
    matt[i ,3] <- round(test.result$statistic, 3)
    rownames(matt) <- names(d)
    colnames(matt) <- c("P-value", "cor", "t-value")

  }

  if (showSig == F){
    return(matt)
  }else{
    p =  which(matt[,1] < sig.level)
    return(list(p))
  }
}
