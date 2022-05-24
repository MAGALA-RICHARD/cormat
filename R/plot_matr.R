#' plot_matr
#' generate plots and fitted lines of significantly correlated variables for exploration.
#'
#' This function allows you to visualize the correlated variables with the selected variable. the fitted line is through regression

#' It takes on an array or a data frame. if a vector is passed to the function it will be converted into a data frame
#' Check if all the variables are numeric and not strings

#' @param df data frame or a matrix with predictor variables
#' @param y is a response variable for exploring its relationship with the other predictor variables
#' @param nrow specifies the number of plots to be inserted in a row
#' @param ncol specifies the number of plots to be inserted in a column
#' @param sig.level specifies the number significant level default is 0.05
#' @param ylab specifies the y-axis, which is the name of the respons variables defaults is the y names
#' @title Correlation exploration using vectors or data frames
#' @export
#' @author Richard Magala, 2022
#' @return  a matrix of plots
plot_matr <- function(y, df, nrow =2, ncol= 2, sig.level = 0.05, ylab = ""){
  if(is.data.frame(df) == FALSE){
    d <- data.frame(df)
  }else{
    d = df 
  }
  pvalues <- rep(NA, ncol(d))
  for ( i in 1:ncol(d)){
    test.result <- cor.test(y, d[, i])
    pvalues[i] = round(test.result$p.value, 4)
  }
  par(mfrow = c(nrow, ncol))
  # plot significant results
  sig.variable <- which(pvalues < sig.level)
  dff <- d[, sig.variable]
  if (length(sig.variable)> 0) {
    for ( i in 1:ncol(dff)){
     a <- colnames(dff)
      plot(dff[, i], y, col = "blue", pch = 15, xlab = colnames(dff)[i], ylab = ylab)
      text(mean(dff[,i]), median(y), label ="Fitted line")
      data.temp  <- data.frame("response" =y, "x"= dff[,i])
      abline(coef(lm(response ~ x, data= data.temp)), col = "red")
      a
    }
  } else {
    print("no significant predictor was found try again with another variable: use 'mat.cor' **function** to check for the p values")

  }

}
