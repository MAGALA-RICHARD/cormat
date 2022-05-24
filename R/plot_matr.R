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
  if(!is.data.frame(df)){
    d <- data.frame(df)
  }
  pvalues <- rep(NA, ncol(df))
  for ( i in 1:ncol(df)){
    test.result <- cor.test(y, df[, i])
    pvalues[i] = round(test.result$p.value, 4)
  }
  par(mfrow = c(nrow, ncol))
  # plot significant results
  sig.variable <- which(pvalues < sig.level)
  df <- df[, sig.variable]
  if (length(sig.variable)> 0) {
    for ( i in 1:ncol(df)){
     a <- names(df)
      plot(df[, i], y, col = "blue", pch = 2, xlab = colnames(df)[i], ylab = paste0(ylab , " is the selected", collpase = " "))
      text(mean(df[,i]), median(y), label ="Fitted line")
      data.temp  <- data.frame("response" =y, "x"= df[,i])
      abline(coef(lm(response ~ x, data= data.temp)), col = "green4")
      a
    }
  } else {
    print("no significant predictor was found try again with another variable: use 'mat.cor' **function** to check for the p values")

  }

}
