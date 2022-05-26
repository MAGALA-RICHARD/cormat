#' likert_scale_conversion
#' convert likert scale characters to numerical numbers
#' It takes on an array or a data frame. if a vector is passed to the function it will be converted into a data frame
#' Check if all the variables are numeric and not strings

#' @param df data frame or a matrix with likert scale rating strongly agree, agree, etc
#' @title Correlation exploration using vectors or data frames
#' @export
#' @author Richard Magala, 2022
#' @return  a matrix of plots
likert_scale_conversion <- function(df){
  if(is.data.frame(df) == FALSE){
    dat <- data.frame(df)
  }else{
    dat  = df
  }
  pd <- dat
  nc <- ncol(dat)
  for ( i in 1:nc){
    pd[, i] <- ifelse(dat[, i] == "Agree", 4, ifelse(dat[, i] == "Strongly agree", 5, ifelse(dat[, i] == "Disagree", 2, ifelse(dat[, i ] =="Strongly disagree", 1,3))))
  }
  return(pd)
}
  