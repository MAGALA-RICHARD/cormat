#' CumVec This function allow you to add a vector in a squence to get cumulative values
#' @param x a vector to increment
#' @return a vector of incremented values in a sequence relative to the paretn vector
#' @export
CumVec <- function(x){
  add_vec <- function(x) Reduce("+", x, accumulate =TRUE)
  cumx <- eval.parent(substitute(add_vec(x)))
  return(cumx)
}
