#' optimize_vals
#' 
#' Simple function 
#'
#' @param x Integer. The solution to the minimization of the 'optimize_me' function. 
#' @param fp Integer. The total number of false positives.
#' @param anno Integer. The total number of truth annotations. 
#'
#' @return Numeric vector containing the number of false positives, false negatives, and true positives 
optimize_vals<-function(x, fp, anno){
  fp<-fp
  fn<-x
  tp<-anno-fn
  out<-c(fp, fn, tp)
  out
}