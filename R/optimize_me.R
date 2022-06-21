#' optimize_me
#'
#' Internal helper function defining the function to be minimized to solve for the number of true positives and false negatives given known values of false postives, total annotations, and total predictions.  
#' @param x The parameter to be solved for 
#' @param fp Integer. The total number of false positives.
#' @param anno Integer. The total number of truth annotations.
#' @param preds Integer. The total number of predictions.
#'
#' @return Numeric. Returns value of function to be minimized when solving for x
#' 
#' @export
optimize_me<-function(x, fp,anno, preds){
  tp<-anno-x
  pred<-tp+fp
  pred-preds
}