#' vvipr
#'
#' Runs the R Shiny App
#' 
#' The Verify VIAME Prediction (vvipr) code was developed as a Shiny app to quantify the performance of an image classification model built with VIAME software. The app compares the output from the model with corresponding user-defined "truth" annotations of the same set of images. At present, the code supports object detection within those image frames for any number of user-defined object classes.
#' 
#' The code takes two files as input. One is the set of truth annotations, the other is the set of predictions from a trained model applied to the same images for which the truth annotations exist. The user can select appropriate cut-offs for retention of model predictions based on the confidence threshold assigned to predictions by the model. The user can also adjust the amount of overlap between truth and prediction bounding boxes that is required to classify detections as true positives (TP), false positives (FP), or false negatives (FN). The code will compute 4 model performance metrics: accuracy, precision, recall, and the F1 score.
#' 
#' Once the app is open, select appropriate input .csv files, set appropriate thresholds, and behold the overlap. 
#' @return The app returns a data frame for download as a .csv file, if desired. The data frame combines the three tables displayed by the app and include the input values used, the counts of annotations, predictions, true and false positives, and the performance scores. These data reflect the performance of the model as a whole. 
#' @usage vvipr()
#' 
#' @export
vvipr<-function() {
  appDir <- system.file("shiny", package = "vvipr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `photoR`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
