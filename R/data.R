#' truth_ex.rda
#' 
#' An example truth data set formatted as a data.frame for use the 'assess_overlap_shiny' function. These data represent the truth annotations for a single image and a single class (penguin).
#'The data.frame faithfully represents the original VIAME output file and has headers defined follows:
#' 
#'
#' \itemize{
#' \item DETECTION_ID. Unique identifier for each annotaion
#' \item PIC_NAME. The file name of the image used for annotation
#' \item IMAGE. A numeric index identifying the image
#' \item TLX. Top left X coordinate of bounding box
#' \item TLY. Top left Y coordinate of bounding box
#' \item BRX. Bottom right X coordinate of bounding box
#' \item BRY. Bottom right Y coordinate of bounding box
#' \item CONF. Confidence level of truth annotation. This is 1 by default, since it represents a true observation.
#' \item TARGET. ????
#' \item CLASS. The class identifier for the object to be detected.
#' \item CONF_2. A repeat of the confidence level of the annotation.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name truth_ex
#' @format A data frame with 11 columns and 74 rows
"truth_ex"
#' 
#'  prediction_ex.rda
#' 
#' An example prediction data set formatted as a data.frame for use the 'assess_overlap_shiny' function. These data represent the model predictions for a single image and a single class (penguin).
#'The data.frame faithfully represents the original VIAME output file and has headers defined follows:
#' 
#'
#' \itemize{
#' \item DETECTION_ID. Unique identifier for each annotation
#' \item PIC_NAME. The file name of the image used for annotation
#' \item IMAGE. A numeric index identifying the image
#' \item TLX. Top left X coordinate of bounding box
#' \item TLY. Top left Y coordinate of bounding box
#' \item BRX. Bottom right X coordinate of bounding box
#' \item BRY. Bottom right Y coordinate of bounding box
#' \item CONF. Confidence level of truth annotation. This is 1 by default, since it represents a true observation.
#' \item TARGET. ????
#' \item CLASS. The class identifier for the object to be detected.
#' \item CONF_2. A repeat of the confidence level of the annotation.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name prediction_ex
#' @format A data frame with 11 columns and 75 rows
"prediction_ex"