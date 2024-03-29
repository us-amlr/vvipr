#' Verify VIAME Predictions
#'
#' This package contains functions designed to assess predictions from trained classification models  that were developed in VIAME. 
#'
#' @name vvipr_package
#' @aliases vvipr_package
#' @docType package
#' @title Verify VIAME Predictions
#' @author Jefferson Hinke \email{jefferson.hinke@noaa.gov}
#' @seealso \url{https://github.com/us-amlr/vvipr}
#' 
#' @import shiny
#'
#' @importFrom dplyr %>% .data group_by summarise
#' @importFrom sf st_bbox st_intersects st_intersection st_area st_as_sf 
#'   st_convex_hull st_sf
#' @importFrom graphics box legend
#' @importFrom grDevices col2rgb rgb adjustcolor
#' @importFrom stats uniroot
#' @importFrom utils read.csv write.csv
#' @keywords package
NULL
