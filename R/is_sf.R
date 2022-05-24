#' is_sf
#' 
#' Helper function to identify whether an object is of class 'sf'
#'
#' @param x R object to be tested
#'
#' @return Logical. The function returns T/F  
is_sf<-function(x){
  cc<-class(x)
  sfclass<-c("sf", "data.frame")
  ret<-ifelse(identical(cc, sfclass), TRUE, FALSE)
  ret
}
