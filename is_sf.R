is_sf<-function(x){
  cc<-class(x)
  sfclass<-c("sf", "data.frame")
  ret<-ifelse(identical(cc, sfclass), TRUE, FALSE)
  ret
}
