# pass back a list for plotting that contains 3 components
#1 the truth annototaions
#2 the predictions that are true positives
#3 the predictions that are false positives
# reac_func_output produces a list of which element 2 houses the sf data and element 3 houses the IDs of the FP
plot_image_class<-function(dat1, dat2, dat3, conf.thresh, over1, over2, image, class){
  
  truth<-dat1[[as.numeric(image)]]
  
  class<-truth[[dat3$INDEX[dat3$CLASS==class]]] # class should contain 1 or 2 lists of truth and prediction (if present) for selected image and annotation class
  
  truth<-class[[1]]
  nclass<-length(class)
  if(length(nclass>1)){
    for(j in 2:nclass){
      fp<-class[[j]][class[[j]]$ID%in%dat2,]
      tp<-class[[j]][!(class[[j]]$ID%in%dat2),]
    }
  }
  
  res<-list(truth, fp, tp) 
  
  res
}