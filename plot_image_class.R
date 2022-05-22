# pass back a list for plotting that contains 3 components
#1 the truth annotations
#2 the predictions that are true positives
#3 the predictions that are false positives
# reac_func_output produces a list of which element 2 houses the sf data and element 3 houses the IDs of the FP
plot_image_class<-function(dat1, dat2, dat3, conf.thresh, over1, over2, image, class){
  
  
  truth<-dat1[[as.numeric(image)]]
  
  # check if all classes present for plotting
  
  dat3$KEEP<-dat3[,1]%in%names(truth)
  
  # this line will fail if target class is not present
  if(dat3$KEEP[dat3$CLASS==class]==TRUE){
    class<-truth[[dat3$INDEX[dat3$CLASS==class]]] # class should contain 1 or 2 lists of truth and prediction (if present) for selected image and annotation class
    
    truth<-class[[1]]
    nclass<-length(class)
    res<-list()
    if(length(nclass>1)){
      for(j in 2:nclass){
        fp<-class[[j]][class[[j]]$ID%in%dat2,]
        tp<-class[[j]][!(class[[j]]$ID%in%dat2),]
      }
    }
    
    # find  appropriate bbox for plotting later
    x<-rbind(truth, fp, tp)
    plot_bb<-st_bbox(x)
    
    # pass out data for app
    res[[1]]<-list(truth, fp, tp) 
    res[[2]]<-plot_bb
    
  } else {
    # return negative result to trigger error message
    res<-NULL
  }
  # add bits to spit out the st_bbox for proper plotting of each image
  
  res
}