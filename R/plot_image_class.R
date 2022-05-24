#' plot_image_class
#' 
#' Helper function to corral data for plotting in the app.  
#'
#' @param dat1 List. The list of sf files that contain annotation polygons for the selected image and class
#' @param dat2 Vector of polygon IDs for all false positive predictions.
#' @param dat3 Data frame listing object classes ('CLASS') and arbitrary numeric code ('INDEX') 
#' @param conf.thresh A numeric value from 0 to 0.99 that specifies the minimum value for including model predictions in the analysis. Predictions with confidence levels below the threshold will be eliminated from the analysis. 
#' @param over1 A numeric value from 0 to 1 that specifies the minimum proportion of a truth annotation that must be covered by a model prediction. Predictions that do not meet this threshold will be assigned a status of 'false positive'  
#' @param over2 A numeric value from 0 to 1 that specifies the minimum proportion of a prediction area that must overlap with a truth annotation. Predictions that do not meet this threshold will be assigned a status of 'false positive'. Note that predictions that fail to meet the 'over1' threshold, but do meet the 'over2
#' @param image Integer. The numeric value for the selected image to plot.
#' @param class Character. The name of the object class to plot.
#'
#' @return List of length 2. The first component is a list containing a list of length 3 with the geometries for truth annotations, false positive, and true positive predictions. THe second component is a bounding box for proper plot X and Y limits. 
#' @export
#' 
plot_image_class<-function(dat1, dat2, dat3, conf.thresh, over1, over2, image, class){
  # pass back a list for plotting that contains 3 components
  #1 the truth annotations
  #2 the predictions that are true positives
  #3 the predictions that are false positives
  # reac_func_output produces a list of which element 2 houses the sf data and element 3 houses the IDs of the FP
  
  
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