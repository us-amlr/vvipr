plot_image_class<-function(truth, prediction, conf.thresh=0, over1=0.5, over2=0.5, image=0, class="penguin"){
  library(dplyr)
  library(sf)
  library(sfheaders)
  library(RColorBrewer)
  # read in a csv that combines the truth and prediction coordinates with unique detection IDs for each
  #truth<-read.csv(truth, skip=2, header=FALSE)
  #prediction<-read.csv(prediction, skip=2, header=FALSE)
  #truth<-read.csv("penguin_truth.csv", skip=2, header=FALSE)
  #prediction<-read.csv("penguin_detections.csv", skip=2, header=FALSE)
  #truth<-read.csv("penguin_truth.csv", skip=2, header=FALSE)

  #names(truth)<-c("DETECTION_ID","PIC_NAME", "IMAGE", "TLX", "TLY", "BRX", "BRY","CONF", "TARGET", "CLASS", "CONF_2")
  #names(prediction)<-c("DETECTION_ID","PIC_NAME", "IMAGE", "TLX", "TLY", "BRX", "BRY","CONF", "TARGET", "CLASS", "CONF_2")

  t.classes<-unique(truth$CLASS)
  n.classes<-length(t.classes)
  p.classes<-unique(prediction$CLASS)
  #
  # the prediction DETECTION_IDs need to be different than the truth ID
  #prediction$DETECTION_ID<-seq(from=max(truth$DETECTION_ID)+1000, by=1, length.out=length(prediction$IMAGE))
  # ensure classes for truth and predictions are identical
  #if(!identical(t.classes, p.classes)){
  #  warning("Truth and Annotation class names do not match. This code requires matching names for comparing.")
  #}
  
  # create numeric code to allow matching of classes
  index<-data.frame(CLASS=t.classes, INDEX=1:n.classes)
  # change CLASS to clearly differentiate truth and prediction classes
  truth<-merge(truth, index, by="CLASS")
  prediction<-merge(prediction, index, by="CLASS")
  truth$ORIG_CLASS<-truth$CLASS
  prediction$ORIG_CLASS<-prediction$CLASS
  truth$CLASS<-paste("truth_", truth$CLASS, sep="")
  prediction$CLASS<-paste("prediction_", prediction$CLASS, sep="")
  
  #
  annos<-rbind(truth, prediction)
  print(unique(annos$ORIG_CLASS))
  #write.csv(annos, "annos.csv")
  # we'll conduct this analysis of overlap for each image separately
  #
  # cull predictions that are below a confidence threshold
  #
  #conf.thresh<-0
  #image=
  #class="penguin"
  test<-annos[annos$CONF>conf.thresh,]
  test<-test[test$IMAGE==image,]
  test<-test[test$ORIG_CLASS==class,]
  
  sf_out<-list()
  n_class<-length(unique(test$CLASS))
  classes<-unique(test$CLASS)
  for(i in 1:n_class){
    dat<-test[test$CLASS==classes[i],]# assuming just looking at annotation and prediction to overlap here
    if(dim(dat)[1]==0){
      # if the class is utterly absent in this image, pass NULL out
      out[[1]]<-NULL
    } else {
      n.polys<-length(dat[,1])
      out<-list()
      tt.class<-list()
      for(j in 1:n.polys){
        tt.dat<-dat[j,] # select ith row of data
        out[[j]]<-data.frame(ID=tt.dat$DETECTION_ID,
                             X=c(tt.dat$TLX,tt.dat$BRX, tt.dat$BRX, tt.dat$TLX, tt.dat$TLX),
                             Y=c(tt.dat$TLY, tt.dat$TLY, tt.dat$BRY, tt.dat$BRY, tt.dat$TLY))
        tt.class[[j]]<-tt.dat$CLASS
      }
      DTlong<-do.call("rbind", out)
      class<-do.call("rbind", tt.class)
      #XLIM<-range(DTlong$X)
      #YLIM<-range(DTlong$Y)
      # use sfheaders::sf_polygon to create the polygons for overlaying later
      x<-sfheaders::sf_polygon(obj=DTlong, x="X", y="Y", polygon_id ="ID")
      x$CLASS<-class
      sf_out[[i]]<-x
    }
}
sf_out  
}    
