assess_overlap_shiny<-function(truth="penguin_truth.csv", prediction="penguin_detections.csv", conf.thresh=0, over1=0.5, over2=0.5, PLOT=FALSE){
library(sf)
library(sfheaders)
# read in a csv that combines the truth and prediction coordinates with unique detection IDs for each
#truth<-read.csv(f1, skip=2, header=FALSE)
#prediction<-read.csv(f2, skip=2, header=FALSE)
#names(truth)<-c("DETECTION_ID","PIC_NAME", "IMAGE", "TLX", "TLY", "BRX", "BRY","CONF", "TARGET", "CLASS", "CONF_2")
#names(prediction)<-c("DETECTION_ID","PIC_NAME", "IMAGE", "TLX", "TLY", "BRX", "BRY","CONF", "TARGET", "CLASS", "CONF_2")
t.classes<-unique(truth$CLASS)
n.classes<-length(t.classes)
p.classes<-unique(prediction$CLASS)
#
# the prediction DETECTION_IDs need to be different than the truth ID
#prediction$DETECTION_ID<-seq(from=max(truth$DETECTION_ID)+1000, by=1, length.out=length(prediction$IMAGE))
# ensure classes for truth and predictions are identical
if(!identical(t.classes, p.classes)){
    stop("Truth and Annotation class names do not match. This code requires matching names for comparing.")
}

# create numeric code to allow matching of classes
index<-data.frame(CLASS=t.classes, INDEX=1:n.classes)
# change CLASS to clearly differentiate truth and prediction classes
truth<-merge(truth, index, by="CLASS")
prediction<-merge(prediction, index, by="CLASS")
truth$CLASS<-paste("truth_", truth$CLASS, sep="")
prediction$CLASS<-paste("prediction_", prediction$CLASS, sep="")
#
annos<-rbind(truth, prediction)
#write.csv(annos, "annos.csv")
# we'll conduct this analysis of overlap for each image separately
n.images<-length(unique(annos$PIC_NAME))
images<-unique(annos$PIC_NAME)
#
# cull predictions that are below a confidence threshold
#
#conf.thresh<-0
annos<-annos[annos$CONF>conf.thresh,]

#
# analyze overlap
#
out_image<-list() 
fp<-list()
for(i in 1:n.images){
  tt.test<-annos[annos$PIC_NAME==images[i],]
  # now run analyses for each class of object detected
  class_out<-list()
  false_positives<-numeric()
  for(k in 1:n.classes){
    test<-tt.test[tt.test$INDEX==k,]
    # 
    # if a particular class is not present in the image, skip and report NA for result
    if(dim(test)[1]==0){
      #print("here - fail 1")
      # if the class is utterly absent
      class_out[[k]]<-NA
      false_positives[k]<-0
      if(PLOT){
      XLAB<-paste("No objects of class ", p.classes[k], " present", sep="")
      windows()
      plot(0,0, main=images[i], type="n", xlab=XLAB)
      }
    } else {
      k_classes<-length(unique(test$CLASS))
      #print(unique(test$CLASS))
      #print(c(k_classes, n.classes))
      if(k_classes!=2){
        #print("here - fail 2")
        # filtering likely caused a predicted class to no longer be present. Thus, no predictions available to assess
        # again, set 
        class_out[[k]]<-NA
        false_positives[k]<-0
        if(PLOT){
        XLAB<-paste("No objects of class ", p.classes[k], " present", sep="")
        windows()
        plot(0,0, main=images[i], type="n", xlab=XLAB)
        }
      } else {
        #print("here - normal")
        # build polygons from coordinates indicating bottom left x and y and top right x and y
        #first thing is to make a long-format data frame with 5 polygon vertices for X and Y built from teh corners of the bounding box
        # X pattern is TLX BRX BRX TLX TLX
        # Y pattern is TLY  TLY BRY BRY TLY
        n.polys<-length(test[,1])
        out<-list()
        class<-list()
        for(j in 1:n.polys){
          dat<-test[j,] # select ith row of data
          out[[j]]<-data.frame(ID=dat$DETECTION_ID,
                             X=c(dat$TLX,dat$BRX, dat$BRX, dat$TLX, dat$TLX),
                             Y=c(dat$TLY, dat$TLY, dat$BRY, dat$BRY, dat$TLY))
          class[[j]]<-dat$CLASS
        }
        DTlong<-do.call("rbind", out)
        class<-do.call("rbind", class)
        # use sfheaders::sf_polygon to create the polygons for overlaying later
        x<-sfheaders::sf_polygon(obj=DTlong, x="X", y="Y", polygon_id ="ID")
        x$CLASS<-class
        # separate the different classes here
        nclass<-length(unique(x$CLASS))
        class.names<-unique(x$CLASS)
        classes<-list()
        for(j in 1:nclass){
          classes[[j]]<-x[x$CLASS==class.names[j],]
        }
        # now assess overlap of the classes
        # need to know which classes should overlap (i.e., penguin annotation with penguin prediction, but not with seal prediction!)
        # use st_intersects(obj1, obj2, sparse=FALSE) for basic analysis of clearly unique polygons
        x<-st_intersects(classes[[2]], classes[[1]], sparse=FALSE)
        xx<-apply(x, 1, any)
        # length will give the number of false positives, assuming predictions are the first geometry and truth is in the second
        # this only account for the polygons that exhibit no overlap. 
        false_positives[k]<-length(xx[xx==FALSE])
        #
        # but the issue isn't overlap, it's the quality of overlap. need to know if it overlaps by a decent margin. 
        #
        # the following creates polygons of the overlapping areas
        # to assess false negatives, place the "predictions" in the first location and "truth" annotations in the second location
        #
        tt.int<-st_intersection(classes[[2]], classes[[1]])
        # compute area for each overlapping polygon identified above
        tt.area<-st_area(tt.int)
        tt.int$OVERLAP_AREA<-tt.area
      
        # what is area for each class being compared = we'll use the areas to assess extent of overlap to help assess false negative
        tt.c1.area<-sf::st_area(classes[[1]]) # this should be the annotation class
        tt.c2.area<-sf::st_area(classes[[2]]) # this should be the prediction class
        #merge these areas with the intersection dataframe
        classes[[1]]$AREA<-tt.c1.area
        classes[[2]]$AREA<-tt.c2.area
        #
        tt.c1.merge<-data.frame(ID.1=classes[[1]]$ID, ANNO_AREA=classes[[1]]$AREA)
        tt.c2.merge<-data.frame(ID=classes[[2]]$ID, PRED_AREA=classes[[2]]$AREA)
        #
        tt.int<-merge(tt.int, tt.c2.merge, by="ID", sort=FALSE)
        tt.int<-merge(tt.int, tt.c1.merge, by="ID.1", sort=FALSE)
        #
        # Save out the dataframe for later analysis
        # delete any "overlaps" with an overlap area of zero - these are "lines" and not useful for assessing
        tt.int<-tt.int[tt.int$OVERLAP_AREA>0,]
        # assess percetn of overlap that the prediction and annotation have
        # we expect a good prediction to mostly overlap the annotated area. if the overlap is poor - reject as true prediction and become False negative
        tt.int$PROP_OVERLAP<-tt.int$OVERLAP_AREA/tt.int$ANNO_AREA
        # also assess how much of prediction is in the overlap aera. 
        # if a high proportion, it means the prediction is mainly centered over the annotation, but smaller
        # so, an assessment of whether to retain a prediction requires either high overlap with the annotaiton,
        # or for the prediction to be mostly represented in the overlap area
        tt.int$PROP_PRED_OVERLAP<-tt.int$OVERLAP_AREA/tt.int$PRED_AREA
        tt.int<-tt.int[order(tt.int$ID),]
        fn<-paste("intersections_f",i,"group",k, ".csv", sep="")
        #write.csv(tt.int, fn)
        class_out[[k]]<-tt.int
        # 
        # the output is a data frame that identifies the polygon ids from the truth and prediction that overlap
        # if prediction overlaps iwth more than one, the overlap is indicated by addition of 0.1 
        # example, if prediction 10 overlaps with truth 46 and 47, each overlap is presented as 10 overlaps with 46, 10.1 overlaps with 47, etc. 
        # in some cases overlap is represented by a line, if so, then geometry indicates repeated x or y coordinates, depending on orientation - those could be deleted)
        # for plotting set appropriate xlim and ylim values from bounding box of preds and truth polygons
        XLIM<-range(DTlong$X)
        YLIM=range(DTlong$Y)
        #auxiliary function for color
        #
        ## Transparent colors
        ## Mark Gardener 2015
        ## www.dataanalytics.org.uk
        
        t_col <- function(color, percent = 60, name = NULL) {
          #      color = color name
          #    percent = % transparency
          #       name = an optional name for the color
          
          ## Get RGB values for named color
          rgb.val <- col2rgb(color)
          
          ## Make new color using input color as base and alpha set by transparency
          t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                       max = 255,
                       alpha = (100 - percent) * 255 / 100,
                       names = name)
          
          ## Save the color
          invisible(t.col)
        }
        # view the polygons
        N=ifelse(nclass<3, 3, nclass) # catch since brewer pal requires at least 3 
        #for now, set 2 colors for three classes within truth/prediction annotations
        pick.colors=c("red", "blue")
        #cols<-brewer.pal(n=N, name="Set1")
        if(PLOT){
          windows()
          t.col<-t_col(color=pick.colors[1])
          MAIN<-paste(t.classes[k], "_", images[i], sep="")
          plot(classes[[1]]$geometry, xlim=XLIM, ylim=YLIM, border=1, col=t.col, main=MAIN)
          if(length(nclass>1)){
            for(j in 2:nclass){
              #mycol <- rgb(0, 0, 255, max = 255, alpha = 125) # create a transparent blue for the truth
              t.col<-t_col(color=pick.colors[j])
              plot(classes[[j]]$geometry, add=TRUE, border=1, col=t.col)
            }
          }
        }
      }
    }
  }
  out_image[[i]]<-class_out
  fp[[i]]<-false_positives
}

fp<-do.call("rbind", fp)
fp<-data.frame(fp)
names(fp)<-t.classes
# now analyze the outputs of the overlap to add false positives based on poor overlaps
# will apply a function for each image and class
new_fp<-list()
for(i in 1:n.images){
  #print(images[i])
  dat<-out_image[[i]]
  fps<-numeric()# should house n values (1 for each class annotated) 
  for(j in 1:n.classes){
    #print(j)
    t.dat<-dat[[j]]
    t.dat.dim<-dim(t.dat)[1]
    t.dat.dim<-ifelse(is.null(t.dat.dim),0,t.dat.dim)
    if(!is_sf(t.dat)|t.dat.dim==0){
      # is_sf is a custom function to ensure only sf object (data with polygons) is passed along. 
      # if a particular class has no observations, assign 0 false positives to it's space
      fps[j]<-0
    } else {      # find truth annotations with more than one prediction overlapping it
      n_occur<-data.frame(table(t.dat$ID.1))
      n_occur<-n_occur[n_occur$Freq>1,]
      names(n_occur)<-c("ID.1", "MULTI_TRUTH")
      t.dat<-merge(t.dat, n_occur, by="ID.1", all=TRUE, sort=TRUE)
   
      # find predictions that overlap more than one truth
      n_occur<-data.frame(table(t.dat$ID))
      n_occur<-n_occur[n_occur$Freq>1,]
      names(n_occur)<-c("ID", "MULTI_PRED")
      t.dat<-merge(t.dat, n_occur, by="ID", all=TRUE, sort=TRUE)
 
      # the above indices identify 
      t.dat$MULTI_TRUTH<-ifelse(is.na(t.dat$MULTI_TRUTH), 0, t.dat$MULTI_TRUTH)
      t.dat$MULTI_PRED<-ifelse(is.na(t.dat$MULTI_PRED), 0, t.dat$MULTI_PRED)
      t.dat$REGULAR<-t.dat$MULTI_TRUTH+t.dat$MULTI_PRED
      t.dat$REGULAR<-ifelse(t.dat$REGULAR>0, 0,1)

      # use REGULAR to assess simple FP rate
      fn<-paste("intersections_f",i,"group",j, ".csv", sep="")
      #write.csv(t.dat, fn)
      
       #over1<-0.48 # for 1:1 overlaps (1 annotation and 1 prediction), overlap should be at least this high  --> a FP results
       #over2<-0.48 #
      
      fp_scenario<-numeric()
      tt.dat<-t.dat[t.dat$REGULAR==1,]
      # check if any data exist
      if(dim(tt.dat)[1]==0){
        # assign no fp for scenario 1 if no data exist
        fp_scenario[1]<-0
      } else {
        # now count any  predictions with overlap less than 1:1 overlap ratio AND less than the over2 ratio
        s1<-ifelse(tt.dat$PROP_OVERLAP<over1, 1, 0)
        s2<-ifelse(tt.dat$PROP_PRED_OVERLAP<over2, 1,0)
        s3<-s1+s2
        fp_scenario[1]<-length(s3[s3>1])
      }
      # now add FP for scenario 2 - where one prediction overlaps n annotations. 
      # assume only annotation that overlaps the most is worth counting, then assess it's overlap as above
      tt.dat<-t.dat[t.dat$REGULAR==0,]
      tt.dat<-tt.dat[tt.dat$MULTI_TRUTH>1,]
      # check if any data exist
      if(dim(tt.dat)[1]==0){
        # assign no fp for scenario 1 if no data exist
        fp_scenario[2]<-0
      } else {
        tt.dat<-tt.dat[order(tt.dat$ID.1),]
        # now assess which of the overlaps to keep for assessment as FP and which to discard are incidental overlap
        n.overlaps<-length(unique(tt.dat$ID.1))
        overlaps<-unique(tt.dat$ID.1)
        #print(n.overlaps)
        fpk<-0
        fpcount<-0
        for(k in 1:n.overlaps){
          ttt.dat<-tt.dat[tt.dat$ID.1==overlaps[k],]
          # identify row containing
          ttt.dat<-ttt.dat[order(ttt.dat$PROP_OVERLAP),]
          # ordered data will have highest overlap last
          n.overs<-length(ttt.dat$ID.1)
          ttt.dat$KEEP<-c(rep(0, n.overs-1), 1)
          # check to see if any of the low overlap predictions have 0 other overlaps. if so, then it's a FP
          fp.dat<-ttt.dat[ttt.dat$KEEP==0,]
          # if fp.dat$MULTI_PRED==0, add one FP to the tally
          tt.fp<-length(fp.dat$MULTI_PRED[fp.dat$MULTI_PRED==0])
          fpcount<-fpcount+tt.fp
          # now assess the "better" predictions to see if they meet FP criteria
          ttt.dat<-ttt.dat[ttt.dat$KEEP==1,]
          # now assess remaining overlap
          s1<-ifelse(ttt.dat$PROP_OVERLAP<over1, 1, 0)
          s2<-ifelse(ttt.dat$PROP_PRED_OVERLAP<over2, 1,0)
          s3<-s1+s2
          fpk<-fpk+length(s3[s3>1])
        }
        # add up the false positives counted here
        fp_scenario[2]<-fpk+fpcount
      }
      # now the final       
      tt.dat<-t.dat[t.dat$REGULAR==0,]
      tt.dat<-tt.dat[tt.dat$MULTI_PRED>1,]
      # check if any data exist
      if(dim(tt.dat)[1]==0){
        # assign no fp for scenario 1 if no data exist
        fp_scenario[2]<-0
      } else {
        tt.dat<-tt.dat[order(tt.dat$ID),]
        # now assess which of the overlaps to keep for assessment as FP and which to discard are incidental overlap
        n.overlaps<-length(unique(tt.dat$ID))
        overlaps<-unique(tt.dat$ID)
        fpk<-0
        for(k in 1:n.overlaps){
          ttt.dat<-tt.dat[tt.dat$ID==overlaps[k],]
          # identify row containing
          ttt.dat<-ttt.dat[order(ttt.dat$PROP_OVERLAP),]
          # ordered data will have highest overlap last
          n.overs<-length(ttt.dat$ID.1)
          ttt.dat$KEEP<-c(rep(0, n.overs-1), 1)
          ttt.dat<-ttt.dat[ttt.dat$KEEP==1,]
          # now assess the "better" predictions to see if they meet FP criteria
          # now assess remaining overlap
          s1<-ifelse(ttt.dat$PROP_OVERLAP<over1, 1, 0)
          s2<-ifelse(ttt.dat$PROP_PRED_OVERLAP<over2, 1,0)
          s3<-s1+s2
          fpk<-fpk+length(s3[s3>1])
        }
        fp_scenario[3]<-fpk
        #print(fp_scenario)
      }
    }
    fps[j]<-sum(fp_scenario)
  }
  new_fp[[i]]<-fps
}
new_fp<-as.vector(unlist(new_fp))
new_fp<-matrix(new_fp, ncol=n.classes, byrow=TRUE)
total<-fp+new_fp
total$fp<-rowSums(total)
total$anno<-as.vector(tapply(truth$BRX, truth$IMAGE, length))
# because prediction numbers can change as conf.thresh changes, need to use the annos data set to determine total number of prediction
# but beware! filtering can also cause data from certain images to reduce to 0. need to find a way to identify gaps and fill with 0
p1<-prediction[prediction$CONF>conf.thresh,]
# check to see if all images represented
n.p1<-length(unique(p1$PIC_NAME))
if(n.p1!=n.images){
  # well, we're in the shit here. gotta flush out the missing images and add a 0 in the right place
  new.df<-data.frame(IMAGE=images, N=0)
  p1.images<-unique(p1$PIC_NAME)
  p1.n<-as.vector(tapply(p1$BRX, p1$IMAGE, length))
  p1.df<-data.frame(IMAGE=p1.images, NewN=p1.n)
  p1.df.new<-merge(p1.df, new.df, by="IMAGE", all.y=TRUE, sort=FALSE)
  p1.df.new<-p1.df.new$NewN
  p1.df.new<-ifelse(is.na(p1.df.new), 0, p1.df.new)
  total$preds<-p1.df.new
} else {
  # all cool here
total$preds<-as.vector(tapply(p1$BRX, p1$IMAGE, length))
}
# at this point, you could calculate performace on each image. 
# but in reality, probalby just need performance of model generally
# so sum columns for total fp, anno, and preds to solve for fn and tp and then calculate model scores
FP<-sum(total$fp)
ANNO<-sum(total$anno)
PREDS<-sum(total$preds)
# now solve uniroot function
#print(c(FP, ANNO, PREDS))

fun<-uniroot(fill_matrix, interval=c(0,1000), fp=FP, anno=ANNO, preds=PREDS)
fvals<-vals(x=fun$root, FP, ANNO)
# calcualte the model performance metrics based on fp, fn, and tp counts
fp<-fvals[1]
fn<-fvals[2]
tp<-fvals[3]
accur<-tp/(tp+fp+fn)
prec<-tp/(tp+fp)
recall<-tp/(tp+fn)
f1<-tp/(tp+0.5*(fp+fn))
res<-data.frame(CONF=conf.thresh, OVER1=over1, OVER2=over2, FP=fp, FN=fn, TP=tp, ANNO=ANNO, PREDS=PREDS, ACCURACY=accur, PRECISION=prec, RECALL=recall,F1=f1)
return(res)
}
