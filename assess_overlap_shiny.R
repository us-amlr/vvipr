assess_overlap_shiny<-function(truth="penguin_truth.csv", prediction="penguin_detections.csv", 
                               conf.thresh=0, over1=0.5, over2=0.5, PLOT=FALSE){
  library(sf)
  library(sfheaders)
  
  t.classes<-unique(truth$CLASS)
  n.classes<-length(t.classes)
  p.classes<-unique(prediction$CLASS)
  
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
  
  # cull predictions that are below a confidence threshold
  
  annos<-annos[annos$CONF>conf.thresh,]

  
  # create output placeholders
  
  out_image<-list() 
  fp<-list()
  FP_IDS<-numeric() # keep track of false positive IDs
  TP_IDS<-numeric() # keep track of true positive IDs
  out_classes<-list()
  
  # analyze overlap in each image and class
  
  for(i in 1:n.images){
    tt.test<-annos[annos$PIC_NAME==images[i],]
    
    # create output for iteration on target class within each image
    
    class_out<-list()
    tp_classes<-list()
    false_positives<-numeric()
    
    # interate for each class within each image
    
    for(k in 1:n.classes){
      test<-tt.test[tt.test$INDEX==k,]
      # if a particular class is not present in the image, skip and report NA for result
      if(dim(test)[1]==0){
        class_out[[k]]<-NA
        false_positives[k]<-0
      } else {
        k_classes<-length(unique(test$CLASS))
        if(k_classes!=2){
          # filtering likely caused a predicted class to no longer be present. Thus, no predictions available to assess
          class_out[[k]]<-NA
          false_positives[k]<-0
        } else {
          # build polygons from coordinates indicating bottom left x and y and top right x and y
          #first thing is to make a long-format data frame with 5 polygon vertices for X and Y built from the corners of the bounding box
          # X pattern is TLX BRX BRX TLX TLX
          # Y pattern is TLY  TLY BRY BRY TLY
          n.polys<-length(test[,1])
          out<-list()
          class<-list()
          for(j in 1:n.polys){
            dat<-test[j,] # select ith row of data
            out[[j]]<-data.frame(ID=dat$DETECTION_ID,
                                 X=c(dat$TLX, dat$BRX, dat$BRX, dat$TLX, dat$TLX),
                                 Y=-c(dat$TLY, dat$TLY, dat$BRY, dat$BRY, dat$TLY))
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
          #pass out the truth and prediction lists for class k in image i
          tp_classes[[k]]<-classes
          names(tp_classes)[[k]]<-t.classes[k]
          
          # now assess overlap of the classes

          # use st_intersects(obj1, obj2, sparse=FALSE) for basic analysis of clearly unique polygons
          x<-st_intersects(classes[[2]], classes[[1]], sparse=FALSE)
          xx<-apply(x, 1, any)
          # identify each polygon in the prediction class that is a clear FP
          # what polygon ID are we working with
          indx<-rep(NA, length(classes[[2]][,1]))
          indx<-ifelse(xx, NA, "FP")
          #
          FP_IDS<-c(FP_IDS, classes[[2]]$ID[!xx])
          # will later update the class[[2]]$TYPE index based on evaluation of area overlap below
          classes[[2]]$TYPE<-indx
          # for later merging, also add the TYPE index to classes[[1]]
          classes[[1]]$TYPE<-NA
          # length will give the number of false positives, assuming predictions are the first geometry and truth is in the second
          # this only accounts for the polygons that exhibit no overlap. 
          false_positives[k]<-length(xx[xx==FALSE])
          
          # but the issue isn't only the absence of overlap, it's the quality of overlap if it exists. we want to know if it overlaps by a decent margin. 
          #
          # the following creates polygons of the overlapping areas
          #
          # to assess false negatives, place the "predictions" in the first location and "truth" annotations in the second location
      
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
          
          # assess percent of overlap that the prediction and annotation have
          # we expect a good prediction to mostly overlap the annotated area. if the overlap is poor - reject as true prediction and become False negative
          
          tt.int$PROP_OVERLAP<-tt.int$OVERLAP_AREA/tt.int$ANNO_AREA
          
          # also assess how much of prediction is in the overlap area. 
          # if a high proportion, it means the prediction is mainly centered over the annotation, but smaller
          # so, an assessment of whether to retain a prediction requires either high overlap with the annotation,
          # or for the prediction to be mostly represented in the overlap area
          
          tt.int$PROP_PRED_OVERLAP<-tt.int$OVERLAP_AREA/tt.int$PRED_AREA
          tt.int<-tt.int[order(tt.int$ID),]
          class_out[[k]]<-tt.int
          # 
          # the output is a data frame that identifies the polygon ids from the truth and prediction that overlap
          # if prediction overlaps with more than one, the overlap is indicated by addition of 0.1 
          # example, if prediction 10 overlaps with truth 46 and 47, each overlap is presented as 10 overlaps with 46, 10.1 overlaps with 47, etc. 
          # in some cases overlap is represented by a line, if so, then geometry indicates repeated x or y coordinates, depending on orientation - those could be deleted)
        }
      }
    }
    out_image[[i]]<-class_out
    out_classes[[i]]<-tp_classes
    fp[[i]]<-false_positives
  }
  
  fp<-do.call("rbind", fp)
  fp<-data.frame(fp)
  names(fp)<-t.classes
  
  # now analyze the outputs of the overlap to add false positives based on poor overlaps
  # will apply a function for each image and class
  
  new_fp<-list()
  for(i in 1:n.images){
    dat<-out_image[[i]]
    fps<-numeric()# should house n values (1 for each class annotated) 
    for(j in 1:n.classes){
      t.dat<-dat[[j]]
      t.dat.dim<-dim(t.dat)[1]
      t.dat.dim<-ifelse(is.null(t.dat.dim),0,t.dat.dim)
      if(!is_sf(t.dat)|t.dat.dim==0){
        # is_sf is a custom function to ensure only sf object (data with polygons) is passed along. 
        # if a particular class has no observations, assign 0 false positives to it's space
        fps[j]<-0
      } else {
        
        # test 1 find truth annotations that have one prediction overlapping it
        
        n_occur<-data.frame(table(t.dat$ID.1))
        n_occur<-n_occur[n_occur$Freq>1,]
        names(n_occur)<-c("ID.1", "MULTI_TRUTH")
        t.dat<-merge(t.dat, n_occur, by="ID.1", all=TRUE, sort=TRUE)
        
        n_occur<-data.frame(table(t.dat$ID))
        n_occur<-n_occur[n_occur$Freq>1,]
        names(n_occur)<-c("ID", "MULTI_PRED")
        t.dat<-merge(t.dat, n_occur, by="ID", all=TRUE, sort=TRUE)
        
        # the above indices identify 
        t.dat$MULTI_TRUTH<-ifelse(is.na(t.dat$MULTI_TRUTH), 0, t.dat$MULTI_TRUTH)
        t.dat$MULTI_PRED<-ifelse(is.na(t.dat$MULTI_PRED), 0, t.dat$MULTI_PRED)
        t.dat$REGULAR<-t.dat$MULTI_TRUTH+t.dat$MULTI_PRED
        t.dat$REGULAR<-ifelse(t.dat$REGULAR>0, 0,1)
        
        # use REGULAR to assess simple FP rate of one prediction to one truth overlaps

        fp_scenario<-numeric()
        tt.dat<-t.dat[t.dat$REGULAR==1,] # regular overlap indicates one:one truth and prediction annotations
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
          fpid<-tt.dat$ID[s3>1] # false positives fail both threshold criteria
          tpid<-tt.dat$ID[s3<=1] # true positives meet at least one of the criteria
          FP_IDS<-c(FP_IDS, fpid)
          TP_IDS<-c(TP_IDS, tpid)
        }
        
        # test 2 -- where n predictions overlap 1 annotation 
        
        # assume only annotation that overlaps the most is worth counting, then assess it's overlap as above
        tt.dat<-t.dat[t.dat$REGULAR==0,]
        tt.dat<-tt.dat[tt.dat$MULTI_TRUTH>1,]
        
        # check if any data exist
        if(dim(tt.dat)[1]==0){
          # assign no fp for scenario 1 if no data exist
          fp_scenario[2]<-0
        } else {
          # now assess which of the overlaps to keep for assessment as FP and which to discard are incidental overlap
          n.overlaps<-length(unique(tt.dat$ID.1))
          overlaps<-unique(tt.dat$ID.1)
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
            fpid<-fp.dat$ID[fp.dat$MULTI_PRED==0]
            FP_IDS<-c(FP_IDS, fpid)
            fpcount<-fpcount+tt.fp
            # now assess the "better" predictions to see if they meet FP criteria
            ttt.dat<-ttt.dat[ttt.dat$KEEP==1,]
            # now assess remaining overlap
            s1<-ifelse(ttt.dat$PROP_OVERLAP<over1, 1, 0)
            s2<-ifelse(ttt.dat$PROP_PRED_OVERLAP<over2, 1,0)
            s3<-s1+s2
            fpk<-fpk+length(s3[s3>1])
            fpid<-ttt.dat$ID[s3>1]
            FP_IDS<-c(FP_IDS, fpid)
            tpid<-ttt.dat$ID[s3<=1] # true positives meet at least one of the criteria
            TP_IDS<-c(TP_IDS, tpid)
          }
          # add up the false positives counted here
          fp_scenario[2]<-fpk+fpcount
        }
        
        # test 3 --test where 1 prediction overlaps n annotations 
     
        tt.dat<-t.dat[t.dat$REGULAR==0,]
        tt.dat<-tt.dat[tt.dat$MULTI_PRED>1,]
        # check if any data exist
        if(dim(tt.dat)[1]==0){
          # assign no fp for scenario 1 if no data exist
          fp_scenario[2]<-0
        } else {
          #tt.dat<-tt.dat[order(tt.dat$ID),]
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
            fpid<-ttt.dat$ID[s3>1]
            FP_IDS<-c(FP_IDS, fpid)
            tpid<-ttt.dat$ID[s3<=1] # true positives meet at least one of the criteria
            TP_IDS<-c(TP_IDS, tpid)
          }
          fp_scenario[3]<-fpk
        }
      }
      # correct FP/TP double counting before leaving the class here
      # because code might prematurely assign FP status, the list of FP IDs and TP IDs needs to be checked and corrected for each class
      x<-FP_IDS%in%TP_IDS
      n.fps<-length(FP_IDS)
      FP_IDS<-FP_IDS[!x] # only keep the valid false positives
      nn.fps<-length(FP_IDS)
      lose.n<-n.fps-nn.fps
      fps[j]<-sum(fp_scenario)-lose.n
      # end of iteration for each class within each photo
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
  res<-list()
  res[[1]]<-data.frame(CONF=conf.thresh, TRUTH_OVERLAP=over1, PREDICTION_OVERLAP=over2, FP=fp, FN=fn, TP=tp, ANNO=ANNO, PREDS=PREDS, ACCURACY=accur, PRECISION=prec, RECALL=recall,F1=f1)
  res[[2]]<-out_classes
  res[[3]]<-FP_IDS
  res[[4]]<-index
  res
}
