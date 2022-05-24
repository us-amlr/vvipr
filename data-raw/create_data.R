## code to prepare `create_data.R` dataset goes here

NAMES<-c("DETECTION_ID", "PIC_NAME", "IMAGE", "TLX", "TLY", "BRX", "BRY",
         "CONF", "TARGET", "CLASS", "CONF_2")

func_image_name <- function(x) {
  z<-as.vector(table(x))
  rep(1:length(unique(x)), z)
}


# Process input CSVs
truth_ex <-read.csv(file="inst/extdata/peng4_truth.csv", skip=2, header=FALSE)
names(truth_ex)<-NAMES
truth_ex$IMAGE <- func_image_name(truth_ex$IMAGE)


prediction_ex<-read.csv(file="inst/extdata/peng4_detections.csv", skip=2, header=FALSE)
names(prediction_ex)<-NAMES
# Why is this necessary?
prediction_ex$DETECTION_ID<-seq(from=max(truth_ex$DETECTION_ID)+1000, by=1,
                             length.out=length(prediction_ex$IMAGE))
prediction_ex$IMAGE <- func_image_name(prediction_ex$IMAGE)


usethis::use_data(truth_ex, overwrite = TRUE)
usethis::use_data(prediction_ex, overwrite=TRUE)
