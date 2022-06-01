# vvipr
## Verify VIAME Predictions

This little ditty was developed as a [Shiny app](https://shiny.rstudio.com/) to quantify the performance of an image classification model built with [VIAME](https://www.viametoolkit.org/) software. The app compares the output from the model with corresponding user-defined "truth" annotations of the same set of images. At present, the code supports object detections within those image frames for any number of user-defined object classes.

The code takes two .csv files as input. One is the set of truth annotations, the other is the set of predictions from a trained model applied to the same images for which the truth annotations exist. The user can select appropriate cut-offs for retention of model predictions based on the confidence threshold assigned to predictions by the model. The user can also adjust the amount of overlap between truth and prediction bounding boxes that is required to classify detections as true positives (TP), false positives (FP), or false negatives (FN). The code will compute 4 model performance metrics: accuracy, precision, recall, and the F1 score. 

Where:

**accuracy** = *TP/(TP+FP+FN)*

**precision** = *TP/(TP+FP)*

**recall** = *TP/(TP+FN)*

**F1** = *TP/(TP+0.5(FP+FN))*


Optionally, the user can visualize the location and overlap of truth and predicted classes in the image (the image itself is not plotted) by selecting an image number and object class from the appropriate drop-downs. Image and class options are generated automatically once the truth annotation is uploaded. Images that do not contain a specified class will result in no plot. 

## General idea of the analysis

The analysis is only concerned with identifying false positives in the detections, because the false negatives and true positives can be estimated via a simple optimization routine if you know the number of false positives, total truth annotations, and total predictions) 

The analysis first looks for predictions whose bounding boxes do not overlap any truth annotations. Those are the easy-to-identify false positives. Their presence/absence can change with the the confidence threshold supplied (Step 3).

The next steps iteratively assess how partial overlap of the truth and prediction bounding boxes should be treated. It is based on the areas of the truth and prediction bounding boxes, whether their overlap meets the threshold supplied, and deals with several possible configurations of overlap. 

The first overlap parameter (Step 4) simply defines the proportion of the truth area that needs to be overlapped by a prediction area to qualify as a match. It is applied to scenarios with single and multiple overlaps of truth and predictions.

The second overlap parameter (Step 5) is essentially the reverse - how much of the prediction area covers a truth annotation. If the overlap criteria of Step 4 is not met, the analysis double checks overlap against Step 5. This was borne of cases where a prediction area was small relative to the truth, so that Step 4 overlap threshold is routinely rejected, but the prediction may still be valid if it meets the Step 5 threshold. In most instances, changing the threshold in Step 5 won't really affect the results very much unless there are a lot of these really small prediction areas. Still, I felt it needed to be checked in case this happens in future runs.

## Installation

Here are some simple instruction for running the vvipr app from RStudio (points 1-5) or online [here](https://jefferson.shinyapps.io/vvipr/) (points 2-5).

To install the latest version from GitHub:

 ```r
install.packages("devtools")
devtools::install_github("jthinke/vvipr")
```

## Usage

1) Run the app with 
```r
vvipr::vvipr()
```

2) There will be 2 data files to load. The first is the annotations representing the truth data (an example: [truth.csv](https://github.com/jthinke/vvipr/blob/main/inst/extdata/peng4_truth.csv)) and the second is the file containg the model predictions from the trained model (an example: [predictions.csv](https://github.com/jthinke/vvipr/blob/main/inst/extdata/peng4_detections.csv)). To load your own, navigate to where the data are housed and allow upload. 

3) Specify desired values for confidence thresholds and overlap proportions.

4) Optionally change the image and class of detection (based on what's in the truth annotation files).

5) Use the download button to download results.

## Citation

Hinke JT, Giuseffi LM, Hermanson VR, Woodman SM, and Krause DJ. (in prep). Evaluating a machine learning approach to detect penguins and seals in thermal and color images collected with an unoccupied aerial system.

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
