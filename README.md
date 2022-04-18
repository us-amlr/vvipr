#  vvipr
## Verify VIAME Predictions

This code was developed as a [Shiny app](https://shiny.rstudio.com/) to quantify the performance of an image classification model built with [VIAME](https://www.viametoolkit.org/) software. The app compare the output from the model with corresponding, user-defined "truth" annotations of the same set of images.  At present, the code supports object detections within those image frames for any number of user-defined object classes.

The code takes two .csv files as input. One is the set of truth annotations, the other is the set of predictions from a trained model. The user  can select appropriate cut-offs for retention of model predictions based on the confidence threshold assigned to predictions by the model. The user can also adjust the amount of overlap between truth and prediction bounding boxes that is required to classify detections as true positives (TP), false positives (FP),  or false negatives (FN). The code will compute 4 model performance metrics: accuracy, precision, recall, and the F1 score. 

Where:

**accuracy** = *TP/(TP+FP+FN)*

**precision** = *TP/(TP+FP)*

**recall** = *TP/(TP+FN)*

**F1** = *TP/(TP+0.5(FP+FN))*


Optionally, the user can visualize the location and overlap of truth and predicted classes in teh image (the image itself is not plotted) by selecting an image number and object class from the appropriate drop-downs. Image and class options are generated from the input "truth" file once the `Enable plots` button has been clicked. Images the do not contain a specified class result in no plot. 

