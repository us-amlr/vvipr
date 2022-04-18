# H1 vvipr
# H2 Verify VIAME Predictions

This code is developed as a Shiny app to assist with comparing image classification model output from VIAME software with "truth" annotations to assess model performance with a suite of performance metrics.

The code takes two .csv files as input. One is the set of truth annootations, the other is the set of predictions from a trained model. You then can select appropriate cut-offs for retention of model predictions, and the amount of overlap between truth and prediction bounding boxes that are required to classify detections as true positives, false positives, or false negatives. The code wwil compute 4 model performance metrics: accuracy, precision, recall, and the F1 score. 

Optionally, you can visualize the location and overalp of truth and predicted classes by selecting an image and class. Image and class options are generated fro the input "truth"" file. 

