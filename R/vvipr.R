#' vvipr
#'
#' Runs the R Shiny App
#' 
#' @param ... passed to \code{\link[shiny]{shinyApp}}
#' 
#' @details
#' The Verify VIAME Prediction (vvipr) code was developed as a Shiny app to quantify the performance of an image classification model built with VIAME software. The app compares the output from the model with corresponding user-defined "truth" annotations of the same set of images. At present, the code supports object detection within those image frames for any number of user-defined object classes.
#' 
#' The code takes two files as input. One is the set of truth annotations, the other is the set of predictions from a trained model applied to the same images for which the truth annotations exist. The user can select appropriate cut-offs for retention of model predictions based on the confidence threshold assigned to predictions by the model. The user can also adjust the amount of overlap between truth and prediction bounding boxes that is required to classify detections as true positives (TP), false positives (FP), or false negatives (FN). The code will compute 4 model performance metrics: accuracy, precision, recall, and the F1 score.
#' 
#' Once the app is open, select appropriate input .csv files, set appropriate thresholds, and behold the overlap. 
#' 
#' @return 
#' The app returns a data frame for download as a .csv file, if desired. The data frame combines the three tables displayed by the app and include the input values used, the counts of annotations, predictions, true and false positives, and the performance scores. These data reflect the performance of the model as a whole. 
#'  
#' @examples
#' if (interactive()) vvipr()
#' 
#' @export
vvipr <- function(...) {
  csv.accept <- c(
    "text/csv",
    "text/comma-separated-values,text/plain",
    ".csv"
  )
  
  
  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
    "))
    ),
    
    ### Use shinybusy to indicate when plot work is being done
    # Options: https://epic-spinners.epicmax.co/
    shinybusy::add_busy_spinner(
      spin = "double-bounce", position = "top-right", margin = c(20, 20),
      height = "100px", width = "100px"
    ),
    
    sidebarLayout(
      sidebarPanel(
        h1("vvipr", style="text-align:center"),
        h2("Verify VIAME Predictions", style="text-align:center"),
        p(style="text-align:center", "Evauluate VIAME predictions against truth annotations"),
        p(style="text-align:center","Last updated: 21 June 2022"),
        
        fluidRow(
          column(6, fileInput(inputId="truth", label="1. Choose truth annotations",
                              accept = csv.accept)),
          column(6, fileInput(inputId="prediction", label="2. Choose model predictions",
                              accept = csv.accept))
        ),
        
        hr(),
        
        numericInput(inputId = "conf.thresh",
                     label = "3. Enter confidence threshold level between 0 and 0.99",
                     value=0, min=0, max=0.99, step=0.01),
        
        fluidRow(
          column(6, numericInput(inputId="over1",
                                 label="4. Enter minimum proportion of truth area needed for overlap (Truth overlap)",
                                 value=0.5, min=0, max=0.99, step=0.01)),
          
          column(6, numericInput(inputId="over2",
                                 label="5. Enter minimum proportion of predicted area needed for overlap (Prediction overlap)",
                                 value=0.5, min=0, max=0.99, step=0.01)),
          
          actionButton("goButton","Update"),
          p("Click the button to update results")
        ),
        
        hr(),
        
        # tags$h3("Plotting"),
        
        uiOutput("image_uiOut"),
        uiOutput("class_uiOut"),
        uiOutput("legend_pos_uiOut"),
        
        downloadButton("downloadData", "Download results"),
        p("Download the table of results.")
      ),
      
      mainPanel(
        #h2("Model performance metrics"),
        #p("Input parameters"),
        textOutput("data_message_uiOut_text"),
        tableOutput("result1"),
        #p("Counts of false positives (FP), false negatives (FN), true positives (TP), total annotations (ANNO), and total predictions (PREDS)"),
        tableOutput("result2"),
        #p("Performance scores for accuracy, precision, recall and F1"),
        tableOutput("result3"),
        #h2("Plot for selected image and class"),
        plotOutput("plots", height="800px")
      )
    )
  )
  
  
  server <- function(input, output, session){
    #---------------------------------------------------------------------------- 
    vals <- reactiveValues(
      check_class_warning = NULL, 
      assess_overlap_output = NULL
    )
    
    ### Print relevant validate/error message(s) for assess_overlap
    output$data_message_uiOut_text <- renderText({
      validate(
        need(input$truth, "Please upload a truth annotations file."),
        need(input$prediction, "Please upload a model prediction file.")
      )
      
      # # if using tryCatch():
      # if (is.null(vals$assess_overlap_output)) {
      #   validate("Please click 'Update results' button to generate outputs")
      # } else if (inherits(vals$assess_overlap_output, "simpleError")) {
      #   validate(vals$assess_overlap_output$message)
      # } else if (inherits(vals$assess_overlap_output, "simpleWarning")) {
      #   validate(vals$assess_overlap_output[[2]]$message)
      # }
      
      # is using try()
      if (is.null(vals$assess_overlap_output)) {
        validate("Please click 'Update results' button to generate outputs")
      } else {
        validate(
          need(vals$assess_overlap_output,
               attr(vals$assess_overlap_output, "condition")$message)
        )
      }
      
      NULL
    })
    
    ### Print relevant warnings pre - assess_overlap
    output$check_class_warning_uiOut_text <- renderText({
      # Only print warnings if vals$assess_overlap_output is truthy
      req(vals$assess_overlap_output)
      
      # Print any warning messages in this reactiveValue
      req(vals$check_class_warning)
    })
    
    #-----------------------------------------------------------------------------
    # Render UIs
    
    output$image_uiOut <- renderUI({
      req(data_truth())
      
      selectInput("image", "6. Select image to plot",
                  choices=as.list(unique(data_truth()$IMAGE)))
    })
    
    output$class_uiOut <- renderUI({
      req(data_truth())
      
      selectInput("class", "7. Select annotation class to plot",
                  choices=as.list(unique(data_truth()$CLASS)))
    })
    
    output$legend_pos_uiOut <- renderUI({
      req(data_truth())
      
      selectInput("legend_pos", "Select legend position",
                  choices=list("bottomright", "bottom", "bottomleft", "left",
                               "topleft", "top", "topright", "right", "center"),
                  selected = "bottomleft")
    })
    
    
    
    
    #-----------------------------------------------------------------------------
    # Helpers
    NAMES<-c("DETECTION_ID", "PIC_NAME", "IMAGE", "TLX", "TLY", "BRX", "BRY",
             "CONF", "TARGET", "CLASS", "CONF_2")
    #
    func_image_name <- function(x) {
      z<-as.vector(table(x))
      rep(1:length(unique(x)), z)
    }
    
    
    #-----------------------------------------------------------------------------
    # Process input CSVs
    ### Reset output reactiveValues if new files are uploaded
    observeEvent(c(input$truth, input$prediction), {
      vals$assess_overlap_output <- NULL
    })
    
    data_truth <- reactive({
      truth <- read.csv(file=req(input$truth$datapath), skip=2, header=FALSE)
      names(truth)<-NAMES
      # order truth on image to ensure consistent numbering between VIAME sequence and plotting here
      truth<-truth[order(truth$IMAGE),]
      truth$IMAGE <- func_image_name(truth$IMAGE)
      truth
    })
    
    data_prediction <- reactive({
      prediction<-read.csv(file=req(input$prediction$datapath), skip=2, header=FALSE)
      names(prediction)<-NAMES
      # order prediction on image to ensure consistent numbering between VIAME sequence and plotting here
      prediction<-prediction[order(prediction$IMAGE),]
      truth <- data_truth()
      # Why is this necessary? -- because each polygon needs a unique ID
      prediction$DETECTION_ID<-seq(from=max(truth$DETECTION_ID)+1000, by=1,
                                   length.out=length(prediction$IMAGE))
      prediction$IMAGE <- func_image_name(prediction$IMAGE)
      
      prediction
    })
    
    #-----------------------------------------------------------------------------
    # Run input data through vvipr functions, and create plots and tables
    # reac_func_output<-reactive({
    observeEvent(input$goButton, {
      truth <- data_truth()
      pred <- data_prediction()
      # if using try():
      check.class <- !all(unique(truth$CLASS) %in% unique(pred$CLASS)) ||
        !all(unique(pred$CLASS) %in% unique(truth$CLASS))
      
      vals$check_class_warning <- if (check.class) {
        "warning message here"
      } else {
        NULL
      }
      
      vals$assess_overlap_output <- try(
        assess_overlap(
          truth=input$truth$datapath, prediction=input$prediction$datapath,
          conf.thresh=input$conf.thresh, over1=input$over1, over2=input$over2
        ), 
        silent = TRUE
      )
      
      # # if using tryCatch():
      # vals$assess_overlap_output <- tryCatch({
      #   assess_overlap(
      #     truth=input$truth$datapath, prediction=input$prediction$datapath,
      #     conf.thresh=input$conf.thresh, over1=input$over1, over2=input$over2
      #   )
      # }, 
      # error = function(e) e, 
      # warning = function(w) {
      #   l.out <- list(
      #     suppressWarnings(assess_overlap(
      #       truth=input$truth$datapath, prediction=input$prediction$datapath,
      #       conf.thresh=input$conf.thresh, over1=input$over1, over2=input$over2
      #     )), 
      #     w
      #   )
      #   class(l.out) <- c("list", class(w))
      #   l.out
      # })
      
    })
    
    assess_overlap_output_reac <- reactive({
      # if using try():
      req(vals$assess_overlap_output)
      
      # # if using tryCatch():
      # req(vals$assess_overlap_output, 
      #     !inherits(vals$assess_overlap_output, "simpleError"))
      # if (inherits(vals$assess_overlap_output, "simpleWarning")) {
      #   vals$assess_overlap_output[[1]]
      # } else {
      #   vals$assess_overlap_output
      # }
    })
    
    output$result1<-renderTable({
      assess_overlap_output_reac()[[1]][1:3]
    }, striped=TRUE, caption="Input parameters")
    
    output$result2<-renderTable({
      assess_overlap_output_reac()[[1]][4:8]
    }, striped=TRUE, caption="Counts of false positives (FP), false negatives (FN), true positives (TP), total annotations (ANNO), and total predictions (PREDS)")
    
    output$result3<-renderTable({
      assess_overlap_output_reac()[[1]][9:12]
    }, striped=TRUE, caption="Performance scores for accuracy, precision, recall and F1")
    
    sf_out_reactive<-reactive({
      req(input$image, input$class)
      plot.list<-assess_overlap_output_reac()
      
      sf_data<-plot_image_class(
        dat1=plot.list[[2]], dat2=plot.list[[3]], dat3=plot.list[[4]], dat4=plot.list[[5]],
        conf.thresh=input$conf.thresh, over1=input$over1, over2=input$over2,
        image=input$image, class=input$class)
      
      sf_data
    })
    
    
    output$plots<-renderPlot({
      
      
      sf_out<-sf_out_reactive()
      validate(
        need(class(sf_out)=="list", "Given input values, the target class is either not annotatated or not predicted for this image. No comparison is possible."),
      )
      
      MAIN<-paste("Image: ", input$image, " (", sf_out[[3]], ")  Class: ", input$class, sep="")
      t.col1<-adjustcolor(col="black", alpha.f=0.75)
      plot(sf_out[[1]][[1]]$geometry, border=1, col=t.col1, main=MAIN, xlim=sf_out[[2]][c(1,3)], ylim=sf_out[[2]][c(2,4)])
      t.col2<-adjustcolor(col="red", alpha.f=0.66)
      plot(sf_out[[1]][[2]]$geometry, add=TRUE, border=1, col=t.col2)
      t.col3<-adjustcolor(col="yellow", alpha.f=0.5)
      plot(sf_out[[1]][[3]]$geometry, add=TRUE, border=1, col=t.col3)
      legend(x=input$legend_pos, border="black", fill=c(t.col1, t.col2, t.col3), cex=2,
             legend=c("Truth", "False positive", "True positive"), bty="n")
      box()
      #
    })
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("vvipr_output_ct-", input$conf.thresh, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(assess_overlap_output_reac()[[1]], file, row.names=FALSE)
      }
    )
  }
  
  shiny::shinyApp(ui = ui, server = server, ...)
}
