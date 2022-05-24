library(shiny)
library(sf)
library(sfheaders)
library(vvipr)

csv.accept <- c(
  "text/csv",
  "text/comma-separated-values,text/plain",
  ".csv"
)


ui<-fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
    "))
  ),
  sidebarLayout(
    sidebarPanel(
      h1("vvipr", style="text-align:center"),
      h2("Verify VIAME Predictions", style="text-align:center"),
      p(style="text-align:center", "Evauluate VIAME predictions against truth annotations"),
      p(style="text-align:center","Last updated: 22 May 2022"),

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

      numericInput(inputId="over1",
                   label="4. Enter minimum proportion of truth area needed for overlap (Truth overlap)",
                   value=0.5, min=0, max=0.99, step=0.01),

      numericInput(inputId="over2",
                   label="5. Enter minimum proportion of predicted area needed for overlap (Prediction overlap)",
                   value=0.5, min=0, max=0.99, step=0.01),

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


server<-function(input, output, session){

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

  func_image_name <- function(x) {
    z<-as.vector(table(x))
    rep(1:length(unique(x)), z)
  }


  # Process input CSVs
  data_truth <- reactive({
    truth <- read.csv(file=req(input$truth$datapath), skip=2, header=FALSE)
    names(truth)<-NAMES
    truth$IMAGE <- func_image_name(truth$IMAGE)
    truth
  })

  data_prediction <- reactive({
    validate(
      need(input$prediction, "Please upload a model prediction file."),
    )
    prediction<-read.csv(file=req(input$prediction$datapath), skip=2, header=FALSE)
    names(prediction)<-NAMES
    truth <- data_truth()
    # Why is this necessary?
    prediction$DETECTION_ID<-seq(from=max(truth$DETECTION_ID)+1000, by=1,
                                 length.out=length(prediction$IMAGE))
    prediction$IMAGE <- func_image_name(prediction$IMAGE)

    prediction
  })


  #-----------------------------------------------------------------------------
  # Run input data through vvipr functions
  reac_func_output<-reactive({
    assess_overlap_shiny(
      truth=data_truth(), prediction=data_prediction(),
      conf.thresh=input$conf.thresh, over1=input$over1, over2=input$over2
    )
  })

  output$result1<-renderTable({
    reac_func_output()[[1]][1:3]
  }, striped=TRUE, caption="Input parameters")

  output$result2<-renderTable({
    reac_func_output()[[1]][4:8]
  }, striped=TRUE, caption="Counts of false positives (FP), false negatives (FN), true positives (TP), total annotations (ANNO), and total predictions (PREDS)")

  output$result3<-renderTable({
    reac_func_output()[[1]][9:12]
  }, striped=TRUE, caption="Performance scores for accuracy, precision, recall and F1")

  sf_out_reactive<-reactive({
    req(input$image, input$class)
    plot.list<-reac_func_output()

  
    sf_data<-plot_image_class(
      dat1=plot.list[[2]], dat2=plot.list[[3]], dat3=plot.list[[4]],
      conf.thresh=input$conf.thresh, over1=input$over1, over2=input$over2,
      image=input$image, class=input$class)

    sf_data
  })
  output$plots<-renderPlot({


    sf_out<-sf_out_reactive()
    validate(
      need(class(sf_out)=="list", "Given input values, the target class is either not annotatated or not predicted for this image. No comparison is possible."),
    )
    
    MAIN<-paste("Image: ", input$image,  "  Class: ", input$class, sep="")
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
      write.csv(reac_func_output()[[1]], file, row.names=FALSE)
    }
  )
}

shinyApp(ui=ui, server=server)
