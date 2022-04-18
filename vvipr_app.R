library(shiny)
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
      fluidRow(
        column(12,
               h1("vvipr", style="text-align:center"),
               h2("Verify VIAME Predictions", style="text-align:center"),
               p(style="text-align:center", "Evauluate VIAME predictions against truth annotations"),
               p(style="text-align:center","Last updated: 18 April 2022"))
      ),
      
      hr(),

      fluidRow(
        column(12, 
               fileInput(inputId="truth", label="1. Choose truth annotations",
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))),
      ),
      
      fluidRow(
        column(12, 
               fileInput(inputId="prediction", label="2. Choose model predictions",
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))),
      ),
      
      fluidRow(
        column(12,
               numericInput(inputId = "conf.thresh",
                            label = "3. Enter confidence threshold level between 0 and 0.9",
                            value=0, min=0, max=0.99, step=0.01)),
      ),
  
      fluidRow(
        column(12,
               numericInput(inputId="over1", 
                            label="4. Choose minimum overlap of prediciton and truth for accepting",
                            value=0.5, min=0, max=0.99, step=0.01)),
      ),
      
      fluidRow(
        column(12,
               numericInput(inputId="over2", 
                            label="5. Choose minimum overlap of shared area and predicted area for accepting",
                            value=0.5, min=0, max=0.99, step=0.01)),
      ),
      
      fluidRow(
        column(12, actionButton(inputId="plot.me", label="Enable plots")),
      ),
      
      fluidRow(
        column(12, selectInput("image", "6. Select image to plot", choices=list("Check plot button to update list"))),
       ),
  
      fluidRow(
        column(12, selectInput("class", "7. Select annotation class to plot", choices=list("Check plot button to update list"))),
       ),
  
      fluidRow(
        column(12, downloadButton("downloadData", "Download results"),
             p("Download the table of results generated below.")),
      ),
    ),
    
    mainPanel(
      
      fluidRow(
        tableOutput("result")
      ),
    
     
        plotOutput("plots", height="800px")
    )
  )
)


server<-function(input, output, session){
  source('assess_overlap_shiny.R') 
  source('plot_image_class.R')
  source('is_sf.R') 
  source('fill_matrix.R') 
  source('vals.R')
  source('trans_col.R')
  library(sf)
  library(sfheaders)
  library(RColorBrewer)
  data<-reactive({
    req(input$truth)
    truth<-read.csv(file=input$truth$datapath, skip=2, header=FALSE)
    prediction<-read.csv(file=input$prediction$datapath, skip=2, header=FALSE)
    NAMES<-c("DETECTION_ID","PIC_NAME", "IMAGE", "TLX", "TLY", "BRX", "BRY","CONF", "TARGET", "CLASS", "CONF_2")
    names(truth)<-NAMES
    names(prediction)<-NAMES
    prediction$DETECTION_ID<-seq(from=max(truth$DETECTION_ID)+1000, by=1, length.out=length(prediction$IMAGE))
    classes_to_use<-unique(truth$CLASS)
    images<-as.list(unique(truth$IMAGE))
    list(truth, prediction, classes_to_use, images)
    })
  observeEvent(input$plot.me, {
    updateSelectInput(session, inputId="class", "Select annotation class to plot", choices=data()[[3]])
    updateSelectInput(session, inputId="image", "Select image to plot", choices=data()[[4]])
  })
  
  reac_func_output<-reactive({
    req(input$conf.thresh)
    req(input$over1)
    req(input$over2)
    dat.list <- data()
    out<-assess_overlap_shiny(truth=dat.list[[1]], prediction=dat.list[[2]], conf.thresh=input$conf.thresh, over1=input$over1, over2=input$over2)
    out
  })
  output$result<-renderTable({
    #req(input$table_type)
    dat.list <- reac_func_output()
  })
  
  sf_out_reactive<-reactive({
    #req(input$conf.thresh)
    #req(input$over1)
    #req(input$over2)
    req(input$image)
    req(input$class)
    dat.list <- data()
    sf_data<-plot_image_class(truth=dat.list[[1]], prediction=dat.list[[2]], conf.thresh=input$conf.thresh, over1=input$over1, over2=input$over2, image=input$image, class=input$class)# flesh out code/function to plot overlap of selected class from selected image
    sf_data
    })
    output$plots<-renderPlot({
    dat.list <- data()
    sf_out<-sf_out_reactive()
    MAIN<-paste("Image: ", input$image,  "  Class: ", input$class, sep="")
    t.col1<-trans_col(color="red")
    plot(sf_out[[1]]$geometry, border=1, col=t.col1, main=MAIN)
    t.col2<-trans_col(color="blue")
    plot(sf_out[[2]]$geometry, add=TRUE, border=1, col=t.col2)
    t.col3<-trans_col(color="purple", percent=0.33)
    legend(x="bottomleft", pch=c(15,15,15), col=c(t.col1, t.col2, t.col3), cex=2, legend=c("Truth", "Prediction", "Areas of overlap"), bty="n")
    box()
    mtext("Image only responds to image, class, and confidence threshold inputs", side=1)
  })
  output$downloadData <- downloadHandler(
    #output$result<-downloadHandler(  
    filename = function() {
      paste(input$conf.thresh, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reac_func_output(), file, row.names=FALSE)
    }
  )
}

shinyApp(ui=ui, server=server)