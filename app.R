#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
options(shiny.launch.browser=TRUE)
library(ggplot2)
library(ggmosaic)
library(gridExtra)
library(shinyFiles)
print(getwd())
source("mosaiq.R")
settings = ''
roots = c(wd='~/')

ui <- shinyUI(fluidPage(
  titlePanel(title=div( img(src="http://i.imgur.com/oHzfOOA.png", height="30px"), "Mosaiq")),
  sidebarLayout(
    sidebarPanel(
      shinyFilesButton('file', 'Csv select', 'Please select a csv file', FALSE),
      verbatimTextOutput('filepaths'),
      uiOutput("choose_fields")
    ),
    mainPanel(
      plotOutput("plot",height="600px")
    )
  )
))

server <- shinyServer(function(input, output, session) {
  output$keypress = renderPrint({input$key})
  values <-reactiveValues(dataset=NULL, datapath = NULL)
  shinyFileChoose(input, 'file', roots=roots, filetypes=c('', 'csv'))
  output$rawInputValue <- renderPrint({str(input$file)})
  output$filepaths <- renderPrint({
    if (is.null(input$file))
      return()
    values$datapath<-parseFilePaths(roots, input$file)$datapath
    values$datapath
  })
  observeEvent(values$datapath,{
    dat = read.csv(as.character(values$datapath), header=T, nrow=10000)
    values$dataset = dat 
  })
  output$choose_fields <- renderUI({
    if(is.null(values$dataset)) 
      return()
    colnames <- names(values$dataset)
    column(12,
      br(),
      selectInput("field", "choose field", choices=colnames),
      selectInput("target", "choose target", choices=colnames),
      uiOutput("choose_numeric_options")
    )
  })
  output$choose_numeric_options <- renderUI({
    if (is.null(input$target)){
      return()
    }

    if(!is.numeric(values$dataset[[input$target]])){
      return()
    }
    fluidRow(
     checkboxInput("log10_scaling", "log10 scale target", value=FALSE) ,
     checkboxInput("invert_color_ramp", "invert target color ramp", value=FALSE) 
    )
  })
  observeEvent({
    input$target 
    input$field
    input$log10_scaling
    input$invert_color_ramp
    },{
      if (is.null(input$target) || is.null(input$field))
        return()
      output$plot <-renderPlot({
        mosaic_feature(values$dataset, input$field, input$target, input$log10_scaling, input$invert_color_ramp)
      })
  })
})

# Run the application 
shinyApp(ui = ui, server = server)
