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
      shinyFilesButton('file', 'File select', 'Please select a file', FALSE),
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
      uiOutput("choose_target_levels")
    )
  })
  output$choose_target_levels <- renderUI({
    if (is.null(input$target))
      return()
     vals <-input$target
     vals = values$dataset[[input$target]]
     if (!is.numeric(vals)){
        text = paste(names(table(topn(vals))),collapse=", ")
     } else {
       text = paste(hist(vals,plot=F, breaks=10)$breaks,collapse=", ")
     }
     textInput("target_levels", "choose target levels", value=text) 
  })
  observeEvent({
    input$target 
    input$field
    input$target_levels
    },{
      if (is.null(input$target) || is.null(input$field) || is.null(input$target_levels))
        return()
      output$plot <-renderPlot({
        mosaic_feature(values$dataset, input$field, input$target, input$target_levels)
      })
  })
})

# Run the application 
shinyApp(ui = ui, server = server)
