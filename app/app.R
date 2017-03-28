#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gridExtra)
library(shinyFiles)
library(readr)
print(getwd())
source("../mosaiq.R")
settings = ''
roots = c(wd='~/')
ui <- shinyUI(fluidPage(
  titlePanel("Mosaiq"),
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
    dat = read.csv(values$datapath, header=T, nrow=10000)
    values$dataset = dat 
  })
  output$choose_fields <- renderUI({
    if(is.null(values$dataset)) 
      return()
    colnames <- names(values$dataset)
    column(12,
      br(),
      selectInput("target", "choose target", choices=colnames),
      selectInput("field", "choose field", choices=colnames)
    )
  })
  observeEvent(input$key,{
    code = input$key.which
    if (code == 37 || code == 39) {
      
    }
  })
  observeEvent({
    input$target 
    input$field
    },{
      if (is.null(input$target) || is.null(input$field))
        return()
      output$plot <-renderPlot({
        mosaic_feature(values$dataset, input$field, input$target)
      })
  })
})

# Run the application 
shinyApp(ui = ui, server = server)
