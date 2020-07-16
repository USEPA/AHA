library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("global.R")
program_code_UI <- function(id){
  ns <- NS(id)
  uiOutput(ns("program_code_output"))
}

program_code <- function(input, output, session, multiple_codes=TRUE){
  ns <- session$ns
  output$program_code_output <- renderUI({
    allowance_codes <- c('ARP', 'CSOSG1','CSOSG2','CSNOXOS','CSSO2G1','CSSO2G2', 'CSNOX','TXSO2', 'CAIRNOX', 'CAIROS','CAIRSO2', 'OTC', 'NBP')
    pickerInput(
    ns("program_code_input"),
      label = "Program Code",
      selected = NULL,
      allowance_codes,
      multiple = multiple_codes,
      options = list('actions-box' = TRUE)
    )
  })
  observeEvent(input$program_code_input,{
    print(paste0("selected code:", input$program_code_input))
  })
  
  
  reactive(
    list(
      prg_code = input$program_code_input
    )
  )
  
  
}