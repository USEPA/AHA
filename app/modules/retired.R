library(odbc)
library(DBI)
library(dplyr)
library(dbplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(hash)
library(data.table)
source("global.R")
source("modules/programCode.R")
# This module will display a graph of all retired units for a program by the year they retire
# Need to understand difference between 'Retired', 'Retired (Retired DATE)' and Operating ('Retired DATE')

retired_UI <- function(id){
  ns <-NS(id)
  tabBox(
    title = "Retired Units",
    id = "retired", 
    height = "100%",
    width = "100%",
    tabPanel(
      program_code_UI(ns("program_code")),
      verbatimTextOutput(ns("note")),
      plotlyOutput(ns("plot_output")), 
      fluidRow(DT::dataTableOutput(ns("table_output"))),
      downloadButton(ns("download_data"),"Download Data")      
  ))
}

retired <-function(input, output, session) {
  ns <-session$ns
  prg_code <- callModule(program_code,"program_code", multiple_codes=FALSE)
  retire_output <- reactive({
    prg <- prg_code()
    prg_code <- prg$prg_code
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    
    op_status <- channel %>%
      tbl("op_status_year_dim")
    op_status
    
    filter <- paste('%', prg_code, '%', sep ="")
    filter2 <- paste('%', prg_code, ',%', sep ="")
    
    unit_fact <- channel %>%
      tbl("unit_fact") %>%
      filter(prg_code_info %like% filter | prg_code_info %like% filter2)
    unit_fact
    
    unit_op_join <- unit_fact %>%
      left_join(op_status, by = c("unit_id" = "unit_id", "op_year" = "op_year")) %>%
      filter(op_status != 'OPR' & op_status != 'FUT' & op_status != 'CAN') %>%
      select(unit_id,
             facility_name, 
             orispl_code,
             opr_status = op_status_description,
             retire_year = op_year,
             prg_code_info) %>%
      collect()
  })

  output$plot_output <- renderPlotly({
    prg <- prg_code()
    prg_code <- prg$prg_code
    if (is.null(prg_code)){
       return(NULL)}
    retires <- retire_output()
    retires2 <- retires %>%
      group_by(retire_year, opr_status) %>%
      summarise(total_units = n())
    plot1 <- retires2 %>%
      ggplot(aes(x = retire_year, y = total_units, fill = opr_status)) +
      geom_bar(stat = "identity", position = "stack") +
      ggtitle("Unit Retirements by Year")
    ggplotly(plot1)
   })
  
  output$note <- renderText({"Note: There are no retired units in programs CSOSG1, CSOSG2, CSSO2G2, OTC, or TXSO2 as of 3/17/2020."})
  
  # # Removing the filters for the facility number will provide table of an entire program's accounts that received allocations
  output$table_output <- DT::renderDataTable({
    prg <- prg_code()
    prg_code <- prg$prg_code
    if (is.null(prg_code)){
      return(NULL)}
    table1 <- retire_output()
  }, rownames = FALSE, class = "compact", filter = 'top', options = list(scrollX = TRUE, pageLength = 5))
  
  output$download_data <- downloadHandler(
    filename = "retired_units.csv",
    content = function(file) {
      write.csv(retire_output(), file, row.names = FALSE)
    }
    
  )

}

