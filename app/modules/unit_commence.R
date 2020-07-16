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

unit_commence_UI <- function(id){
  ns <- NS(id)
  tabBox(
    title = "Unit Commence Dates",
    id = "unit_commence",
    height = "100%",
    width = "100%",
    tabPanel(
      #verbatimTextOutput(ns("note")),
      program_code_UI(ns("program_code")),
      plotlyOutput(ns("plot_output")),
      fluidRow(DT::dataTableOutput(ns("table_output"))),
      downloadButton(ns("download_data"),"Download Data")  
    )
  )
}

unit_commence <-function(input, output, session) {
  ns <-session$ns
  prg_code <- callModule(program_code,"program_code", multiple_codes=FALSE)
  unit_start <- reactive({
    prg <- prg_code()
    prg_code <- prg$prg_code
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    
    filter <- paste('%', prg_code, '%', sep ="")
    filter2 <- paste('%', prg_code, ',%', sep ="")
    
    # Gets unique untit_ids by facility and each units commence operation date
    # Gets earliest possible date, checks if commercial date or commence date is earlier and picks the earlier one as final_date
    # Also replaces any null date with the other date
    unit_fact <- channel %>%
      tbl("unit_fact") %>%
      filter(prg_code_info %like% filter | prg_code_info %like% filter2) %>%
      mutate(commence_date = if_else(is.null(comm_op_date), comr_op_date, comm_op_date),
             commerce_date = if_else(is.null(comr_op_date), comm_op_date, comr_op_date),
             final_date = if_else(commence_date < commerce_date, commence_date, commerce_date)) %>%
      group_by(unit_id, facility_name, final_date, primary_fuel_info) %>%
      distinct(unit_id, facility_name, final_date, primary_fuel_info) %>%
      collect() %>%
      mutate(year = as.integer(substr(as.character(final_date), 0, 4))) %>% 
      collect()
  })

  output$plot_output <- renderPlotly({
    prg <- prg_code()
    prg_code <- prg$prg_code
    if (is.null(prg_code)) {
       return(NULL)}
    units <- unit_start()
    units2 <- units %>%
      group_by(year, primary_fuel_info) %>% ### Remove Primary Fuel Info from here and fill = below in plot to remove fuel type classifications
      summarise(total_units = n())

    print(units2)
    plot1 <- units2 %>%
      ggplot(aes(x = year, y = total_units, fill = primary_fuel_info)) +
      geom_bar(stat = "identity", position = "stack") +
      ggtitle("Unit Operation Starts") + theme(axis.text.x = element_text(angle = 0, hjust = 1)) + scale_x_continuous(breaks = c(1900, 1925, 1950, 1975, 2000, 2025))
    ggplotly(plot1)
   })

  #output$note <- renderText({"Note: BLANK"})

  # # Removing the filters for the facility number will provide table of an entire program's accounts that received allocations
  output$table_output <- DT::renderDataTable({
    prg <- prg_code()
    prg_code <- prg$prg_code
    if (is.null(prg_code)){
      return(NULL)}
    table1 <- unit_start()
  }, rownames = FALSE, class = "compact", filter = 'top', options = list(scrollX = TRUE, pageLength = 5)
  )
  
  output$download_data <- downloadHandler(
    filename = "downloadtest.csv",
    content = function(file) {
      write.csv(unit_start(), file)
    }
    
  )
  
}


