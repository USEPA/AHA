library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(odbc)
library(DBI)
library(dbplyr)
library(scales)
source("global.R")

account_types_UI <- function(id){
  ns <- NS(id)
  tabBox(title = "Account Types", id = "account_types", height ="100%",width ="100%",
         tabPanel(
           program_code_UI(ns("program_code")),
           plotlyOutput(ns("plot_output")), 
           fluidRow(DT::dataTableOutput(ns("table_output")))
         )
  )
}
account_types <- function(input, output, session, prg_code) {
  prg_code <- callModule(program_code,"program_code", multiple_codes=FALSE)
  ns <- session$ns
  account_types_data <- reactive({
    prg_in <- prg_code()
    prg_code <- prg_in$prg_code
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    
    accounts <- channel %>%
      tbl("account_fact") %>%
      filter(prg_code == prg_code) %>%
      group_by(account_type) %>%
      summarise(number_of_accounts = n()) %>%
      select(account_type, number_of_accounts) %>%
      collect()
  })
  
  output$plot_output <- renderPlotly({
    prg <- prg_code()
    prg_code <- prg$prg_code
    if (is.null(prg_code)) {
      return(NULL)}
    account_types_grouped <- account_types_data()
    acct_types_plot <- account_types_grouped %>%
      ggplot(aes(x = account_type, y = number_of_accounts)) + 
      geom_bar(stat = "identity") + scale_y_continuous(labels = comma) +
      ggtitle(paste("Account Type Composition for", prg_code, sep = ' ')) +
      theme(axis.text.x = element_text(angle = 315, hjust = 1))
    ggplotly(acct_types_plot)
  })
  
  output$table_output <- DT::renderDataTable({
    prg <- prg_code()
    prg_code <- prg$prg_code
    if (is.null(prg_code)){
      return(NULL)}
    table1 <- account_types_data()
  }, rownames = FALSE, class = "compact", filter = 'top', options = list(scrollX = TRUE, pageLength = 5, order = list(list(0, 'asc'))))
}