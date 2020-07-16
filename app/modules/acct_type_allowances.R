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

### This code sums the allowance totals by account types for a given compliance year
acct_types_allowances_UI <- function(id){
  ns <- NS(id)
  tabBox(title = "Account Type Allowances", id = "acct_types_allowances", height ="100%",width ="100%",
         tabPanel(
           program_code_UI(ns("program_code")),
           uiOutput(ns("comp_year_output")),
           plotlyOutput(ns("plot_output")), 
           fluidRow(DT::dataTableOutput(ns("table_output")))
         )
  )
}
acct_types_allowances <- function(input, output, session, prg_code) {
  prg_code <- callModule(program_code,"program_code", multiple_codes=FALSE)
  ns <- session$ns
  account_types_data <- reactive({
    prg <- prg_code()
    prg_code <- prg$prg_code
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    # Here the first possible transaction date and last possible transaction date (based on compliance year) are retrieved
    first_trans_date <- as_date("1993/01/01")
    last_trans_date <- get_comp_year(input$comp_year, prg_code)
    last_trans_date <- format(as_date(last_trans_date), "%Y/%m/%d")
    print(last_trans_date)
    prg_code_sql <- paste("\'", prg_code, "\'", sep = "")
    print(paste("Calculating", input$comp_year, "allowance total", sep = " "))
    acct_types = c('State Holding Account', 'Unit Account', 'Facility Account', 'General Account', 'Overdraft Account')
    transactions <- channel %>%
      dbSendQuery(paste("SELECT a.transaction_id, transaction_date, transaction_total, transaction_block_id, 
                        vintage_year, total_block, sell_account_type, buy_account_type 
                        FROM transaction_fact a, transaction_block_dim b 
                        WHERE a.prg_code = b.prg_code AND a.prg_code= ", prg_code_sql,
                        "AND a.transaction_id = b.transaction_id
                        AND transaction_date >= TO_DATE(\'", first_trans_date, "\', 'yyyy/mm/dd') 
                        AND transaction_date <= TO_DATE(\'", last_trans_date,"\','yyyy-mm-dd')", sep = "")) %>% dbFetch()
    print(1.1)
    buy_trans <- transactions %>%
      group_by(buy_account_type, vintage_year) %>%
      mutate(total_bought = sum(total_block, na.rm = TRUE)) %>%
      distinct(buy_account_type, vintage_year, total_bought) %>%
      filter(buy_account_type %in% acct_types)
    print(1.2)
    sell_trans <- transactions %>%
      group_by(sell_account_type, vintage_year) %>%
      mutate(total_sold = sum(total_block, na.rm = TRUE)) %>%
      distinct(sell_account_type, vintage_year, total_sold) %>%
      filter(sell_account_type %in% acct_types)
    print(1.3)
    final <- buy_trans %>%
      full_join(sell_trans, by = c('buy_account_type' = 'sell_account_type', 'vintage_year' = 'vintage_year')) %>%
      mutate(total_sold = replace(total_sold, is.na(total_sold), 0)) %>%
      mutate(total_held = total_bought - total_sold) 
    print(1.4)
    final
    # print(final)
    # Total <- sum(final$total_held)
  })
  
  output$plot_output <- renderPlotly({
    prg <- prg_code()
    prg_code <- prg$prg_code
    if (is.null(prg_code) || is.null(input$comp_year)) {
      return(NULL)}
    account_types_grouped <- account_types_data()
   
    acct_types_plot <- account_types_grouped %>%
      ggplot(aes(x = account_types_grouped$vintage_year, y = account_types_grouped$total_held, fill = account_types_grouped$buy_account_type)) +
      geom_bar(stat = "identity") + scale_y_continuous(labels = comma) +
      ggtitle(paste("Allowances", prg_code, sep = ' ')) +
      theme(axis.text.x = element_text(angle = 315, hjust = 1))
    ggplotly(acct_types_plot)
  })
  
  output$table_output <- DT::renderDataTable({
    prg <- prg_code()
    prg_code <- prg$prg_code
    if (is.null(prg_code) || is.null(input$comp_year)){
      return(NULL)}
    table1 <- account_types_data()
  }, rownames = FALSE, class = "compact", filter = 'top', options = list(scrollX = TRUE, pageLength = 5, order = list(list(0, 'asc'))))
  
  output$comp_year_output <- renderUI({
    prg <- prg_code()
    prg_code <- prg$prg_code
    if (is.null(prg_code)) {
      return(NULL)
    }
    channel <-connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    possibilities <-
      channel %>% tbl("account_compliance_dim") %>%
      filter(prg_code == prg_code) %>%
      distinct(op_year) %>%
      collect()
    selectizeInput(inputId = ns("comp_year"), label = "Enter an Compliance Year", choices = sort(possibilities$op_year), 
                   multiple = TRUE, selected = NULL, options = list(maxOptions = 100, maxItems = 1, placeholder = 'Enter a Number...'))
  })
}