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
## This code displays the allocations by type and vintage for a program both graphically and in a table.

program_allocations_UI <- function(id){
  ns <- NS(id)
  tabBox(title = "Program Allocations", id = "program_allocations", height ="100%",width ="100%",
         tabPanel(
            program_code_UI(ns("program_code")),
            plotlyOutput(ns("plot_output")), 
            fluidRow(DT::dataTableOutput(ns("table_output")))
         )
  )
}
program_allocations <-function(input, output, session, prg_code) {
  prg_code <- callModule(program_code,"program_code", multiple_codes=FALSE)
  ns <- session$ns
  program_allocation_output <- reactive({
    prg_in <- prg_code()
    prg_code <- prg_in$prg_code
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    
    trans_blocks <- channel %>%
      tbl("transaction_block_dim")
    trans_blocks
    # Commented out have no transactions recorded in database
    trans_types <- c('Initial Allocation','New Unit Set Aside Allocation', 
                     #'Conservation Issuance',
                     'Distribute Allowances for New Program',
                     'Distribute Revintaged Allowances',
                     'Early Reduction Allocation',
                     'Early Reduction Issuance',
                     #'Energy Biomass Issuance',
                     #'Energy Geothermal Issuance',
                     #'Energy Solar Issuance', 
                     #'Energy Wind Issuance', 
                     #'July Confirmation Credit',
                     'Opt-In Allocation',
                     'Other Reserve Allocation',
                     'Phase 1 Extension Issuance',
                     'Purchase at EPA Auction')
    
    #filtered out EPA Reserve accounts because for 2035 vintage an later an inital 8,700,000 allowance allocation is made from one reserve account to another
    #additionally, a number of initial allocations involved transfer from a resreve account to a surrender account, which also were excluded
    transactions <- channel %>%
      tbl("transaction_fact") %>%
      filter(prg_code == !!prg_code & transaction_type %in% trans_types 
             & buy_account_type != 'EPA Reserve Account' & buy_account_type != 'Surrender Account')
    transactions
    
    # At this point, pulls all Initial and NUSA allocations (which is split by vintage year since those allocations happen separately)
    # existing in system based on the inputted program. The join shows this by joining trans blocks to the transactions for vintage idenfitication
    join1 <- transactions %>%
      left_join(trans_blocks, by = c("transaction_id" = "transaction_id", "prg_code" = "prg_code")) %>%
      select(transaction_type,
             buy_acct_name,
             buy_acct_number,
             total_block,
             vintage_year,
             prg_code)
    join1
    
    # At this point, groups by the following variables to sum up the total allocations for a given vintage and type and removes duplicates
    join2 <- join1 %>%
      group_by(transaction_type, vintage_year) %>%
      mutate(allocation_total = sum(total_block)) %>% 
      distinct(transaction_type, vintage_year, allocation_total) %>%
      collect()
  })
  
  output$plot_output <- renderPlotly({
    prg_in <- prg_code()
    prg_code <- prg_in$prg_code
    if (is.null(prg_code)) {
      return(NULL)}
    if(length(prg_code) > 1) {validate("Please select only one program")} else{
    allocations <- program_allocation_output()
    plot1 <- allocations %>%
      ggplot(aes(x = vintage_year, y = allocation_total, fill = transaction_type)) + 
      geom_bar(stat = "identity", position = "stack") + scale_y_continuous(labels = comma) +
      ggtitle("Allocation Amounts")
    ggplotly(plot1)}
  })
  
  # Removing the filters for the facility number will provide table of an entire program's allocations by vintage
  output$table_output <- DT::renderDataTable({
    prg_in <- prg_code()
    prg_code <- prg_in$prg_code
    if (is.null(prg_code)){
      return(NULL)}
    table1 <- program_allocation_output()
    table1
  }, rownames = FALSE, class = "compact", filter = 'top', options = list(scrollX = TRUE, pageLength = 5))
}

