library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(odbc)
library(DBI)
library(lubridate)
library(hash)
library(dbplyr)
library(networkD3)
source("global.R")
source("modules/programCode.R")
allowance_transfers_UI <- function(id){
  ns <- NS(id)
  box(
          program_code_UI(ns("program_code")),
          uiOutput(ns("transfer_date_output")),
          uiOutput(ns("transaction_type_output")),
          forceNetworkOutput(ns("force_grace_output")),
          hr(),
          hr(),
          plotlyOutput(ns("transfers_output")),
          hr(),
          fluidRow(DT::dataTableOutput(ns("transfer_table_output"))),
          downloadButton(ns("downloadData"),"Download Data")
         )

  
}

allowance_transfers<- function(input, output, session){
  ns <- session$ns
  prg_code <- callModule(program_code,"program_code")
  output$transfer_date_output <- renderUI({
    prg_in <- prg_code()
    prg_code <- prg_in$prg_code
    if (is.null(prg_code)) {
      return(NULL)
    }
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    
    possibilities <-
      channel %>% tbl("transaction_fact") %>%
      filter(prg_code %in% !! prg_code) %>%
      distinct(transaction_date) %>% collect()
    dateRangeInput(
      ns("transaction_date_input"),
      "Transaction Date",
      start = min(as_date(possibilities$transaction_date, tz =
                            NULL)),
      end = max(as_date(possibilities$transaction_date, tz =
                          NULL))
      
    )
    
  })
  output$transaction_type_output <- renderUI({
    prg_in <- prg_code()
    prg_code <- prg_in$prg_code
    if (is.null(prg_code)) {
      return(NULL)
    }
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    possibilities <-
      channel %>% tbl("transaction_fact") %>%
      filter(prg_code %in% !!prg_code) %>%
      distinct(transaction_type) %>% collect()
    pickerInput(
      inputId = ns("transaction_type_input"),
      label = "Transaction Type",
      choices = sort(possibilities$transaction_type),
      options = list('actions-box' = TRUE),
      multiple = TRUE
    )
  })
  table_filter <- reactive({
    filtered_transactions <- filtered_trans()
    table_data <- input$transfer_table_output_rows_all
    if (length(table_data) > 0 &&length(table_data) < nrow(filtered_transactions)){
      new_filtered <- filtered_transactions[table_data, , drop=FALSE]}
    else {
      new_filtered<-filtered_transactions
    }
    new_filtered
  })
  
  output$transfers_output <- renderPlotly({
    prg_in <- prg_code()
    prg_code <- prg_in$prg_code
    if (is.null(input$transaction_date_input) ||
        is.null(prg_code) ||
        is.null(input$transaction_type_input)) {
      return(NULL)
    }
    filtered_transactions <- table_filter()
    thePlot <-
      filtered_transactions %>%ggplot(
        aes(
          x = transaction_date,
          y = transaction_total,
          color = transaction_type,
          label1 = buy_acct_name,
          label2 = buy_acct_number,
          label3 = sell_acct_name,
          label4 = sell_acct_number
        )
      ) + ggtitle("transactions") + geom_point()
    ggplotly(thePlot)
    
  })
  
  filtered_trans <- reactive({
    prg_in <- prg_code()
    prg_code <- prg_in$prg_code
    if (is.null(input$transaction_date_input) ||
        is.null(prg_code) ||
        is.null(input$transaction_type_input)) {
      return(NULL)
    }
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    table <-
      channel %>% tbl("transaction_fact") %>%
      filter(prg_code %in% !! prg_code) %>% filter(transaction_type %in% !!input$transaction_type_input) %>% collect()
    transactions <-
      table %>% mutate(trans_date = as_date(transaction_date)) %>%
      filter(
        trans_date >= !!input$transaction_date_input[1],
        trans_date <= !!input$transaction_date_input[2]
      ) 
    transactions
  })
  output$force_grace_output <- renderForceNetwork({
    if (is.null(input$transaction_date_input) ||
        is.null(prg_code) ||
        is.null(input$transaction_type_input)) {
      return(NULL)}
    table <- table_filter()
    buy_accounts <-
      table %>% select(buy_acct_number, buy_display_name, buy_own_display_name)
    sell_accounts <-
      table %>% select(sell_acct_number, sell_display_name, sell_own_display_name)
    account_join <-
      buy_accounts %>% full_join(
        sell_accounts,
        by = c(
          "buy_acct_number" = "sell_acct_number",
          "buy_display_name" = "sell_display_name",
          "buy_own_display_name" = "sell_own_display_name"
        )
      ) %>%
      distinct(buy_acct_number, .keep_all = TRUE)
    account_nodes <-
      mutate(account_join, id = as.numeric(rownames(account_join)) - 1)
    account_edges <-
      table %>% distinct(transaction_id, .keep_all = TRUE) %>% group_by(buy_acct_number, sell_acct_number) %>% summarize(total_transfers = sum(transaction_total)) %>%
      left_join(
        select(account_nodes, "buy_acct_number", 'id'),
        by = c("buy_acct_number" = "buy_acct_number")
      ) %>%
      rename(buyer = id) %>%
      left_join(
        select(account_nodes, "buy_acct_number", 'id'),
        by = c("sell_acct_number" = "buy_acct_number")
      ) %>%
      rename(seller = id)
    fn<-forceNetwork(
      Links = account_edges,
      Nodes = account_nodes,
      NodeID = 'buy_acct_number',
      Source = "buyer",
      Target = "seller",
      Group = 'buy_own_display_name',
      zoom = TRUE,
      opacityNoHover = 1,
      opacity = 1,
      clickAction = 'Shiny.onInputChange("AccountNumber", d.name)'
    )
    fn
  })
  
  output$transfer_table_output <- DT::renderDataTable({
    if (is.null(input$transaction_date_input) ||
        is.null(prg_code) ||
        is.null(input$transaction_type_input)) {
      return(NULL)
    }
    filtered_trans()
  }, rownames = FALSE, class = "compact", filter = 'top',
  options = list(scrollX = TRUE)
  )
  
  output$downloadData <- downloadHandler(
    filename = "allowance_transfer.csv",
    content = function(file) {
      write.csv(filtered_trans(), file, row.names = FALSE)
    }
  
  )
}

