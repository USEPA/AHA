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
library(scales)
source("global.R")
source("modules/get_acct_balance.R")
#### NOTE: This does not work for general accounts in OTC since they do not "exist" in the database in account_fact where this code pulls
#### programs an account belongs to. Logic could be included to choose NBP accounts as OTC accounts if needed since they are essentially the same
accounts_UI <- function(id){
  ns <- NS(id)
  tabBox(title = "Accounts", id = "accounts", height ="100%",width ="100%",
         tabPanel(
           uiOutput(ns("account_numb_output")),
           uiOutput(ns("prg_code_output")),
           #uiOutput(ns("comp_year_output")),
           #program_code_UI(ns("program_code")),
           plotlyOutput(ns("balance_plot")),
           fluidRow(DT::dataTableOutput(ns("balance_table")))
         )
  )
}
accounts <- function(input, output, session, prg_code){
  #prg_code <- callModule(program_code,"program_code")
  ns <- session$ns
  # this creates a dataframe with the program years applicable to the inputted account 
  program_year_df <- reactive ({
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    
    year_df<- channel %>%
      tbl("program_year_dim") %>%
      filter(prg_code == !!input$prog_code) %>%
      distinct(op_year) %>% 
      collect() %>% 
      as.data.frame() %>%
      mutate(prg_code = input$prog_code,
             acct_numb = input$account_numb)
    year_df
  })
  # This gets the dataframe with the compliance years and calculates the total account balance for each year
  # putting them into a list that is then joined to the dataframe as the column 'balance'
  get_yearly_bal <- reactive({
    year_df <- program_year_df()
    numb <- 0
    balance_list <- integer()
    for (each in year_df$op_year){
      numb <- numb + 1
      bal <- get_acct_balance(year_df$op_year[numb], year_df$prg_code[numb], year_df$acct_numb[numb])
      balance_list <- c(balance_list, bal)
    }
    year_df$balance <- balance_list
    year_df <- year_df %>%
      select(compliance_year = op_year, balance)

    year_df
  })
  ############################################### Tables Section ########################################################
  output$balance_table <- DT::renderDataTable({
    if(is.null(input$account_numb) || is.null(input$prog_code)){return(NULL)}
    table1 <- get_yearly_bal()
  }, filter = 'top', rownames = FALSE, class = 'compact', options = list(scrollX = TRUE, pageLength = 5, order = list(list(0, 'asc'))))
  ############################################### Plots Section ########################################################
  output$balance_plot <- renderPlotly({
    if(is.null(input$account_numb) || is.null(input$prog_code)){return(NULL)}
    balance_plot <- get_yearly_bal() %>%
      ggplot(aes(y= balance, x = compliance_year)) + geom_bar(stat = "identity") + ggtitle("Account Balance (All Vintages)")
    ggplotly(balance_plot)
  })
############################################### Inputs Section ########################################################
  output$account_numb_output <- renderUI({
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    possibilities <- channel %>%
      tbl("account_fact") %>%
      distinct(account_number) %>%
      collect()
    selectizeInput(inputId = ns("account_numb"), label = "Enter an Account Number", choices = sort(possibilities$account_number), 
                   multiple = TRUE, selected = NULL, options = list(maxOptions = 5, maxItems = 1, placeholder = 'Enter a Number...'))
  })
  output$prg_code_output <- renderUI({
    if(is.null(input$account_numb)){return(NULL)}
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    possibilities <- channel %>% tbl("account_fact")%>%
      filter(account_number == !!input$account_numb) %>%
      distinct(prg_code) %>%
      collect()
    selectInput(inputId = ns("prog_code"), label = "Select Program Type", choices = sort(possibilities$prg_code), multiple = FALSE)
  })
}