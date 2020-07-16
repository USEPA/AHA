library(igraph)
library(tidyverse)
library(shiny)
library(lubridate)
library(stringi)
library(data.table)
source("global.R")
source("modules/programCode.R")
transaction_summary_UI <- function(id){
 
  ns <- NS(id)
    tabBox(title="Transaction Summary", id="trans_summary",
      height ="100%",width ="100%",
      tabPanel(
        h1('Table of Distinct Transactions'),
        program_code_UI(ns("program_code")),
        dateRangeInput(ns("transaction_dates"), "Transaction Dates:", start = "2018-1-1", end =Sys.Date()),
        fluidRow(DT::dataTableOutput(ns("table_output"))),
        downloadButton(ns("download_data"),"Download Data")  
  )
    )

  
  
}


transaction_summary <- function(input, output, session){
  ns <- session$ns
  prg_code <- callModule(program_code,"program_code")
  prg_code <- debounce(prg_code, 3000)
  private_transactions <- reactive({
    prg_in <- prg_code()
    prg_code <- prg_in$prg_code
    if (is.null(prg_code)) {
      return(NULL)
    }
    cat(file=stderr(), "dates:", paste(as.character(input$transaction_dates), collapse = " to "))
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    private_transactions <- channel%>%
      tbl("transaction_fact")%>%
      filter(transaction_type_code %in% c("PRIVATE", "ERREV"),
              buy_account_type %in% c("Facility Account", "General Account"),
              sell_account_type %in% c("Facility Account", "General Account"),
              transaction_date >= to_date( !!input$transaction_dates[1],'YYYY-MM-DD'),
             transaction_date <= to_date(!!input$transaction_dates[2],'YYYY-MM-DD'),#Arbitrary Date, for performance
              prg_code %in% !! prg_code
      )%>%collect()
  })
  #getting company network
  nodes_grouped <- get_nodes_grouped()
  
  classified_transactions <- reactive({    
    prg_in <- prg_code()
    prg_code <- prg_in$prg_code
    if (is.null(prg_code)) {
      return(NULL)
    }
    split_trans <-private_transactions()%>%
      select(transaction_id,prg_code, transaction_date, transaction_total, transaction_type_code, buy_acct_number, 
             buy_display_name, buy_own_display_name, sell_acct_number, sell_display_name, sell_own_display_name)%>%
      mutate( sell_own_display_name=str_split(sell_own_display_name, "<br>"), 
              buy_own_display_name = str_split(buy_own_display_name, "<br>"))
     unnested_trans <- split_trans%>% unnest(sell_own_display_name)%>% unnest(buy_own_display_name)
    joined_trans <- unnested_trans %>%
      left_join(select(nodes_grouped,company, company_list), by=c("sell_own_display_name"="company"))%>%
      rename(sell_own_companies=company_list)%>%mutate(connected_companies = map2(buy_own_display_name, sell_own_companies, ~.x %in%.y))
    transactions <- make_transaction_table(prg_code, joined_trans)
    transactions
  })
  

  output$table_output <- DT::renderDataTable({
    prg_in <- prg_code()
    prg_code <- prg_in$prg_code
    if (is.null(prg_code)) {
      return(NULL)
    }
    
     classified_transactions()
  }, rownames = FALSE, class = "compact", filter = 'top', options = list(scrollX = TRUE, pageLength = 5))
  
  output$download_data <- downloadHandler(
    filename = "transaction_summary.csv",
    content = function(file) {
      write.csv(classified_transactions(), file, row.names = FALSE)
    }
    
  )
}

#here are some helper functions
calculate_transactions <- function(prg_code, transaction_type_code, transaction_table){
  transaction_summary <- transaction_table%>% filter(prg_code == prg_code)%>%filter(transaction_type_code == transaction_type_code)%>%group_by(transaction_id,prg_code, transaction_date,buy_acct_number, transaction_total)%>% summarise(are_connected = buy_own_display_name %in% sell_own_display_name || sum(unlist(connected_companies))>0)
  related_trans <- transaction_summary%>% filter(are_connected ==TRUE)
  unrelated_trans <- transaction_summary %>% filter(are_connected==FALSE)
  related_trans_number <- nrow(related_trans)
  unrelated_trans_number <- nrow(unrelated_trans)
  total_trans_number <- nrow(transaction_summary)
  related_trans_volume <- sum(related_trans$transaction_total)
  unrelated_trans_volume <- sum(unrelated_trans$transaction_total)
  total_trans_volume <- sum(transaction_summary$transaction_total)
  prg_frame <- data.frame(prg_code, total_trans_number,  related_trans_number, unrelated_trans_number, total_trans_volume,related_trans_volume, unrelated_trans_volume)
  return(prg_frame)
}
  
  make_transaction_table <- function(prg_codes, transaction_table){
    transactions_errrev <- rbindlist(map(prg_codes,~calculate_transactions(..1,transaction_type_code ="ERREV", transaction_table=transaction_table)))
    transactions_private <- rbindlist(map(prg_codes,~calculate_transactions(..1,transaction_type_code ="PRIVATE", transaction_table=transaction_table)))
    transactions_final <- transactions_private
    transactions_final$total_trans_number <- transactions_final$total_trans_number - transactions_errrev$total_trans_number
    transactions_final$related_trans_number <- transactions_final$related_trans_number - transactions_errrev$related_trans_number
    transactions_final$unrelated_trans_number <- transactions_final$unrelated_trans_number- transactions_errrev$unrelated_trans_number
    transactions_final$related_trans_volume <-transactions_final$related_trans_volume - transactions_errrev$related_trans_volume
    transactions_final$unrelated_trans_volume<-transactions_final$unrelated_trans_volume-transactions_errrev$unrelated_trans_volume
    transactions_final$total_trans_volume <-transactions_final$total_trans_volume - transactions_errrev$total_trans_volume
    transactions_final <-transactions_final %>% mutate(
      related_trans_number_percent = related_trans_number / (related_trans_number + unrelated_trans_number),
      unrelated_trans_number_percent = unrelated_trans_number /(related_trans_number + unrelated_trans_number),
      related_trans_volume_percent = related_trans_volume / (related_trans_volume + unrelated_trans_volume),
      unrelated_trans_volume_percent = unrelated_trans_volume / (related_trans_volume + unrelated_trans_volume)
    )
    return(transactions_private)
  }


#this function returns the nodes that are connected to a given graph as id numbers
#using breadth first search algorithm
do_bfs <- function(my_graph, root_id){
  results <-bfs(my_graph, root=root_id, "all", unreachable=FALSE)
  connections <- (as_ids(results$order))
  connections <- as.numeric(connections[!is.na(connections)])
  return(connections)
  
}

# #this function gets ids from do_bfs and returns the company name as a list
connected_companies <- function(company_name, nodes, my_graph){
  node_id <- nodes %>% filter(company==company_name)
  node_id <- node_id$id
  linked_nodes <- do_bfs(my_graph, node_id)
  found_nodes <- nodes %>% filter(id %in% linked_nodes)
  return(as.character(found_nodes$company))
}

get_company_network <- function(){
  companies_with_holdings <- read_csv("CSV_files/company_w_holdings.csv")
  company_names <- unique(c(companies_with_holdings$company_name, companies_with_holdings$holding_company_name))%>% stri_encode("", "UTF-8")
  nodes <- data.frame(id = seq(1,length(company_names)), company=as.character(company_names))
  edges <- companies_with_holdings %>% select(company_name, holding_company_name) %>% filter(is.na(holding_company_name)==FALSE ) %>%left_join(nodes, by=c('company_name'='company')) %>% rename(holding= id)
  edges <- edges %>% left_join(nodes, by=c('holding_company_name'='company'))%>% rename(holder=id) %>% filter(holding != holder) %>% select(holding, holder, company_name, holding_company_name)
  my_graph <- graph_from_data_frame(edges, vertices=nodes)
  list_return <- list("my_graph"=my_graph, "nodes"=nodes)
  list_return
}
get_nodes_grouped <-function(){
  company_network <- get_company_network()
  nodes_grouped <- company_network$nodes %>% 
  filter(!is.na(company))%>%
  mutate(company_list = map(company,~connected_companies(.x, company_network$nodes, company_network$my_graph)))%>%
  mutate_if(is.factor, as.character)
  nodes_grouped
}
