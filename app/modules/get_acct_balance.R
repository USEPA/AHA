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
source("modules/get_comp_year.R")

get_acct_balance <- function (compliance_year, prg_code, acct_numb){
  channel <- connect_to_db()
  on.exit(dbDisconnect(channel), add = TRUE)
  
  if(compliance_year < 1995) { Total <- 0} else{
  first_trans_date <- as_date("1993/01/01")
  last_trans_date <- get_comp_year(compliance_year, prg_code)
  last_trans_date <- format(as_date(last_trans_date), "%Y/%m/%d")
  print(last_trans_date)
  prg_code_sql <- paste("\'", prg_code, "\'", sep = "")
  acct_numb_sql <- paste("\'", acct_numb, "\'", sep = "")
  
  print(paste("Calculating", compliance_year, "allowance total", sep = " "))
 
  transactions <- channel %>%
    dbSendQuery(paste("SELECT a.transaction_id, transaction_date, transaction_total, transaction_block_id,
                        vintage_year, total_block, sell_account_type, sell_acct_number, buy_account_type, buy_acct_number
                        FROM transaction_fact a, transaction_block_dim b
                        WHERE a.prg_code = b.prg_code AND a.prg_code= ", prg_code_sql,
                        "AND a.transaction_id = b.transaction_id AND (buy_acct_number = ", acct_numb_sql," OR sell_acct_number = ", acct_numb_sql,")
                        AND transaction_date >= TO_DATE(\'", first_trans_date, "\', 'yyyy/mm/dd')
                        AND transaction_date <= TO_DATE(\'", last_trans_date,"\','yyyy-mm-dd')", sep = "")) %>% dbFetch()
  # transactions <- channel %>% tbl("transaction_fact")%>%
  #   filter( transaction_date <=lubridate::ymd(last_trans_date ), prg_code==program_code )%>% 
  #   filter(buy_acct_number == acct_numb | sell_acct_number==acct_numb)%>%collect()
  # transaction_blocks <- channel %>% tbl("transaction_block_dim")%>%
  #   filter(prg_code==program_code) %>%collect()
  # joined_transactions <-transactions%>%
  #   inner_join(transaction_blocks, by = c("transaction_id", "prg_code"))
  print(1.1)
  buy_trans <- transactions %>%
    filter(buy_acct_number == acct_numb) %>%
    group_by(vintage_year) %>%
    mutate(total_bought = sum(total_block, na.rm = TRUE)) %>%
    distinct(buy_acct_number, vintage_year, total_bought)
  print(1.2)
  sell_trans <- transactions %>%
    filter(sell_acct_number == acct_numb) %>%
    group_by(vintage_year) %>%
    mutate(total_sold = sum(total_block, na.rm = TRUE)) %>%
    distinct(sell_acct_number, vintage_year, total_sold)
  print(1.3)
  final <- buy_trans %>%
    left_join(sell_trans, by = c('buy_acct_number' = 'sell_acct_number', 'vintage_year' = 'vintage_year')) %>%
    mutate(total_sold = replace(total_sold, is.na(total_sold), 0)) %>%
    mutate(total_held = total_bought - total_sold)
  print(1.4)
  Total <- sum(final$total_held)
  print(paste(compliance_year, " Balance: ", Total, sep = ""))
  Total}
}