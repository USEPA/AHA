library(RPostgres)
library(pool)
library(config)

config <- config::get()
connect_to_db <- function(){
  con <- dbConnect(RPostgres::Postgres(),dbname = config$dbname, 
                   host = config$host,
                   port = config$port, 
                   user = config$user,
                   password = config$password)
  con
}