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
library(sf)
source("global.R")
source("modules/programCode.R")
compliance_map_UI <- function(id){
  ns <- NS(id)
  tabBox(
    title = "Compliance Map",
    id = "compliance_map",
    height = "100%",
    width = "100%",
    tabPanel(
       program_code_UI(ns("program_code")),
       uiOutput(ns("comp_year_output")),
      uiOutput(ns("map_type_output")),
      verbatimTextOutput(ns("ratio_example")),
      plotOutput(ns("emiss_allow_plot")), 
      fluidRow(DT::dataTableOutput(ns("map_table_output"))),
      downloadButton(ns("download_data"),"Download Data") 
  )
)
  
}



compliance_map <- function(input, output, session) {
  ns <- session$ns
  prg_code <- callModule(program_code,"program_code", multiple_codes=FALSE)
  #This part for mapping State Allowances vs State Emissions
  emiss_alllow_output <- reactive({
    prg <- prg_code()
    prg_code <- prg$prg_code
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    #This collects the state allowance budgets table from file
    #sourced from the AMPD query 'State Allowance Budget By Program'
    state_allocations = read_csv("CSV_files/State_Allowance_Budget_by_Program.csv", col_names = TRUE)
    
    not_ozone <- c("CAIRSO2", "CAIRNOX", "CSSO2G1" , "CSSO2G2" , "CSNOX", "ARP")
    nox_annual <- c("CAIRNOX", "CSNOX")
    so2_annual_ <- c("CAIRSO2", "CSSO2G1" , "CSSO2G2" , "ARP")
    ozone <- c("OTC", "NBP", "CAIROS", "CSOSG1", "CSOSG2")
    
    #This retrieves the annual emission data by unit for SO2 and NOX or the Ozone season emission data
    if (prg_code %in% not_ozone) {
      unit_emissions <- channel %>%
        tbl("annual_unit_data") %>%
        select(unit_id, op_year, so2_mass, nox_mass) } 
    else{ 
      unit_emissions <- channel %>% 
        tbl("ozone_unit_data") %>%
        select(unit_id, op_year, so2_mass, nox_mass )}
    
    #This retrieves the unit information for units from 1999 and after for all programs except ARP since ARP doesn't have state level budgets
    units_info <- channel %>%
      tbl("unit_fact") %>%
      select(unit_id, op_year, state_name, state, prg_code_info) %>%
      filter(op_year >= 1999) %>%
      filter(prg_code_info %like% "%CAIR%" | prg_code_info %like% "%CS%" | prg_code_info %like% "%NBP%" | prg_code_info %like% "%OTC%")
    
    #This joins the unit information with the emissions data and groups the units
    #together by program (OP) year and state to determine the emissions per state,
    #per program, per year
    state_emissions <- units_info %>%
      left_join(unit_emissions,by = c("op_year" = "op_year", "unit_id" = "unit_id")) %>%
      group_by(op_year, state_name) %>%
      mutate(nox_emiss = sum(nox_mass), SO2_annual = sum(so2_mass)) %>%
      distinct(op_year, state_name, state, SO2_annual, nox_emiss) %>%
      collect() 
    
    emiss_allow <- state_emissions %>%
      left_join(state_allocations, by = c("state" = "state", "op_year" = "program_year")) %>%
      filter(program_code == prg_code, op_year == input$comp_year) %>%
      mutate(allow_emiss_ratio = if_else((program_code %in% so2_annual_), 
                                         ALLOW_BUDGET/round(SO2_annual),
                                         ALLOW_BUDGET/round(nox_emiss)),
             excess_allow = if_else((program_code %in% so2_annual_), 
                                    ALLOW_BUDGET - round(SO2_annual), 
                                    ALLOW_BUDGET - round(nox_emiss)), 
             percent_excess = excess_allow/ALLOW_BUDGET*100)
    
    # This chooses which emissions value (SO2 or NOX) to display in table and the labeling (annual or ozone) based
    # on program (i.e. SO2 = annual, NOX = annual OR ozone)
    if (prg_code %in% so2_annual_) {
      emiss_allow_2 <- emiss_allow %>%
        select(state_name, SO2_annual, ALLOW_BUDGET, allow_emiss_ratio, excess_allow, percent_excess) %>%
      collect()
    } else if(prg_code %in% nox_annual) {
      emiss_allow_2 <- emiss_allow %>%
        select(state_name, NOX_ANNUAL = nox_emiss, ALLOW_BUDGET, allow_emiss_ratio, excess_allow, percent_excess) %>%
        collect()
    } else {
      emiss_allow_2 <- emiss_allow %>%
        select(state_name, NOX_OZONE = nox_emiss, ALLOW_BUDGET, allow_emiss_ratio, excess_allow, percent_excess) %>%
        collect()}
    emiss_allow_2
    
  })
  plot_map_join <- reactive ({
    states <- st_read("shapefiles/tl_2019_us_state.shp")
    contig_states <- states %>%
      filter(REGION != 9, NAME != 'Alaska', NAME !='Hawaii') %>%
      select(STUSPS, NAME)
    
    emiss_allow_returned_data <- emiss_alllow_output()
    map_with_data <- contig_states %>%
      left_join(emiss_allow_returned_data, by = c("NAME" = "state_name"))
    map_with_data
  })
  output$emiss_allow_plot <- renderPlot({
    prg <- prg_code()
    prg_code <- prg$prg_code
    if (is.null(prg_code) ||
        is.null(input$comp_year) || 
        is.null(input$map_type)) {
      return(NULL)
    }
    if(prg_code == 'ARP') {
    validate( "Please select a different program. ARP allocations are performed at the unit level, n\
              therefore budgets do not exist for each state as in later trading programs.")
      } else{
    map_with_data <- plot_map_join()
    map_with_data <- map_with_data %>%
      filter(op_year == input$comp_year)
    if (input$map_type == "Allowance Emission Ratio") {
      plot_1 <- map_with_data %>%
        ggplot() + geom_sf(aes(fill = allow_emiss_ratio)) + geom_sf_label(aes(label = round(allow_emiss_ratio, 1))) + 
        ggtitle("State Allowance-Emission Ratio") + scale_fill_gradient2(low = "red", high ="blue", mid = "white", midpoint = 1)
      plot_1
    } else {
      plot_2 <- map_with_data %>%
        ggplot() + geom_sf(aes(fill = percent_excess)) + geom_sf_label(aes(label = round(percent_excess, 0))) + 
        ggtitle("State Excess Allowance Percentages") + scale_fill_gradient2(low = "red", high ="blue", mid = "white", midpoint = 0) 
      plot_2
    }
  } 
  })
  
  output$ratio_example <- renderText({"The following maps demonstrate:
    1) the ratio of Allowances Budgeted to a State vs. the Total Emissions of all facilities within the State
       i.e. A ratio of 1.2 means that for every 1.2 allowances the state was budgeted, it emitted 1 ton of the pollutant 
    2) the percentage of excess allowances (when comparing the state's budget vs. its total emissions)
       i.e. A percentage of 3 means the state's number of excess allowances was equal to 3% of its allowance budget"
  })
  
  output$map_type_output <- renderUI({
    prg <- prg_code()
    prg_code <- prg$prg_code
    if (is.null(prg_code) ||
        is.null(input$comp_year)) {
      return(NULL)}
    selectInput(ns("map_type"), label = "Select Map", choices = sort(c("Allowance Emission Ratio", "Percent Excess Allowances")), multiple = FALSE)
  })
  
  output$comp_year_output <- renderUI({
    prg <- prg_code()
    prg_code <- prg$prg_code
    if (is.null(prg_code)){return(NULL)}
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    possibilities <- channel %>% tbl('account_compliance_dim') %>%
    filter(prg_code %in% !! prg_code)%>%
    distinct(op_year) %>% 
    collect()
    selectInput(ns("comp_year"), label = "Select Compliance Year", choices = sort(possibilities$op_year), multiple = FALSE)

  })
  
  output$map_table_output <- DT::renderDataTable({
    prg <- prg_code()
    prg_code <- prg$prg_code
    if (is.null(prg_code) ||
        is.null(input$comp_year)) {
      return(NULL)}
    emiss_alllow_output() 
  }, rownames = FALSE, class = "compact", filter = 'top', options = list(scrollX = TRUE))
  
  output$download_data <- downloadHandler(
    filename = "compliance_percentages.csv",
    content = function(file) {
      write.csv(emiss_alllow_output(), file)
    }
  )
}


