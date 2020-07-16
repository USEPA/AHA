library(plotly)
source("modules/programCode.R")
source("global.R")
compliance_balances_UI <- function(id) {
  ns <- NS(id)
  tabBox(
    title = "Compliance Balances",
    id = "compliance",
    height = "100%",
    width = "100%",
    tabPanel(
      program_code_UI(ns("program_code")),
      uiOutput(ns("state_output")),
      plotlyOutput(ns("compliance_plot")),
      #fluidRow(DT::dataTableOutput(ns("comp_balances_table_output"))),
      downloadButton(ns("download_data"),"Download Data")  
      
      
    )
  )
}

compliance_balances <- function(input, output, session) {
  prg_code <- callModule(program_code,"program_code")
  ns <- session$ns
  output$state_output <- renderUI({
    prg <- prg_code()
    prg_code <- prg$prg_code
    if (is.null(prg_code)) {
      return(NULL)
    }
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    facility_info <-
      channel %>% tbl('facility_ss')%>%select('fac_id','state')
    programs <- channel %>% tbl('account_compliance_dim')%>%
      select('account_number', 'prg_code')%>% distinct(.keep_all = TRUE)%>% filter(prg_code %in% !! prg_code)
    account_fact <- channel %>% tbl('account_fact') %>%
      select('account_number', 'fac_id')
    account_programs <- inner_join(programs, account_fact)
    filtered_facilities<- facility_info %>% inner_join(account_programs)%>% collect()
    states <- sort(unique(trimws(filtered_facilities$state)))
    pickerInput(
      ns("stateChoiceInput"),
      label = "State",
      states,
      options = list('actions-box' = TRUE),
      multiple = TRUE
    )
  })
  filter_facilities <- reactive({
    if (is.null(input$stateChoiceInput)) {
      return(NULL)
    }
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    prg <- prg_code()
    prg_code <- prg$prg_code
    facilities <-
      channel %>% tbl('facility_ss') %>% filter(state %in% !!input$stateChoiceInput)
    account_compliance <-
      channel %>% tbl("account_compliance_dim") %>%
      filter(prg_code %in% !!prg_code, op_year > 2006)
    account_info <-
      channel %>% tbl("account_fact")
    facilities <-
      facilities %>% inner_join(account_info, by = "fac_id") %>% inner_join(account_compliance,
                                                                            by = c("account_number", "prg_code"))
    facilities$state <- gsub('\\s+', '', facilities$state)
    facilities %>% collect()
  })
  account_bal <- reactive({
    if (is.null(input$stateChoiceInput)) {
      return(NULL)
    }
    channel <- connect_to_db()
    on.exit(dbDisconnect(channel), add = TRUE)
    facilities <- filter_facilities() %>% collect()
    account_compliance <-
      channel %>% tbl("account_compliance_dim")
    bank_dim <- channel %>% tbl("bank_dim")
    compBank <-
      inner_join(
        account_compliance,
        bank_dim,
        by = c(
          "account_number" = "account_number" ,
          "prg_code" = "prg_code",
          "op_year" = "calendar_year"
        )
      ) %>% select(
        "account_number",
        "op_year",
        "prg_code",
        "allocated",
        "total_allowance_recieved",
        "total_held",
        "comp_year_emiss",
        "total_deduct",
        "carried_over",
        "total_allowance_sold",
      ) %>% collect() %>% semi_join(facilities, by = c("account_number", "prg_code"))
    compBank
  })
  
  output$comp_balances_table_output <- DT::renderDataTable({
    if (is.null(input$stateChoiceInput)) {
      return(NULL)
    }
    account_bal()
  }, rownames = FALSE, class = "compact", filter = 'top', options = list(scrollX = TRUE))
  
  
  output$compliance_plot <- renderPlotly({
    if (is.null(input$stateChoiceInput)) {
      return(NULL)
    }
    allowanceTotals <- filter_facilities()
    #building the plots for the different programs
    compPlot <- plot_ly(type = "scatter")
    for (prg_code in unique(allowanceTotals$prg_code)) {
      temp_totals <-
        allowanceTotals %>% filter(prg_code == prg_code) %>%  group_by(op_year) %>%
        summarize(
          allocated_total = sum(allocated),
          held_total = sum(total_held),
          deductions = sum(total_deduct)
        )
      
      compPlot <-
        add_trace(
          compPlot,
          x = temp_totals$op_year,
          y =  temp_totals$allocated_total,
          name = paste(prg_code, 'allocated_total'),
          mode = 'lines+markers'
        ) %>%
        add_trace(
          x = temp_totals$op_year,
          y =  temp_totals$held_total,
          name = paste(prg_code, 'total_held'),
          mode = 'lines+markers'
        ) %>%
        add_trace(
          x =  temp_totals$op_year,
          y = temp_totals$deductions,
          name = paste(prg_code, 'total_deductions'),
          mode = 'lines+markers'
        )
    }
    compPlot
  })
  
  output$download_data <- downloadHandler(
    filename = "downloadtest.csv",
    content = function(file) {
      write.csv(mtcars, file)
    }
    
  )
  
}