source("modules/allowanceTransfers.R")
source("modules/transactionSummary.R")
source("modules/programCode.R")
source("modules/complianceBalances.R")
source("modules/complianceMap.R")
source("modules/accounts.R")
source("modules/account_types.R")
source("modules/acct_type_allowances.R")
source("modules/programAllocation.R")
source("modules/retired.R")
source("modules/unit_commence.R")
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    # Application title
    dashboardHeader(title="AHA"),

    # Sidebar 
    dashboardSidebar(
        sidebarMenu(
          menuItem("Allowance Transfers", tabName= "allowance_transfers", icon=icon("project-diagram")),
          menuItem("Compliance Balances", tabName= "compliance_balances", icon=icon("cog")),
          menuItem("Accounts", tabName = "accounts", icon=icon("piggy-bank")),
          menuItem("Program Allocations", tabName = "program_allocations", icon=icon("chart-pie")),
          menuItem("Operating Info", tabName = "operating_info",icon=icon("file-invoice"))
          )
        ),

        # Body
        dashboardBody(
           tabItems(
               tabItem("allowance_transfers",
                       tabBox( height ="100%",width ="100%",
                               tabPanel("Allowance Transfers", allowance_transfers_UI("allowance_transfers_UI")),
                               tabPanel("Transaction Summary", transaction_summary_UI("trans_summary_UI"))
                       )
               ),
               tabItem("compliance_balances",
                       tabBox( height ="100%",width ="100%",
                               tabPanel("Compliance Balances", compliance_balances_UI("compliance_balances_UI")),
                               tabPanel("Compliance Map", compliance_map_UI("compliance_map_UI")))
                       
                       
               ),
               tabItem("accounts", 
                       tabBox(height = "100%", width = "100%",
                              tabPanel("Yearly Account Balance", accounts_UI("accounts_UI")),
                              tabPanel("Accounts by Program", account_types_UI("account_types_UI")),
                              tabPanel("Allowances by Account Type", acct_types_allowances_UI("acct_types_allowances_UI"))
                       )
               ),
               tabItem("program_allocations", 
                       program_allocations_UI("program_allocations_UI")
               ),
               tabItem("operating_info",
                       tabBox(height = "100%", width = "100%",
                              tabPanel("Retired Units", retired_UI("retired_UI")),
                              tabPanel("Unit Commence Dates", unit_commence_UI("unit_commence_UI"))
                       )
               )
               
           )
        )
    
)

# Define server logic
server <- function(input, output) {
    #allowance transfers 
    callModule(allowance_transfers, 'allowance_transfers_UI')
    callModule(transaction_summary, 'trans_summary_UI')
    #compliance balances and map
    callModule(compliance_balances, 'compliance_balances_UI')
    callModule(compliance_map, 'compliance_map_UI')
    #accounts
    callModule(accounts, 'accounts_UI')
    callModule(account_types, 'account_types_UI')
    callModule(acct_types_allowances, 'acct_types_allowances_UI')
    #Program allocations
    callModule(program_allocations, 'program_allocations_UI')
    #Commence operations & Retirement
    callModule(retired,'retired_UI' )
    callModule(unit_commence, 'unit_commence_UI')
    
}

# Run the application 
shinyApp(ui = ui, server = server)
