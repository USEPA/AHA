Hello, welcome to AHA.

This is an R Shiny application that tries to allow viewers new perspectives on CAMD data.



This app is for prototyping purposes only. 





# Packages
To use this application you will need the following R packages:
odbc,
DBI,
dplyr,
dbplyr,
lubridate,
stringr,
ggplot2,
shiny,
shinydashboard,
DT,
plotly,
hash,
data.table,
networkD3,
shinyWidgets,
scales

 To add packages in R, use the following command in the R console:
	
		install.packages("PackageName")




# running the app
CAMD users can create the CSV files that the app runs off of by using the utility function pullfromash.Rmd. You will need to know your username password for this to work. 

Non-CAMD users can use the tables from AMPD to get the right files to allow for the SQL script to generate the database.

**note:** you must have docker desktop installed to create and run the dev database as it is currently set up

to launch the database for the app, go to the command line with the docker file, and type

`docker-compose up`

You can then launch the Shiny application normally through R Studio.

The easiest way to run the app is to click on "Run App" in the code editing screen of either the ui.R or server.R in R studio

# About the the modules
## transactionSummary.R
This module calculates the total number of transactions between distinct and related companies. What this code does:

1. Create a network of all of the facility and general accounts, linking account that have the same owners
2. Iterates through the transaction table in ASH to see if both of the companies in a transaction are part of the same network, if so, they are marked as a transaction between related companies. 
3. Sum up the total number of transaction, total number of related transactions, and total number of distinct transactions. Divide distinct transactions by total transactions to get your number


To learn more about how network analysis works, read this https://www.jessesadler.com/post/network-analysis-with-r/

## allowanceTransfers.R
This module makes a network graph that shows transactions between different accounts. This is working off of TRANSACTION_FACT from ASH. What is happening here:

1. User sorts TRANSACTION_FACT for desired dates and program(s) 
2. Transasctions are grouped by what parties are conducting the transaction. For instance if there were two transactions between A and B, there would only be one edge linking them. It is possible to calculate a weight and a direction for these transaction but I didn't do that here.
3. Node ids are assigned to each distinct account value for buyer and seller accounts
4. All transaction fact rows are labeled with the buyer and seller node ids
5. Network graph 3d generates the graph