#instaling and loading dependencies

# install.packages('shinythemes')
# install.packages('ecocomDP')
# install.packages('plotly')
#install.packages('readr')
# library(shiny)
# library(neonUtilities)
#  library(dplyr)
# # library(devtools)
# library(tidyverse)
# # library(ecocomDP)
# library(readr)
#  library(plotly)
# library(shinyWidgets)
# library(shinythemes)
#-----------------------------------------------
 

  # user interface
  ui <- fluidPage(
    #background color
    # shinyWidgets::setBackgroundColor(color = "#e6f5ff", 727477
    #                                  gradient = "linear",
    #                                  direction = "bottom"),
    
    shiny::titlePanel("Customize  Discharge Time Series "),
    
    shiny::sidebarLayout(
      
      
      shiny::sidebarPanel(
               
        
          id = "form",
          
          # textInput(inputId = "domainId", "Domain ID", "DO1"),
          selectInput(inputId ="domainId", "Domain ID",
                      
                      c("D01", "D02", "D03", "D04","D05",
                        "D06", "D07", "D08", "D09","D10",
                        "D11", "D12", "D13", "D14","D15",
                        "D16",  "D17", "D18", "D19","D20"
                       )),
          selectInput("siteId", "Select Site ID", choices = NULL ),

          dateRangeInput("dateRange", "Date range:",
                         startview = "decade",
                         min    = "2013-01-01",
                         start = NULL, end = NULL, 
                          #format = "yyyy-mm-dd", 
          ),
          #textOutput('startDate'),
    
          actionButton(inputId="submit", "Submit", class = "btn-primary"),
        
    
        
        shiny::br(),
        
        
        shiny::p(),
      ), #end of sidebarPanel
      
      shiny::mainPanel(
        
       plotlyOutput('plot')
      ) # end mainPanel
    )# end of sidebarLayout
    
  ) # end of ui and fluidPage
  

  library(dplyr)
  library(tidyverse)
  library(readr)
  library(plotly)
  library(shinyWidgets)  
  
  #importing domain sites csv, map your own pc
  productList <- read_csv("productList.csv")

  
  #server function
  server <- function(session, input, output) {
    
    observe({
    if (is.null(input$domainId) || is.na(input$domainId)){
      return()
    } else {
        x <- productList$siteid[productList$domainid == input$domainId]
        updateSelectInput(session,"siteId",choices = unique(x))
        
    }
      })
   
    output$startDate <- renderText(
      paste(input$dateRange[1])
    )
    
    #PLting
    
    #downloading data package from Neon portal
    package <- reactive({
    NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJkaXZpbmVhc2Vha3VAZ21haWwuY29tIiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxNzgyNzUzNTM0LCJpYXQiOjE2MjUwNzM1MzQsImVtYWlsIjoiZGl2aW5lYXNlYWt1QGdtYWlsLmNvbSJ9.9gJXs7Sf5lO_mQG0IbaD1rZwAUZ_SVQ-cIqvyDfqOLhX5XoZR4SFgy77rrw8BNhTHMYsca524z4xxDDHRgFAfg"
    
    # Get continuous discharge data from the NEON API
    ctDischarge<- neonUtilities::loadByProduct(
      dpID="DP4.00130.001",
      token = Sys.getenv("NEON_TOKEN"),
      package = "expanded",
      check.size = F,
      site = as.character(input$siteId),
      startdate = as.character(input$dateRange[1]),
      enddate = as.character(input$dateRange[2])
     )
    
    # DP4.00130.001 <- neonUtilities::loadByProduct(
    #   dpID="DP4.00130.001",
    #   token = Sys.getenv("NEON_TOKEN"),
    #   package = "expanded",
    #   check.size = F,
    #   site = "LEWI",
    #   startdate = "2018-09- 20",
    #   enddate = "2018-10 -19"
    # )
    
  
    
    # Get Discharge rating curve data
    stDischarge<- neonUtilities::loadByProduct(
      dpID="DP4.00133.001",
      token = Sys.getenv("NEON_TOKEN"),
      package = "expanded",
      check.size = F,
      site = as.character(input$siteId),
      startdate = as.character(input$dateRange[1]),
      enddate = as.character(input$dateRange[2])
    )
    
    })#end of reactive
    
    # Unpack all data frames into objects
    list2env(ctDischarge, .GlobalEnv)
    list2env(stDischarge, .GlobalEnv)
    
    
   
    
    
    
    
    
  }#end of server
    
    
  # Run the app ----
  shiny::shinyApp(ui = ui, server = server)