# Define UI for application 
ui <- shiny::fluidPage(
  
  # App theme
  theme = bslib::bs_theme(version = 4),
  
  # Margin padding
  style = "padding:25px;",
  
  # Modal Scaling
  tags$head(tags$style("#shiny-modal img { max-width: 100%; }")),
  
  # Title panel with logos
  shiny::titlePanel(shiny::fluidRow(
    # App logo
    shiny::column(10,
                  img(src = "app-logo.png",width = 300,height = 150)),
    # NEON-NSF logo
    shiny::column(2,
                  img(src = "logo-NEON-NSF.png",width = 200,height = 75)))),# End of title panel
  
  # Main panel of app
  shiny::fluidRow(
    
    # First column, left side bar with user input
    shiny::column(2,
                  
                  # Select the domain
                  shiny::fluidRow(
                    shiny::selectInput("domainId","Domain ID",productList$domain)),
                  
                  # Based on the domain, generate hyperlinks to the NEON Portal for information about the domain
                  shiny::fluidRow(
                    shiny::uiOutput("domainInfo")),
                  shiny::br(),
                  
                  # Based on the domian, select the site
                  shiny::fluidRow(
                    shiny::selectInput("siteId","Select Site ID",NULL)),           
                  
                  # Based on the site, generate hyperlinks to the NEON Portal for information about the site
                  shiny::fluidRow(
                    shiny::uiOutput("siteInfo")),                            
                  shiny::br(),                          
                  
                  # Select the date range - defaults the the most recent previous month
                  shiny::fluidRow(
                    shiny::dateRangeInput("dateRange","Date range:",
                                          startview="month",
                                          min="2016-01-01",
                                          start=lubridate::floor_date(base::Sys.Date()-14,"month")-base::months(1),
                                          end=lubridate::floor_date(base::Sys.Date()-14,"month")-1,
                                          format="yyyy-mm-dd"),
                    # shiny::textInput("apiToken", "NEON API Token (Optional)")),                                   
                  shiny::br(),                               
                  
                  # Submit button
                  shiny::fluidRow(
                    shiny::actionButton(inputId="submit","Submit")),                              
                  shiny::br(),
                  
                  # Checkbox options for the visuals (SRFs, Units, Dark mode)
                  shiny::fluidRow(
                    shiny::checkboxInput("qctrFlagScRv", "Include Discharge Science Review Quality Flags", FALSE),
                    shiny::checkboxInput("impUnitFlag", "Convert to Imperial Units", FALSE),
                    shiny::checkboxInput("dark_mode", "Show in Dark Mode")),
                  shiny::hr(),
                  
                  # Button to download graph as an HTML and save to local downloads file
                  shiny::fluidRow(
                    shiny::conditionalPanel(
                      #checks that one of the graphs has been loaded
                      condition = "output.plot1 != null || output.plot2 != null",
                      shiny::downloadButton("downloadPlotly", "Download Graph"))),
                  shiny::br(),
                  
                  # Data frame that shows site metadata
                  shiny::fluidRow(shiny::textOutput("title"),
                                  DT::dataTableOutput("table")))),# End of first column
    
    # Second column, plotting pane
    shiny::column(10,
                  
                  # Generate a set of tabs for plotting and readme
                  shiny::tabsetPanel(type = "tabs",id = "selectedTab",
                                     
                                     # Continuous discharge plotting tab
                                     shiny::tabPanel("Continuous Discharge",
                                                     shinycssloaders::withSpinner(
                                                       plotly::plotlyOutput("plot1",height="800px"))),
                                     
                                     # Rating curve plotting tab
                                     shiny::tabPanel("Rating Curve(s)",
                                                     shinycssloaders::withSpinner(
                                                       plotly::plotlyOutput("plot2",height="800px"),
                                                                            color = "#00ADD7")),
                                     
                                     # Readme tab
                                     shiny::tabPanel("About the App",
                                                     shiny::includeMarkdown("about.Rmd"))))# End of second column
    )# End of main panel
  
)# End of UI