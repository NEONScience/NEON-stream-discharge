ui <- shinydashboard::dashboardPage(
    #skin= "green",
    #theme = bslib::bs_theme(),
header <- shinydashboard::dashboardHeader(title = "Open Flow"),
    ####******images and logos not shown yet *****###
    #tags$head(type= "text/css"),
    # tags$img(src = "logo-NEON-NSF.png",width = 200,height = 75)
      
    #####modal scaling
 # End of page header

  #Sidebar menu                
sidebar <- dashboardSidebar(
                          shinydashboard::sidebarMenu(
                                            shinydashboard::menuItem("About the App", tabName = "AbouttheApp", icon = icon("info-circle")),
                                            shinydashboard::menuItem("Time series viewer", tabName = "OpenFlow", icon = icon("sitemap")),
                                            shinydashboard::menuItem("Cross section viewer", tabName = "CrossSection", icon = icon("pizza-slice")), #This does not exist yet, JB working on it
                                            shinydashboard::menuItem( "Calculated Gauge Height", tabName = "gaugeHeight", icon=icon("water"))
                                            )
                           ),
 
  
 #Body of each menu item
body <- shinydashboard::dashboardBody(
  useShinyjs(),
   shinydashboard::tabItems(
     
     #Tab Item for About the App
     shinydashboard::tabItem(tabName= "AbouttheApp",
              #tags$img(src = "app-logo.png",width = 300,height = 150),
              shiny::fluidRow(shiny::includeMarkdown('../README.md'))
              ),#end of fluid row for about the app
     
     #Tab Item for Open flow inputs
     shinydashboard::tabItem(tabName= "OpenFlow",
              #tags$img(src = "app-logo.png",width = 300,height = 150),
              shiny::fluidRow(shiny::column(2,
                                          shiny::selectInput("domainId","Domain ID",productList$domain),
                                          shiny::fluidRow(shiny::uiOutput("domainInfo")),
                                          shiny::br(),
                                            shiny::fluidRow(
                                              shiny::selectInput("siteId","Select Site ID",NULL)),
                                              shiny::fluidRow(shiny::uiOutput("siteInfo")),
                                              shiny::br(),
                                              shiny::fluidRow(shiny::dateRangeInput("dateRange","Date range:",
                                                     startview="month",
                                                      min="2010-01-01",
                                                      start=lubridate::floor_date(base::Sys.Date()-14,"month")-base::months(1),
                                                      end=lubridate::floor_date(base::Sys.Date()-14,"month")-1, format="yyyy-mm-dd"),
                                                                  shiny::textInput("apiToken", "NEON API Token (Optional)")),
                                                      shiny::br(),
                                                        shiny::fluidRow(
                                                        shiny::actionButton(inputId="submit","Submit")),
                                                        shiny::br(),
                                                        shiny::fluidRow(# shiny::checkboxInput("qctrFlag", "Include Final Quality Flag for Discharge(light gray)", FALSE),
                                                          shiny::checkboxInput("qctrFlagScRv", "Include Discharge Science Review Quality Flags", FALSE),
                                                          # shiny::checkboxInput("precipQctrFlag", "Include Final Quality Flag for Precipitation(gray)", FALSE),
                                                          # shiny::checkboxInput("precipQctrFlagScRv", "Include Science Review Quality Flag for Precipitation(gray)", FALSE),
                                                          shiny::checkboxInput("impUnitFlag", "Convert to Imperial Units", FALSE),
                                                          shiny::checkboxInput("dark_mode", "Show in Dark Mode")),
                                                        shiny::hr(),
                                                        shiny::fluidRow(shiny::conditionalPanel(
                                                            #checks that one of the graphs has been loaded
                                                            condition = "output.plot1 != null || output.plot2 != null",
                                                            shiny::downloadButton("downloadPlotly", "Download Graph"))),
                                                          shiny::br(),
                                                          shiny::fluidRow(shiny::textOutput("title"),
                                                                                      DT::dataTableOutput("table"))),#end of first col
                            shiny::column(10,
                                shiny::tabsetPanel(type = "tabs",id = "selectedTab",
                                        shiny::tabPanel("Continuous Discharge",
                                                      shinycssloaders::withSpinner(plotly::plotlyOutput("plot1",height="800px"))),
                                        shiny::tabPanel("Rating Curve(s)",
                                                      shinycssloaders::withSpinner(plotly::plotlyOutput("plot2",height="800px"),
                                                                         color = "#00ADD7"))
                                )
                        )#end of second column
                )#end of fluid row for open flow,
     ),
  
     #Tab Item for Blue Heron
      shinydashboard::tabItem(tabName= "gaugeHeight",
                               #img(src= #"Blue Heron-logo.png", width = 300,height = 150) #needs a comma whenadding Lucas app
                              #rtdv is shorthand for real-time-data-viewer 
                               shiny::fluidRow(
                                 shiny::column(3,
                                      shinydashboard::box( width = 12, 
                                          #Input for selecting domain
                                          selectizeInput(input = "rtdvDomain", label = "Choose a domain", choices = productList$domain),##replaced the choices from unique(domainSite_info$field_domain_id
                                          #Input for selecting site based off the domain selected
                                          selectizeInput(input = "rtdvSite", label = "Choose a site", choices = NULL),
                                          #Input for selecting the type of water GW = Groundwater and SW = Surfacewater
                                          selectInput(input = "waterType", label = "Choose GW or SW", choices = c("GW","SW")),
                                          h5("GW is for groundwater and SW is for surface water"),
                                          #Input for selecting between an AquaTROLL and LevelTROLL depending what is installed at the SW location
                                          selectInput(input = "trollType", label = "What kind of Troll is it?", choices = c("AquaTroll","LevelTroll")),
                                          #Input for selecting HOR location, techs will likely know which theirs is
                                          selectInput(input = "HOR", label = "Choose Location", choices = NULL),
                                          #Input for selecting a date range of the data desired
                                          dateRangeInput(inputId = "rtdvDaterange", label = "Select date range of pressure reading(s)", start = Sys.Date()-2, end = Sys.Date()-1, max = Sys.Date()-1),
                                          #Input for selecting whether L0 data is used for the graph or not
                                          selectizeInput(input = "dataSource", label = "Where is the data coming from?", choices = c("L0 Data Query", "Grafana CSV File", "Instant Pressure Reading")),
                                          fileInput(inputId = "grafanaFile", label = "Upload the Grafana .csv file (Under Maintenance DONT USE)"),
                                          #Text input for whenever dataSource input is set to not query L0 data
                                          textInput(inputId = "singlePressure", label = "Insert single pressure reading here", value = ""),
                                          #Load bar to show progress of gathering spatial data associated with the TROLL
                                          actionButton(inputId = "rtdvRun", label = "Run")
                                        )
                                      ),
                                   shiny::column(7,
                                    #Provide general information about the app
                                    tabsetPanel( id = "calculatedStageTimeSeries",
                                      tabPanel( title = "About",
                                                includeHTML("introMaterials/staffGaugeIntroText.html")
                                                      ),
                                            tabPanel( id = "CG_timeSeries",
                                                      title = "Calculated Stage Height",       
                                                      #Shows plotly output of water depth with time series
                                                      div(id = "Title_CWE",(h3("Calculated Stage Height"))),
                                                      div(id = "LB",progressBar(id = "GaugeHeightLoadBar", value = 0, title = "Waiting for run button click...")),  ##******need to update this value input option in order to work*****##
                                                      plotlyOutput("calculatedStagePlot"),
                                                      textOutput("singleWaterColumnHeight"), tags$head(tags$style("#singleWaterColumnHeight{font-size: 20px;}")), 
                                                      plotlyOutput("rtdvDischargePlotly")
                                                      )
                                              )
                                        )
                               
                                    
          )
      ) 
  )
)
)
 
# ui <- shinydashboard::dashboardPage(header, sidebar, body)
 

#### end of ui and fluidPage


