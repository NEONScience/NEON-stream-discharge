
    #skin= "green",
    #theme = bslib::bs_theme(),
header <- shinydashboard::dashboardHeader(title = "Open Flow")
    ####******images and logos not shown yet *****###
    #tags$head(type= "text/css"),
    # tags$img(src = "logo-NEON-NSF.png",width = 200,height = 75)
      
    #####modal scaling
 # End of page header

  #Sidebar menu                
sidebar <- dashboardSidebar(160,
                          shinydashboard::sidebarMenu(
                                            shinydashboard::menuItem("About the App", tabName = "AbouttheApp", icon = icon("info-circle")),
                                            shinydashboard::menuItem("Time series viewer", tabName = "OpenFlow", icon = icon("sitemap")),
                                            shinydashboard::menuItem("Cross section viewer", tabName = "CrossSection", icon = icon("pizza-slice")), #This does not exist yet, JB working on it
                                            shinydashboard::menuItem( "Blue Heron", tabName = "waterlevel", icon=icon("hourglass-half"))
                                            )
                           )
 
  
 #Body of each menu item
body <- shinydashboard::dashboardBody(
  useShinyjs(),
   shinydashboard::tabItems(
     
     #Tab Item for About the App
     shinydashboard::tabItem(tabName= "AbouttheApp",
              #tags$img(src = "app-logo.png",width = 300,height = 150),
              shiny::fluidRow(
                                shiny::includeMarkdown('../README.md'))
                ),#end of fluid row for about the app
     
     #Tab Item for Open flow inputs
     shinydashboard::tabItem(tabName= "OpenFlow",
<<<<<<< HEAD
              #tags$img(src = "app-logo.png",width = 300,height = 150),
              shiny::fluidRow(shiny::column(2,
                                          shiny::selectInput("domainId","Domain ID",productList$domain)),
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
                                                                         color = "#00ADD7")),
                                )#end of second column
                ),#end of fluid row for open flow
=======
                             tags$img(src = "app-logo.png",width = 300,height = 150),
                             shiny::column(width= 4, 
                                           shiny::fluidRow(
                                             shinydashboard::box(width =6,
                                                                 shiny::selectInput("domainId","Domain ID",productList$domain),
                                                                 shiny::fluidRow(shiny::uiOutput("domainInfo")),
                                                                 shiny::br(),
                                                                 shiny::fluidRow(
                                                                   shiny::selectInput("siteId","Select Site ID",NULL)),
                                                                 shiny::fluidRow(shiny::uiOutput("siteInfo")),
                                                                 shiny::br(),
                                                                 shiny::fluidRow(
                                                                   shiny::dateRangeInput("dateRange","Date range:",
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
                                                                   shiny::checkboxInput("precipQctrFlag", "Include Final Quality Flag for Precipitation(gray)", FALSE),
                                                                   shiny::checkboxInput("precipQctrFlagScRv", "Include Science Review Quality Flag for Precipitation(gray)", FALSE),
                                                                   shiny::checkboxInput("impUnitFlag", "Convert to Imperial Units", FALSE),
                                                                   shiny::checkboxInput("dark_mode", "Show in Dark Mode")),
                                                                 shiny::hr(),
                                                                 shiny::fluidRow(shiny::conditionalPanel(#checks that one of the graphs has been loaded
                                                                   condition = "output.plot1 != null || output.plot2 != null",
                                                                   shiny::downloadButton("downloadPlotly", "Download Graph"))),
                                                                 shiny::br(),
                                                                 shiny::fluidRow(shiny::textOutput("title"),
                                                                                 DT::dataTableOutput("table"))))
                                           ),#end of first col
              
                                        shiny::column(width =8,
                                                      shiny::fluidRow(
                                                        shiny::tabsetPanel(type = "tabs",id = "selectedTab",
                                                        shiny::tabPanel("Continuous Discharge",
                                                                        shinycssloaders::withSpinner(plotly::plotlyOutput("plot1",height="800px"))),
                                                        shiny::tabPanel("Rating Curve(s)",
                                                                        shinycssloaders::withSpinner(plotly::plotlyOutput("plot2",height="800px"),color = "#00ADD7")),
                                                        shiny::tabPanel("About the App",
                                                                        shiny::includeMarkdown(readmeFile)))
                                                        )#end of second column
                                                      )
                             ),#end of fluid row for open flow
>>>>>>> 486406dd34a7a1145861e13cf57d645a3a5ad6e0
  
     #Tab Item for Blue Heron
      shinydashboard::tabItem(tabName= "waterlevel",
     shinydashboard::tabItem(tabName= "waterlevel",
                               #img(src= #"Blue Heron-logo.png", width = 300,height = 150) #needs a comma whenadding Lucas app
                               shiny::fluidRow(
                                 shiny::column(4,
                                      shinydashboard::box(
                                          #Input for selecting domain
                                          selectizeInput(input = "BH_domain", label = "Choose a domain", choices = productList$domain),##replaced the choices from unique(domainSite_info$field_domain_id
                                          #Input for selecting site based off the domain selected
                                          selectizeInput(input = "BH_site", label = "Choose a site", choices = NULL),
                                          #Input for selecting the type of water GW = Groundwater and SW = Surfacewater
                                          selectInput(input = "waterType", label = "Choose GW or SW", choices = c("GW","SW")),
                                          h5("GW is for groundwater and SW is for surface water"),
                                          #Input for selecting between an AquaTROLL and LevelTROLL depending what is installed at the SW location
                                          selectInput(input = "trollType", label = "What kind of Troll is it?", choices = c("AquaTroll","LevelTroll")),
                                          #Input for selecting HOR location, techs will likely know which theirs is
                                          selectInput(input = "HOR", label = "Choose Location", choices = NULL),
                                          #Input for selecting a date range at which the TROLL was likely installed
                                          #Optional inputs for water column height
                                          numericInput(input = "wellDepth", label = "Enter the wells depth (Optional)", value = NULL),
                                          h5("Leave blank if the water column height is not wanted"),
                                          numericInput(input = "cableLength", label = "Enter the Troll cable length (Optional)", value = NULL),
                                          h5("Leave blank if the water column height is not wanted"),
                                          dateRangeInput(inputId = "dateRange", label = "Select date range of pressure reading(s)", start = NULL, end = NULL),
                                          #Input for selecting whether L0 data is used for the graph or not
                                          selectizeInput(input = "L0Choice", label = "Using L0 data?", choices = c("Yes", "No")),
                                          h5("If using L0 data, place the file Data.csv that was downloaded in the data/ folder"),
                                          #Text input for whenever L0Choice is set to "No", this is used for instant water depth readings
                                          textInput(inputId = "singlePressure", label = "Insert single pressure reading here", value = ""),
                                          #Load bar to show progress of gathering spatial data associated with the TROLL
                                          div(id = "LB",progressBar(id = "spatialDataLB", value = 0, title = "Initializing data load")),  ##******need to update this value input option in order to work*****##
                                          actionButton(inputId = "BH_run", label = "Run")),
                                   shiny::column(6,
                                    #Provide general information about the app
                                    tabsetPanel(
                                      tabPanel( title = "About",
                                          div(id = "Intro",
                                            h3("About the Blue Heron Application"),
                                            p("This application provides an conversion of water pressure to water elevation with provided L0 data or at a single instance for both ground water and surface water."),
                                            br(),
                                            h3("How to use this application:"),
                                            p("1. Choose a domain and site.",br(),
                                              "2. Choose the water type; GW (ground water) or SW (surface water).",br(),
                                              "3. Choose a location. This is site dependent.",br(),
                                              "4. Select the date range for the pressure readings.",br(),
                                              "5. Select if L0 data is used or not."),
                                            br(),
                                            h3("Important Notes:"),
                                            p("1. If the user selects ", strong("L0 data")," as yes, the Data.csv that the user retrieves from L0 needs to be placed in Water_Elevation/data/. Otherwise it will generate an error.", br(), 
                                              "2. When GW is selected as the water type the app will generate two input boxes for the wells depth and troll cable length, ", strong(" both must be filled in "),", otherwise they must remain ", strong("blank."),br(),
                                              "3. Be sure to select the correct", strong("date range"), ", this will determine the calibration information retrieved and impacts the conversion.")
                                                          )
                                                      ),
                                            tabPanel( title = "Water Elevation",       
                                                      #Shows plotly output of water depth with time series
                                                      div(id = "Title_CWE",(h3("Calculated Water Elevation"))),
                                                      plotlyOutput("waterElevation"),
                                                      textOutput("singlePressureOutput"), tags$head(tags$style("#singlePressureOutput{font-size: 20px;}")),
                                                      div(id = "Title_WCH",(h3("Calculated Water Column Height"))),
                                                      plotlyOutput("waterColumnHeightPlot"),
                                                      textOutput("singleWaterColumnHeight"), tags$head(tags$style("#singleWaterColumnHeight{font-size: 20px;}"))
                                                      )
                                              )
                                        )
                               
                                    )
          )
      ) 
  )
)
 
ui <- shinydashboard::dashboardPage(header, sidebar, body)
 

#### end of ui and fluidPage


