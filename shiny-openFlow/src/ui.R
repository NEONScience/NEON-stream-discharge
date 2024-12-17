#Xs Section branch
######Maria Viggiano###
    #skin= "green",
    #theme = bslib::bs_theme(),
ui<- shinydashboard::dashboardPage(
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
                                            shinydashboard::menuItem("Time Series Viewer", tabName = "OpenFlow", icon = icon("chart-line")),
                                            shinydashboard::menuItem("Target Gauge Height", tabName = "CrossSection", icon = icon("bullseye")), #This does not exist yet, JB working on it
                                            shinydashboard::menuItem("Real-Time Data Viewer", tabName = "gaugeHeight", icon=icon("water"))
                                            )
                           ),
 
  
 #Body of each menu item

body <- shinydashboard::dashboardBody(
  tags$head(
    tags$style(HTML(
      " .custom-box { border: 2px solid #007bff; 
      padding: 10px; margin-bottom: 20px; box-shadow: 2px 2px 10px #888888; 
      } .custom-input { width: 100%; margin-bottom: 10px; } ")) ),
  tags$head(tags$style(HTML(".small-box {height: 90px}"))),
  tags$head(tags$style("#shiny-modal img { max-width: 100%; }")),
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
                                            shiny::uiOutput("domainInfo"),
                                            shiny::br(),
                                            shiny::selectInput("siteId","Select Site ID",NULL),
                                            shiny::uiOutput("siteInfo"),
                                            shiny::br(),
                                            shiny::dateRangeInput("dateRange","Date range:",
                                                                  startview="month",
                                                                  min="2016-07-01",
                                                                  start=lubridate::floor_date(base::Sys.Date(),"month")-base::months(1),
                                                                  end=base::Sys.Date()-1),
                                            div(style = "text-align:center",shiny::actionButton(inputId="submit","Submit")),
                                            shiny::br(),
                                            shiny::checkboxInput("qctrFlagScRv", "Include Discharge Science Review Quality Flags", FALSE),
                                            shiny::checkboxInput("impUnitFlag", "Convert to Imperial Units", FALSE),
                                            shiny::checkboxInput("dark_mode", "Show in Dark Mode"),
                                            div(style = "text-align:center",
                                                shiny::conditionalPanel(condition = "output.plot1 != null || output.plot2 != null",#checks that one of the graphs has been loaded
                                                                        shiny::downloadButton("downloadPlotly", "Download Graph"))),
                                            shiny::br(),
                                            div(style = "text-align:center",h4("Metadata Table")),
                                            DT::dataTableOutput("table")
                                            ),#end of first col
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
	 
	#Tab Item for cross section menu
    shinydashboard::tabItem(tabName ="CrossSection",
                            #first row with inputs and output DT for domain target flow
                            shiny::fluidRow(
                              shinydashboard::box(
                                width=2,
                                shiny::selectInput(input= "XS_Domain", label= "Choose Domain",  choices = Target_df$Domain, selected = "No Domain Selected"),
                                #shiny::fluidRow(shiny::uiOutput("domainInfo")),
                                shiny::br(),
                                shiny::fluidRow(shiny::selectInput(input= "XS_site",label = "Select Site ID", choices = Target_df$Site, selected= "No Site Selected")),
                                #shiny::fluidRow(shiny::uiOutput("siteInfo")),
                                shiny::br(),
                                shiny::fluidRow(
                                shiny::actionButton(input="ShowPlot",label ="Show Plots"),
                                )
                                  #render plotly to show plots for site
                                
                              ),
                                 
                              # Summary of Domain target datatable per site selected
                              shinydashboard::box(
                                width= 10,
                                title= "Domain Target Information",
                                solidHeader= TRUE,
                                status= "primary",
                                #Valuebox for the each of result for target gauge height and high flow period
                                #bslib::layout_columns()
                                #h4("Selected Site target Flow Details:"),
                                h3(uiOutput("TargetGaugeHeights"))
                            
                            )
                          ),
                                               
                            
                            #second row with Graphs for cross section and rating curve
                            shiny::fluidRow(
                              shinydashboard::box(
                                title= "DSC Cross-section",
                                width= 6,
                                div(id= "Discharge Cross-Section", 
                                    (h6("Site's Latest Cross section target flow"))),
                                plotlyOutput(outputId="xsdsc", width= "auto", height= "auto")##last edit on 7/18
                                
                              ),
                              shinydashboard::box(
                                title= "Rating Curve",
                                width= 6,
                                div(id= "Rating Curve",(h3("Site's Current Rating Curve"))),
                                plotlyOutput(outputId="curve", width= "auto", height= "auto")
                              )
                            )
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
                                                      textOutput("singleWaterColumnHeight"), tags$head(tags$style("#singleWaterColumnHeight{font-size: 20px;}"))
                                                      )
                                              )
                                        )
                               
                                    
          )
      ) 
  )
)
)

   
    
  


#### end of ui and fluidPage

