#Xs Section branch
######Maria Viggiano###
    #skin= "green",
    #theme = bslib::bs_theme(),
ui<- shinydashboard::dashboardPage(
  hader <- shinydashboard::dashboardHeader(title = "Open Flow"),

  #Sidebar menu                
  sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(shinydashboard::menuItem("About the App", tabName = "AbouttheApp", icon = icon("info-circle")),
                                shinydashboard::menuItem("Time Series Viewer", tabName = "OpenFlow", icon = icon("chart-line")),
                                shinydashboard::menuItem("Target Gauge Height", tabName = "CrossSection", icon = icon("bullseye")),
                                shinydashboard::menuItem("Real-Time Data Viewer", tabName = "gaugeHeight", icon=icon("water"))
                                )# End of sidebarMenu
    ),# End of dashboardSidebar
 
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
                               shiny::fluidRow(shiny::includeMarkdown('../README.md'))
                               ),# End fluid row for about the app
       
       # Tab Item timeseries viewer
       shinydashboard::tabItem(tabName= "OpenFlow",
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
                                                                                                plotly::plotlyOutput("plot1",height="800px")),
                                                                                shiny::tabPanel("Rating Curve(s)",
                                                                                                plotly::plotlyOutput("plot2",height="800px"))
                                                                                )# End tabsetPanel
                                                             )# End second column
                                               )# End fluid row for timeseries viewer,
                               ),# End timeseries viewer tab
       
       # Tab Item for cross section menu
       shinydashboard::tabItem(tabName ="CrossSection",
                               # Summary of Domain target datatable per site selected
                               shinydashboard::box(
                                 width=12,
                                 title= div(style = "text-align:center","Choose Site to View Current Gauge Height Targets for Opportunistic High-Flow Discharge Measurements"),
                                 solidHeader= TRUE,
                                 status= "primary",
                                 shiny::fluidRow(
                                   shiny::column(6,
                                                 shiny::selectInput(input= "XS_Domain", label= "Choose Domain",  choices = Target_df$Domain, selected = NULL)),# End column
                                   shiny::column(6,
                                                 shiny::selectInput(input= "XS_site",label = "Select Site ID", choices = Target_df$Site, selected= NULL))# End column
                                   
                                 ),# End fluidRow
                                 shiny::fluidRow(
                                   shiny::column(6,
                                                 tags$h3("Target Gauge Height (m):",shiny::renderText("targetGaugeHeight")),
                                                 tags$h3("Typrical High Flow Period:",shiny::renderText("highFlowPeriod"))),# End column
                                   shiny::column(6,
                                                 tags$h3("Target + 10% (m):",shiny::renderText("targetGaugeHeightPlus10")),
                                                 tags$h3("Target - 30% (m):",shiny::renderText("targetGaugeHeightMinus30"))),# End column
                                 )# End fluidRow
                                 ### LEFT OFF HERE ###
                               )),
       
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
                                          fileInput(inputId = "grafanaFile", label = "Upload the Grafana .csv file"),
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
                                                      title = "Calculated Heights",
                                                      #Shows plotly output of water depth with time series
                                                      div(id = "Title_CWE",(h3("Calculated Water Elevation"))),
                                                      div(id = "Title_CSH",(h3("Calculated Stage Height"))),
                                                      # plotlyOutput("calculatedStagePlot"),
                                                      
                                                      # plotlyOutput("rtdvStageDischargePlotly"),
                                                      # bscols(
                                                      #   plotlyOutput("ratingCurvePlotly"),
                                                      #   plotlyOutput("waterHeightInteractive")
                                                      # ),
                                                      
                                                      bscols(widths = c(12, 12,12),
                                                             div(plotlyOutput("rtdvStageDischargePlotly"), style = css(width="100%", height="300px")),
                                                             div(plotlyOutput("ratingCurvePlotly"), style = css(width="100%", height="300px")),
                                                             div(plotlyOutput("waterHeightInteractive"), style = css(width="100%", height="300px"))
                                                      ),
                                                      #verbatimTextOutput("hover_info"),
                                                      
                                                      div( id = "singleOutputBox",
                                                           box( width = 10,
                                                             textOutput("singleWaterColumnHeight"), tags$head(tags$style("#singleWaterColumnHeight{font-size: 20px;}")),
                                                             textOutput("EstimatedDischarge"), tags$head(tags$style("#EstimatedDischarge{font-size: 20px;}"))
                                                           )
                                                           )
                                                      
                                                      )
                                              )
                                        )
                               
                                    
          )
      ) 
  )
)
  
  
