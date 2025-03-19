#Xs Section branch
######Maria Viggiano###
    #skin= "green",
    #theme = bslib::bs_theme(),
ui<- shinydashboard::dashboardPage(
  hader <- shinydashboard::dashboardHeader(title = "Open Flow"),

  #Sidebar menu                
  sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(shinydashboard::menuItem("About the App", tabName = "AbouttheApp", icon = icon("info-circle")),
                                shinydashboard::menuItem("Time Series Viewer", tabName = "TimeseriesViewer", icon = icon("chart-line")),
                                shinydashboard::menuItem("Target Gauge Height", tabName = "TargetGAG", icon = icon("bullseye")),
                                shinydashboard::menuItem("Real-Time Data Viewer", tabName = "RealTime", icon=icon("water"))
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
       shinydashboard::tabItem(tabName= "TimeseriesViewer",
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
       shinydashboard::tabItem(tabName ="TargetGAG",
                               shiny::fluidRow(
                                 # Summary of Domain target datatable per site selected
                                 shinydashboard::box(
                                   width=12,
                                   title= div(style = "text-align:center","Choose Site to View Current Gauge Height Targets for Opportunistic High-Flow Discharge Measurements"),
                                   solidHeader= TRUE,
                                   status= "primary",
                                   shiny::fluidRow(
                                     shiny::column(6,
                                                   shiny::selectInput(input= "XS_Domain", label= "Choose Domain",  choices = Target_df$domainID)),# End column
                                     shiny::column(6,
                                                   shiny::selectInput(input= "XS_site",label = "Select Site ID", NULL))# End column
                                     ),# End fluidRow
                                   div(style = "text-align:center",shiny::actionButton("showResultsTargetGAG","Show Results & Plots")),
                                   shiny::fluidRow(
                                     shiny::column(6,
                                                   tags$h3(shiny::htmlOutput("targetGaugeHeight")),
                                                   tags$h3(shiny::htmlOutput("highFlowPeriod"))),# End column
                                     shiny::column(6,
                                                   tags$h3(shiny::htmlOutput("targetGaugeHeightPlus10")),
                                                   tags$h3(shiny::htmlOutput("targetGaugeHeightMinus30")))# End column
                                     ),# End fluidRow
                                   shiny::fluidRow(
                                     shiny::column(6,
                                                   plotly::plotlyOutput("rc_targetGAG")),# End column
                                     shiny::column(6,
                                                   plotly::plotlyOutput("xs_targetGAG"))# End column
                                   )# End fluidRow
                                   )# End box
                                 )# End fluid row for target gauge height viewer
                               ),# End TargetGAG tab
       
       #Tab Item for Blue Heron
       shinydashboard::tabItem(tabName= "RealTime",
                               shiny::fluidRow(shiny::column(2,
                                                             shiny::selectizeInput("rtdvDomain","Domain ID", choices = productList$domain),##replaced the choices from unique(domainSite_info$field_domain_id
                                                             shiny::selectizeInput("rtdvSite","Select Site ID", choices = NULL),
                                                             shinyjs::hidden(shiny::radioButtons("waterType","Surface or Ground?", choices = c("Surface Water","Groundwater"),selected = "Surface Water")),
                                                             shiny::radioButtons("trollType","Select TROLL Type", choices = c("LevelTroll","AquaTroll")),
                                                             # shiny::selectInput("HOR","Choose Location (HOR)", choices = NULL),
                                                             # dateRangeInput(inputId = "rtdvDaterange", label = "Select date range of pressure reading(s)", start = Sys.Date()-2, end = Sys.Date()-1, max = Sys.Date()-1),# ZN 2024-01-03 -- disabling L0 data query because this functionality will exist in the regular timeseries viewer
                                                             shiny::radioButtons("dataSource","Select Data Input Type",choices = c("Single Staff Gauge Reading","Single Pressure Reading","Grafana CSV File")),
                                                             shinyjs::hidden(shiny::textInput("singleStaffGauge","Enter Staff Gauge Reading")),
                                                             shinyjs::hidden(shiny::textInput("singlePressure","Enter Pressure Reading")),
                                                             shinyjs::hidden(shiny::fileInput("grafanaFile","Upload Grafana CSV file")),
                                                             actionButton(inputId = "rtdvRun", label = "Submit")
                                                             ),# End input column
                                               shiny::column(10,
                                                             shiny::uiOutput("realTimeUIOutput")
                                                             )# End output column
                               )# End of fluid row for real-time data viewer
                               )# End of real-time data viewer tab item
       )# End of all shinydashboard tab items
  )# End of shinydashboard body
)# End of shinydashboard page