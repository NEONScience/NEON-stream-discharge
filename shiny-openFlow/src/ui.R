ui<- shinydashboard::dashboardPage(
  hader <- shinydashboard::dashboardHeader(title = "openFlow"),

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
       
       # #Tab Item for About the App
       # shinydashboard::tabItem(tabName= "AbouttheApp",
       #                         shiny::fluidRow(shiny::includeMarkdown('../README.md'))
       #                         ),# End fluid row for about the app
       
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
                                                             shinyjs::hidden(shiny::checkboxInput("dark_mode", "Show in Dark Mode")),
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
                                   tags$br(),
                                   shiny::fluidRow(
                                     shiny::column(6,
                                                   plotly::plotlyOutput("rc_targetGAG")),# End column
                                     shiny::column(6,
                                                   plotly::plotlyOutput("xs_targetGAG"))# End column
                                   ),# End fluidRow
                                   shinydashboard::box(
                                     width = 12,
                                     title= "Click To View Guide for Interpreting the 'Target Gauge Height' Tab",
                                     solidHeader= F,
                                     status= "primary",
                                     collapsible = T,
                                     collapsed = T,
                                     tags$h4(shiny::HTML('<b>Background: </b>')),
                                     tags$p(shiny::HTML("The required number of annual discharge bouts at AQU stream and lake inflow/outflow sites has been reduced from 24 to 22.  With this change, Domains are to attempt to collect 2 additional measurements each year during periods of high flow, when water levels are within +10% to -30% of the designated target stage. Targeted discharge bouts are a high priority when a site is experiencing high flows (see <a href=\"https://neoninc.sharepoint.com/:x:/s/IntegratedProductTeams/ERYReQKu0I9OgxvY9jmpo5UBkNz_cc86rABGnCLVNcKhZA?e=DZFNXL\" target=\"_blank\">Field Prioritization Matrix, OSProcedure-specific-priorities</a> for details).")),
                                     tags$p("High flows near bankfull stage historically occur on 1.5 year intervals, though this frequency is expected to change as extreme weather events (e.g. heat waves and large storms) become more frequent and more intense. Field Science staff should not enter the channel when high flows are present; ADCP instrumentation should always be used to measure high flow discharge as it can be safely deployed from the floodplain. Contact Science with any questions regarding the target gauge height values, the time periods in which they historically occur, and/or strategies to assist with forecasting these stochastic events."),
                                     tags$h4(shiny::HTML('<b>Reported Values: </b>')),
                                     tags$p("The reported values provides the range of staff gauge heights at which high flow discharge measurements should be targeted and the time period in which they have historically occurred. Note that target gauge height values will change if staff gauges are replaced (Science will then update the values following a total station survey of the staff gauge and the discharge cross-section)."),
                                     tags$h4(shiny::HTML('<b>The Visuals: </b>')),
                                     tags$p(shiny::HTML("<b>Rating Curve Plot</b> - The lefthand plot shows the most recently-published stage discharge rating curve with the target gauge range higlighted in <b><span style=\"color: darkgreen;\">green</span></b> showing (from lower to higher stage) the -30% threshold, the target gauge height, and the +10% threshold.")),
                                     tags$p(shiny::HTML("<b>Discharge Cross Section Plot</b> - The righthand plot shows the most recently-surveyed discharge cross-section. The cross section is 'filled' to the target gauge height, with the horizontal black bars showing the -30% - +10% range of gauge heights."))
                                   )# End box
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