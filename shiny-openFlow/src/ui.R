
=======
ui<- dashboardPage(
    skin= "green",
    #theme = bslib::bs_theme(version = 4),
    header = dashboardHeader(
      title = "Open Flow"
      # style
      # tags$style(type= "text/css", ".main-header {padding:25px;}"),
      # tags$img(src = "logo-NEON-NSF.png",width = 200,height = 75)
      
      ),#####modal scaling
 # End of page header

  #Sidebar menu                
  sidebar = dashboardSidebar(
    shinydashboard::sidebarMenu(id = "menu",
      shinydashboard::menuItem("Time series viewer", tabName = "OpenFlow", icon = shiny::icon("sitemap")),
      shinydashboard::menuItem( "Blue Heron", tabName = "waterlevel", icon=shiny::icon("hourglass-half"))
      )
  ),
 
  
 #Body of each menu item
 body = dashboardBody(
   shinydashboard::tabItems(
     shinydashboard::tabItem(tabName= "OpenFlow",
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
                          shiny::tabPanel("About the App",
                                        shiny::includeMarkdown(readmeFile)))
                  )#end of second column
        ),#end of fluid row for open flow
  
    
      tabItem(tabName= "waterlevel")
           #img(src= #"Blue Heron-logo.png", width = 300,height = 150) 
      )
    )
  )                   


 

#### end of ui and fluidPage

>>>>>>> Stashed changes:shiny-openFlow/R/ui.R
