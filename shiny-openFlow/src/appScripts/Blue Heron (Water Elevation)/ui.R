# Define UI
fluidPage(
  useShinyjs(),
  img(src = "blue_heron_title2.png", width = "1920px", height = "320px", style = "padding-bottom: 25px;"), #can add width: 100%; to fill entire screen
  # Application title
  # titlePanel("Blue Heron"),
  
  # Sidebar with option items
  sidebarLayout(
    sidebarPanel(
      #Input for selecting domain
      selectizeInput(input = "domain", label = "Choose a domain", choices = unique(domainSite_info$field_domain_id)),
      #Input for selecting site based off the domain selected
      selectizeInput(input = "site", label = "Choose a site", choices = NULL),
      #Input for selecting the type of water GW = Groundwater and SW = Surfacewater
      selectInput(input = "waterType", label = "Choose GW or SW", choices = c("GW","SW")),
      h5("GW is for groundwater and SW is for surface water"),
      #Input for selecting between an AquaTROLL and LevelTROLL depending what is installed at the SW location
      selectInput(input = "trollType", label = "What kind of Troll is it?", choices = c("AquaTroll","LevelTroll")),
      ##Input for selecting HOR location, techs will likely know which theirs is
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
      div(id = "LB",progressBar(id = "spatialDataLB", value = spatialDataLB_percent, title = spatialDataLB_title)),
      actionButton(inputId = "run", label = "Run")
    ),
    
    mainPanel(
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
          textOutput("singleWaterColumnHeight"), tags$head(tags$style("#singleWaterColumnHeight{font-size: 20px;}")))
      )

    )#End mainPanel
  )#End sidebarLayout
) #End fluidPage
