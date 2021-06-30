#- Using ggplot (not plotly), plot the continuous maxpostDischarge and discrete 
#streamDischarge variables in a single timeseries plot

# Load required libraries
library(neonUtilities)  
library(ggplot2) 
library(dplyr)  
library(tidyr)
library(shiny)

#user assigned variables
site = "LEWI"


#fluidPage
ui <- fluidPage(
  tags$header(
    title= "First Shiny App",
    h1("Stage & Continuous Discharge", align = " center")
  ),
  
# tags$p("Now choose a neon site and  a data Product ID" )
  
 titlePanel( tags$em("Choose and Customize")),
 
   sidebarLayout(
     

     sidebarPanel(
       selectInput("select", h3("Select box"), 
                   choices = list("Choice 1" = LEWI, "Choice 2" = POSE,
                                  "Choice 3" = BLDE,"Choice 4" = REDB,"Choice 5" = WALK,"Choice 6" = LECO,
                                  "Choice 7" = GUIL,"Choice 8" = CUPE,"Choice 9" = BLUE,"Choice 10" = BLDE,
                                  "Choice 11" = HOPB,"Choice 12" = MCDI,"Choice 13" = KING,"Choice 14" = TECR,
                                  "Choice 15" = ARIK,"Choice 16" = COMO,"Choice 17" = WLOU,"Choice 18" = BIGC,
                                  "Choice 19" = CARI,"Choice 20" = MAYF), selected =1)),
     
       
       dateRangeInput("dates", h3("Start and End"))
     ),
     
    
     mainPanel(
       plotOutput("distPlot")
     )
   )
 


#-------------------
#server function
server <- function(input, output) {
  
}

#---------------------------
#shiny App
shinyApp(ui = ui, server = server)


#-------------------------- 


#downloading continuous discharge
conti <- loadByProduct("DP4.00130.001", site= site, 
                       token = Sys.getenv("NEON_TOKEN"),
                       package = "expanded",
                       startdate="2018-10", enddate="2019-09",
                       check.size=F)

#downloading stage discharge
stage <- loadByProduct("DP4.00133.001", site=site, 
                       token = Sys.getenv("NEON_TOKEN"),
                       package = "expanded",
                       startdate="2018-10", enddate="2019-09",
                       check.size=F)
# unlist all dataframes to  
list2env(conti, .GlobalEnv)
list2env(stage, .GlobalEnv)

#split guageEventID
sdrc_gaugeDischargeMeas <- sdrc_gaugeDischargeMeas%>%
separate(gaugeEventID, c("eventSite", "date"), 5, remove = F) %>% 
  mutate(date=paste0(as.Date(date, format="%Y%m%d"), " 12:00: 00"))


#merge stage and continuous
newDataSet <- merge(csd_continuousDischarge,sdrc_gaugeDischargeMeas,by.x = "endDate",by.y = "date",all = T) %>% 
  select(endDate, maxpostDischarge,gaugeHeight,stageUnc, equivalentStage, streamDischargeUnc, streamDischarge,withRemnUncQUpper2Std,withRemnUncQLower2Std,
         withParaUncQUpper2Std,withParaUncQLower2Std) 


#make tempPlot an object that  calls itself(recursion)
(tempPlot <- newDataSet%>%
    
    ggplot(aes(x=as.POSIXct(endDate),y=maxpostDischarge))+
    
    geom_line(color='green')+
    
    geom_point(aes(y=streamDischarge), color='red')+
    
    labs(title="Stage and Continuous Discharge",
         
         x="Date",
         
         y="Discharge")+
    
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 20),
          
          text = element_text(size=18)))


tempPlot









#fluidPage
ui <- fluidPage(
  "hello world",
  #*Input()functions,
  #*Output()functions
  
  # actionButton(inputId = "clicks", 
  #              label = "Click me"),
  actionButton(inputId = "go", 
               label = "Update"),
  
  sliderInput(inputId = "num", label = "choose a number",
              value = 20, min = 1, max = 100),
  
  plotOutput(outputId = "hist"),
  verbatimTextOutput("stats")
)


#server function
server <- function(input, output) {
  # output$hist <-  renderPlot(
  #   {
  #     hist(rnorm(100))
  #   })
  #OR
  # output$hist <-  renderPlot(
  #   {
  #     title <- "100 random normal values"
  #     hist(rnorm(100), main = title)
  #   })
  #OR
  
  # output$hist <- renderPlot({
  #   title <- "100 random normal values"
  #   hist(rnorm(input$num), main = title)
  # })
  
  #actionButton
  # observeEvent(input$clicks, {
  #   print(as.numeric(input$clicks))
  # })
  
  #observe
  #observe({print(input$clicks)})
  
  # reactive expression
  data <- reactive({
    rnorm(input$num)
  })
  
  #evenReactive() used to halt ant server job untill user clicks update 
  data <- eventReactive(input$go,{
    rnorm(input$num)
  })
  
  output$hist <- renderPlot({
    title <- "100 random normal values"
    hist(rnorm(data()), main = title)
  })
  
  output$stats <-  renderPrint({
    summary(data())
  })
  
  
}

#shiny App
shinyApp(ui = ui, server = server)


# # 04-isolate
# 
# library(shiny)
# 
# ui <- fluidPage(
#   sliderInput(inputId = "num", 
#               label = "Choose a number", 
#               value = 25, min = 1, max = 100),
#   textInput(inputId = "title", 
#             label = "Write a title",
#             value = "Histogram of Random Normal Values"),
#   plotOutput("hist")
# )
# 
# server <- function(input, output) {
#   output$hist <- renderPlot({
#     hist(rnorm(input$num), main = isolate(input$title))
#   })
# }
# 
# shinyApp(ui = ui, server = server)