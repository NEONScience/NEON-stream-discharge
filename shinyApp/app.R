##############################################################################################
#' NEON continuous discharge and stage-discharge rating curve data visualization app

#' @name run.RC.cont.Q.plot

#' @author
#' Divine Aseaku \email{divineaseaku@gmail.com} \cr
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr
#' James Ross \email{ross.james94@gmail.com} \cr

#' @description  An interactive app which  plots continuous discharge, Stage discharge and
#' rating curves using language R Data is downloaded from the NEON data portal using
#' neonUltilities and ploting is done at 20mins time interval

#' @return This function launches a shiny app to interactively plot published NEON data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Divine Aseaku (2021-08-04)
#     original creation
#   Zachary L. Nickerson (2021-09-16)
#     updates for plotting provisional 2021 data and TOOK inlet and outlet
#   Zachary L. Nickerson (2022-02-04)
#     updates for plotting TOMB-USGS discharge data
#   Zachary L. Nickerson (2022-06-20)
#     updates that move code chunks in server to functions
#   James M. Ross (2022-06-23)
#     added API Token functionality
#   James M. Ross (2022-07-01)
#     added phenocam functionality
#   James M. Ross (2022-07-01)
#     added phenocam and plot download
##############################################################################################
# # Source packages and set options
options(stringsAsFactors = F)

library(shiny)
library(plotly)
library(neonUtilities)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(lubridate)
library(stageQCurve)
library(neonStageQplot)
library(stringr)
library(dplyr)
library(readr)
library(tidyr)
library(htmlwidgets)
library(httr)
library(bslib)


  # Read in reference table from Github
  # setwd("~/Github/NEON-stream-discharge/L4Discharge/AOSApp") # Code for testing locally - comment out when running app
  #Global Vars
  productList <- readr::read_csv(base::url("https://raw.githubusercontent.com/NEONScience/NEON-stream-discharge/master/shinyApp/aqu_dischargeDomainSiteList.csv"))
  siteID <- NULL
  domainID <- NULL

  light <- bs_theme(version = 3,bootswatch = "flatly")
  dark <- bs_theme(version = 3,bootswatch = "darkly")
  # Develop the User Interface
  ui <- shiny::fluidPage(theme = bs_theme(version = 3,bootswatch = "flatly"),
                         style = "padding:25px;",
                         tags$head(tags$style("#shiny-modal img { max-width: 100%; }")),#####modal scaling
                         shiny::titlePanel(shiny::fluidRow(shiny::column(10, img(src = "applogo-transparent.png",width = 250,height = 150)),
                                                           shiny::column(2, img(src = "logo-NEON-NSF.png",width = 250,height = 150)))),
                         shiny::fluidRow(shiny::column(2,
                                         shiny::fluidRow(shiny::selectInput("domainId","Domain ID",productList$domain)),
                                         shiny::fluidRow(shiny::uiOutput("domainInfo")),
                                         shiny::br(),
                                         shiny::fluidRow(shiny::selectInput("siteId","Select Site ID",NULL)),
                                         shiny::fluidRow(shiny::uiOutput("siteInfo")),
                                         shiny::br(),
                                         shiny::fluidRow(shiny::dateRangeInput("dateRange","Date range:",
                                                                               startview="month",
                                                                               min="2016-01-01",
                                                                               start="2019-01-01",end="2019-01-31",
                                                                               format="yyyy-mm-dd")),
                                         shiny::br(), 
                                         shiny::fluidRow(shiny::textInput("apiToken", "NEON API Token (Optional)")),
                                         shiny::fluidRow(shiny::actionButton(inputId="submit","Submit")),
                                         shiny::br(),                
                                         shiny::fluidRow(shiny::checkboxInput("qctrFlag", "Include Final Quality Flag", FALSE),
                                                         shiny::checkboxInput("qctrFlagScRv", "Include Science Review Quality Flag", FALSE),
                                                         shiny::checkboxInput("dark_mode", "Dark Mode")),
                                         shiny::hr(),
                                         shiny::fluidRow(conditionalPanel(
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
                                                                            shinycssloaders::withSpinner(plotly::plotlyOutput("plot2",height="800px")
                                                                                                         ))))#end of second col
                           )#end of fluid row
    ) # end of ui and fluidPage


  #server function
  server <- function(session, input, output) {

    # Select site ID based on the domain ID chosen
    shiny::observe({x <- productList$siteID[productList$domain == input$domainId]
    shiny::updateSelectInput(session,"siteId",choices = unique(x))})

    #handles light and dark mode switch
    observe(session$setCurrentTheme(
      if (isTRUE(input$dark_mode)) dark else light
    ))
    
    #phenoImage observe
    #displays phenocam image when point is clicked on graph
    #pulls image closest to selected date
    observe({
      new_clickEvent <- plotly::event_data(event = "plotly_click", source = "phenoDate")

      if (!base::is.null(new_clickEvent)) {
        #formats date & time for phenocamGet
        dateTime <- stringr::str_replace(new_clickEvent$x, " ","T")
        dateTime <- base::paste0(dateTime,":00Z")
        #returns url for phenocam image
        phenoURL <- neonStageQplot::pheno.GET(dp.id="DP1.20002",
                                              site.id=siteID,
                                              domain.id=domainID,
                                              date.time=dateTime)
        #formats date & time for bad request modal
        usrDateTime <- dateTime
        usrDateTime <- stringr::str_replace(usrDateTime, "T"," ")
        usrDateTime <- base::substr(usrDateTime,1,base::nchar(usrDateTime)-4)

        if(!base::is.null(phenoURL)){
          phenoInfo <<- createPhenoInfo(phenoURL,usrDateTime)
          shiny::showModal(shiny::modalDialog(
            title = "Phenocam Image",
            size = "l",
            tags$img(
              src = phenoURL),
            footer = shiny::downloadButton("downloadPheno",label = "Download Phenocam Image"),
            easyClose = TRUE))
        }
        else{
          shiny::showModal(shiny::modalDialog(
            title = "Phenocam Image",
            "No phenocam image available at ",siteID," for Date/Time",usrDateTime,
            size = "s",
            easyClose = TRUE))
        }
      }
    })

    output$downloadPheno <- shiny::downloadHandler(
      filename = function() {
        base::paste0("NEON.",domainID,".",siteID,".","DP1.20002","_",phenoInfo$dateTime,".jpg")
      },
      content = function(file) {
        utils::download.file(phenoInfo$URL,file,mode='wb')
      }
    )

    #gets phenocam info for download handler
    phenoInfo <- NULL
    createPhenoInfo <- function(phenoURL,usrDateTime){
      usrDateTime <- stringr::str_replace(usrDateTime, " ","_")
      usrDateTime <- stringr::str_replace(usrDateTime, ":","-")
      usrDateTime <- base::paste0(usrDateTime,"-UTC")
      phenoInfo <- list("URL" = phenoURL, "dateTime" = usrDateTime)
      return(phenoInfo)
    }

    shiny::observeEvent(input$siteId,{
      # Create site description output
      siteURL <- base::paste0("https://www.neonscience.org/field-sites/",base::tolower(input$siteId))
      domainURL <- base::paste0("https://www.neonscience.org/field-sites/about-field-sites")
      siteLink <- a("Click here", href=siteURL,target="_blank")
      domainLink <- a("Click here", href=domainURL,target="_blank")
      output$siteInfo <- shiny::renderUI({tagList("Site: ",input$siteId, siteLink, "for site description",sep="\n")})
      output$domainInfo <- shiny::renderUI({tagList("Domain: ", domainLink, "for domain map and info",sep="\n")})
    })
    
    # Download data, create summary table, and save output
    getPackage <- shiny::eventReactive(input$submit,{

      metaD <-  productList%>%
        dplyr::filter(siteID == input$siteId)%>%
        dplyr::select(upstreamWatershedAreaKM2,reachSlopeM,averageBankfullWidthM,d50ParticleSizeMM)%>%
        dplyr::rename("Upstream watershed area (km^2)"= upstreamWatershedAreaKM2,
                      "Reach slope (m)" = reachSlopeM,
                      "Mean bankfull width (m)"= averageBankfullWidthM,
                      "D50 particle size (mm)"=d50ParticleSizeMM) %>%
        dplyr::mutate_all(as.character)%>%
        tidyr::pivot_longer(c("Upstream watershed area (km^2)","Reach slope (m)","Mean bankfull width (m)","D50 particle size (mm)"),
                            names_to = "MetaData",
                            values_to = "Values")

      # Enter header for metadata table
      output$title <- shiny::renderText("Metadata Table")

      # Create metadata table output
      output$table <- DT::renderDataTable({dat <- DT::datatable(metaD,  options = list(dom = 't'))},selection = 'single')

      # # Manually set input variables for local testing - comment out when running app
      # input <- base::list()
      # input$siteId <- "TOOK_inlet"
      # input$domainId <- "D18"
      # input$dateRange[[1]] <- "2020-09-01"
      # input$dateRange[[2]] <- "2020-10-31"
      # apiToken <- NA

      # Set date variables for app running (special consideration for TOOK)
      siteID <<- input$siteId
      domainID <<- input$domainId
      startDate <- base::format(input$dateRange[1])
      endDate <- base::format(input$dateRange[2])
      apiToken <- input$apiToken

      #progress bar for data downloads
      shiny::withProgress(message = 'Submit',detail = '', min = 0, max = 1 ,value = 0, {

        shiny::incProgress(amount = 0.50,
                           message = "Pulling data from neonUtilities",

                           detail = NULL,
                           session = shiny::getDefaultReactiveDomain())
        base::Sys.sleep(0.25)

        # Download and process NEON data
        continuousDischarge_list <- neonStageQplot::get.cont.Q.NEON.API(site.id = siteID,
                                                                        start.date = startDate,
                                                                        end.date = endDate,
                                                                        api.token = apiToken)
      })#end of withProgress


    },ignoreInit = T)# End getPackage

    plots <- shiny::reactiveValues()
    whichTab <- shiny::reactiveValues()

    #download the correct graph according to tab
    shiny::observeEvent(input$selectedTab, {
      whichTab$currentTab = input$selectedTab
    })

    # Plotting continuous discharge with uncertainty
    output$plot1 <- plotly::renderPlotly({

      # Unpack the data frame from getPackage
      continuousDischarge_list <- getPackage()

      # Format QF inputs

      if(input$qctrFlag == TRUE){
        finalQfInput <- T
      }else{
        finalQfInput <- F
      }
      if(input$qctrFlagScRv == TRUE){
        sciRvwQfInput <- T
      }else{
        sciRvwQfInput <- F
      }
      if(input$dark_mode == TRUE){
        darkModeInput <- T
      }
      else{
        darkModeInput <- F
      }

      # Plot continuous discharge and store in output
      plots$plot.cont.Q <- neonStageQplot::plot.cont.Q(site.id = input$siteId,
                                                       start.date = input$dateRange[[1]],
                                                       end.date = input$dateRange[[2]],
                                                       input.list = continuousDischarge_list,
                                                       plot.final.QF = finalQfInput,
                                                       plot.sci.rvw.QF = sciRvwQfInput,
                                                       mode.dark = darkModeInput)

      # plot_csdWebGL <- plots$plot.cont.Q %>% toWebGL()
      #
      # plot_csdWebGL

    })# End plot1

    # Plotting rating curve(s) with uncertainty
    output$plot2 <- plotly::renderPlotly({

      # Unpack the list of curve IDs from getPackage
      continuousDischarge_list <- getPackage()


      # Plot rating curve(s) and store in outputs
      plots$plot.RC <- neonStageQplot::plot.RC(site.id = input$siteId,
                                               start.date = input$dateRange[[1]],
                                               end.date = input$dateRange[[2]],
                                               input.list = continuousDischarge_list)
    })# End plot2

    #download handler for plotly download functionality
    output$downloadPlotly <- shiny::downloadHandler(
      filename = function() {
        downloadParam <- whichPlot()
        #file name format NEON.DOMAIN.SITE.DP4.0013[0,3]_STARTDATE_ENDDATE.html
        base::paste0("NEON.",domainID,".",siteID,".",downloadParam$dpName,"_",input$dateRange[1],"_",input$dateRange[2],".html")
      },
      content = function(file) {
        downloadParam <- whichPlot()
        shiny::withProgress(
          message = paste0("Downloading Plot to HTML file"),
          value = 0,
          {
            shiny::incProgress(1/10)
            base::Sys.sleep(1)
            shiny::incProgress(5/10)
            htmlwidgets::saveWidget(as_widget(plotly::partial_bundle(downloadParam$plotToWidget)), file, selfcontained = TRUE)
          }
        )
      }
    )
    #sends the correct plot and data package name to download handler
    whichPlot <- function(){
      if(whichTab$currentTab == "Continuous Discharge"){
      downloadParam <- base::list("plotToWidget" = plots$plot.cont.Q, "dpName" = "DP4.00130")
    }
      else{
        downloadParam <- base::list("plotToWidget" = plots$plot.RC, "dpName" = "DP4.00133")
      }
      return(downloadParam)
    }

  }#end of server



  # Run the app ----
  shiny::shinyApp(ui = ui, server = server)





