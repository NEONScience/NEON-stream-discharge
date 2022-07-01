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

#' @export run.RC.cont.Q.plot

# changelog and author contributions / copyrights
#   Divine Aseaku (2021-08-04)
#     original creation
#   Zachary L. Nickerson (2021-09-16)
#     updates for plotting provisional 2021 data and TOOK inlet and outlet
#   Zachary L. Nickerson (2022-02-04)
#     updates for plotting TOMB-USGS discharge data
#   Zachary L. Nickerson (2022-06-20)
#     updates that move code chunks in server to functions
#   James M. Ross (2022-07-01)
#     added phenocam functionality
##############################################################################################
# # Source packages and set options
options(stringsAsFactors = F)

run.RC.cont.Q.plot <-function(){

  # Read in refernce table from Github
  # setwd("~/Github/NEON-stream-discharge/L4Discharge/AOSApp") # Code for testing locally - comment out when running app
  #Global Vars
  productList <- readr::read_csv(base::url("https://raw.githubusercontent.com/NEONScience/NEON-stream-discharge/master/shinyApp/aqu_dischargeDomainSiteList.csv"))
  siteID <- NULL
  domainID <- NULL

  # Develop the User Interface
  ui <- shiny::fluidPage(style = "padding:25px; margin-bottom: 30px;",
                         tags$head(tags$style("#shiny-modal img { max-width: 100%; }")),#####modal scaling
                         shiny::titlePanel("NEON Continuous discharge (DP4.00130.001) and Stage-discharge rating curves (DP4.00133.001) data visualization application"),
                         shiny::fluidRow(shiny::column(3,
                                         shiny::fluidRow("Welcome! This application allows you view and interact with NEON's Continuous discharge",tags$a(href="https://data.neonscience.org/data-products/DP4.00130.001", "(DP4.00130.001)", target="_blank"), "and Stage-discharge rating curves",tags$a(href="https://data.neonscience.org/data-products/DP4.00133.001", "(DP4.00133.001)", target="_blank")," data products. Select a site and date range and the app will download data from the NEON Data Portal and plot continuous and discrete stage and discharge timeseries data and all rating curves used in the development of the timeseries data."),
                                         shiny::fluidRow(style = "background-color:#F8F8F8; height:auto;margin-top: 15px;padding: 15px;",
                                                         shiny::selectInput("domainId","Domain ID",productList$domain),
                                                         shiny::selectInput("siteId","Select Site ID",NULL),
                                                         shiny::dateRangeInput("dateRange","Date range:",
                                                                               startview="month",
                                                                               min="2016-01-01",
                                                                               start="2019-01-01",end="2019-01-31",
                                                                               format="yyyy-mm-dd"),
                                                         shiny::textInput("apiToken", "NEON API Token (Optional)"),
                                                         shiny::actionButton(inputId="submit","Submit"),
                                                         shiny::checkboxInput("qctrFlag", "Include Final Quality Flag", FALSE),
                                                         shiny::checkboxInput("qctrFlagScRv", "Include Science Review Quality Flag", FALSE),
                                                         shiny::hr(),
                                                         conditionalPanel(
                                                           #checks that one of the graphs has been loaded
                                                           condition = "output.plot1 != null || output.plot2 != null",
                                                           shiny::downloadButton("downloadPlotly", "Download Graph"))),
                                         shiny::hr(),
                                         shiny::fluidRow(shiny::uiOutput("siteInfo" )),
                                         shiny::hr(),
                                         shiny::fluidRow(shiny::textOutput("title"),
                                                         DT::dataTableOutput("table"))),#end of first col
                                         shiny::column(9,
                                         shiny::tabsetPanel(type = "tabs",id = "selectedTab",
                                                            shiny::tabPanel("Continuous Discharge",
                                                                            shinycssloaders::withSpinner(plotly::plotlyOutput("plot1",height="800px"),
                                                                                                         color = "#00ADD7"),
                                                                            style = "background-color:#F8F8F8;"),
                                                            shiny::tabPanel("Rating Curve(s)",
                                                                            shinycssloaders::withSpinner(plotly::plotlyOutput("plot2",height="800px"),
                                                                                                         color = "#00ADD7"))))#end of second col
                           )#end of fluid row
    ) # end of ui and fluidPage


  #server function
  server <- function(session, input, output) {

    # Select site ID based on the domain ID chosen
    shiny::observe({x <- productList$siteID[productList$domain == input$domainId]
    shiny::updateSelectInput(session,"siteId",choices = unique(x))})


    #phenoImage observe
    #displays phenocam image when point is clicked on graph
    #pulls image closest to selected date
    observe({
      new_clickEvent <- plotly::event_data(event = "plotly_click", source = "phenoDate")

      if (!is.null(new_clickEvent)) {
        #formats date & time for phenocamGet
        dateTime <- stringr::str_replace(new_clickEvent$x, " ","T")
        dateTime <- paste0(dateTime,":00Z")
        #returns url for phenocam image
        phenoURL <- phenocamGET(siteID,domainID,dateTime)
        #formats date & time for bad request modal
        usrDateTime <- dateTime
        usrDateTime <- stringr::str_replace(usrDateTime, "T"," ")
        usrDateTime <- substr(usrDateTime,1,nchar(usrDateTime)-4)

        isGoodRequest <- FALSE

        if(!is.null(phenoURL)){
          isGoodRequest <- TRUE
          phenoInfo <<- createPhenoInfo(phenoURL,usrDateTime)
          phenoModal(phenoURL,usrDateTime,isGoodRequest,siteID)

        }
        else{
          phenoModal(phenoURL,usrDateTime,isGoodRequest,siteID)
        }
      }
    })

    output$downloadPheno <- downloadHandler(
      filename = function() {
        paste("NEON.",domainID,".",siteID,".","DP1.20002","_",phenoInfo$dateTime,".jpg", sep="")
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
      usrDateTime <- paste0(usrDateTime,"-UTC")
      phenoInfo <- list("URL" = phenoURL, "dateTime" = usrDateTime)
      return(phenoInfo)
    }

    # Download data, create summary table, and save output
    getPackage <- shiny::eventReactive(input$submit,{

      # Run function
      metaD <- neonStageQplot::frmt.meta.data.df(input.list = input,
                                                 product.list = productList)

      # Enter header for metadata table
      output$title <- shiny::renderText("Metadata Table")

      # Create metadata table output
      output$table <- DT::renderDataTable({dat <- DT::datatable(metaD,  options = list(dom = 't'))},selection = 'single')

      # Manually set input variables for local testing - comment out when running app
      # input <- base::list()

      # input$siteId <- "MAYF"
      # input$dateRange[[1]] <- "2020-09-01"
      # input$dateRange[[2]] <- "2020-10-31"

      # Create site description output
      siteURL <- base::paste0("https://www.neonscience.org/field-sites/",base::tolower(input$siteId))
      url <- a("Click here", href=siteURL,target="_blank",style="text-decoration: none; hover:{font-size:150%;}")
      output$siteInfo <- shiny::renderUI({tagList("Site: ",input$siteId, url, "for site description",sep="\n")})

      # Set date variables for app running (special consideration for TOOK)
      siteID <<- input$siteId
      domainID <<- input$domainId
      apiToken <- input$apiToken
      startDate <- base::format(input$dateRange[1])
      endDate <- base::format(input$dateRange[2])


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

    plots <- reactiveValues()
    whichTab <- reactiveValues()

    #download the correct graph according to tab
    observeEvent(input$selectedTab, {
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

      # Plot continuous discharge and store in output
      plots$plot.cont.Q <- neonStageQplot::plot.cont.Q(site.id = input$siteId,
                                            start.date = input$dateRange[[1]],
                                            end.date = input$dateRange[[2]],
                                            input.list = continuousDischarge_list,
                                            plot.final.QF = finalQfInput,
                                            plot.sci.rvw.QF = sciRvwQfInput)
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
    output$downloadPlotly <- downloadHandler(
      filename = function() {
        downloadParam <- whichPlot()
        #file name format NEON.DOMAIN.SITE.DP4.0013[0,3]_STARTDATE_ENDDATE.html
        paste("NEON.",domainID,".",siteID,".",downloadParam$dpName,"_",input$dateRange[1],"_",input$dateRange[2],".html", sep = "")
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
      downloadParam <- list("plotToWidget" = plots$plot.cont.Q, "dpName" = "DP4.00130")
    }
      else{
        downloadParam <- list("plotToWidget" = plots$plot.RC, "dpName" = "DP4.00133")
      }
      return(downloadParam)
    }

  }#end of server



  # Run the app ----
  shiny::shinyApp(ui = ui, server = server)

}



