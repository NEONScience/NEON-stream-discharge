##############################################################################################
#' Get URL to render Phenocam image for a NEON site at a given timestamp
#' @name pheno.GET

#' @author
#' James Ross \email{ross.james94@gmail.com} \cr

#' @description  This function queries the Phenocam API to retrieve an image for the nearest
#' timestamp basedo on user input and prints the image as an output.

#' @param dpID Required: Phenocam DPID formatted as DP#.##### [string]
#' @param siteID Required: NEON site ID [string]
#' @param domainID Required: NEON domain ID [string]
#' @param dateTime Required: Date-time formatted as YYYY-MM-DDTHH:MM:SSZ [string]

#' @return This function returns a url to render a Phenocam image. If no image available, the
#' function returns NULL.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export pheno.GET

# changelog and author contributions / copyrights
#   James M. Ross
#     original creation
##############################################################################################

options(stringsAsFactors = F)

pheno.GET <- function(dp.id,site.id,domain.id,date.time){
  # ###Test GET
  # dp.id <- "DP1.20002"
  # site.id <- "PRIN"
  # domain.id <- "D11"
  # #UTC dateTime
  # date.time <- "2021-12-01T18:00:00Z"

  if(missing(dp.id)){
    stop('must provide dp.id to query Phenocam API')
  }
  if(missing(site.id)){
    stop('must provide site.id to query Phenocam API')
  }
  if(missing(domain.id)){
    stop('must provide domain.id to query Phenocam API')
  }
  if(missing(date.time)){
    stop('must provide date.time to query Phenocam API')
  }

  phenoGET <- httr::content(httr::GET(url = base::paste0("https://phenocam.nau.edu/neonapi/imageurl/NEON.",domain.id,".",site.id,".",dp.id,"/",date.time,"/")),
                            encoding = "UTF-8")

  if(base::is.null(phenoGET$url)){
    phenoURL <- NULL
  }else{
    phenoURL <- phenoGET$url
  }

  return(phenoURL)
}
