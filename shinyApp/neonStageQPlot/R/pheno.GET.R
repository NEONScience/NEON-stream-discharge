##############################################################################################
#' Phenocam GET request
#' @name phenocamGET

#' @author
#' James Ross \email{ross.james94@gmail.com} \cr

#' @description  Uses given siteID, domainID, and dateTime to retrieve url to a phenocam image.
#' Closest to given parameters.

#' @param siteID Required: SiteID from app [string]
#' @param domainID Required: domainID from app [string]
#' @param dateTime Required: dateTime generated from plotly click event [string]

#' @return This function returns a url to a phenocam image. If no image available returns NULL.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export phenocamGET

# changelog and author contributions / copyrights
#   James M. Ross
#     original creation
##############################################################################################

options(stringsAsFactors = F)

phenocamGET <- function(siteID,domainID,dateTime){
  # ###Test GET
  # siteID <- "PRIN"
  # domainID <- "D11"
  # #UTC dateTime
  # dateTime <- "2021-12-01T18:00:00Z"

  phenoGET <- httr::content(httr::GET(url = paste0("https://phenocam.nau.edu/neonapi/imageurl/NEON.",domainID,".",siteID,".DP1.20002/",dateTime,"/")),
                            encoding = "UTF-8")

  if(is.null(phenoGET$url)){
    phenoURL <- NULL
  }
  else{
    phenoURL <- phenoGET$url
  }
  return(phenoURL)
}
