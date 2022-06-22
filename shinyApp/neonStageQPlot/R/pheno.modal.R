##############################################################################################
#' Phenocam modal

#' @name phenoModal

#' @author
#' James Ross \email{ross.james94@gmail.com} \cr

#' @description  Displays appropriate modal for phenoGET functionality.

#' @return This function returns a modal object based on phenoGET return.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export phenoModal
# # changelog and author contributions / copyrights
#   James M. Ross
#     original creation
##############################################################################################

options(stringsAsFactors = F)

phenoModal <- function(phenoURL,usrDateTime,isGoodRequest,siteID){

  if(isGoodRequest==TRUE){
    return(phenoModalGood(phenoURL))
  }
  else{
    print("bad modal")
    return(phenoModalBad(usrDateTime,siteID))
  }
}

##############################################################################################
#' Phenocam modal good request
#' @name phenoModalGood

#' @author
#' James Ross \email{ross.james94@gmail.com} \cr

#' @description  Displays appropriate good request modal for phenoGET functionality.

#' @return This function returns a modal object based on phenoGET return.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' @export phenoModalGood
# # changelog and author contributions / copyrights
#   James M. Ross
#     original creation
##############################################################################################
phenoModalGood <- function(phenoURL)
{modalDialog(
  title = "Phenocam Image",
  "To download the image right click on the image and click 'Save image as...'",
  size = "l",
  tags$img(
    src = phenoURL),
  easyClose = TRUE)}

##############################################################################################
#' Phenocam modal bad request
#' @name phenoModalBad

#' @author
#' James Ross \email{ross.james94@gmail.com} \cr

#' @description  Displays appropriate bad request modal for phenoGET functionality.

#' @return This function returns a modal object based on phenoGET return.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' @export phenoModalBad
# # changelog and author contributions / copyrights
#   James M. Ross
#     original creation
##############################################################################################
phenoModalBad <- function(usrDateTime,siteID)
{modalDialog(
  title = "Phenocam Image",
  "No phenocam image available at ",siteID," for Date/Time",usrDateTime,
  size = "s",
  easyClose = TRUE)}
