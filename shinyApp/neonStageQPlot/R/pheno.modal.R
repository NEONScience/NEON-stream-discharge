##############################################################################################
#' Phenocam modal

#' @name phenoModal

#' @author
#' James Ross \email{ross.james94@gmail.com} \cr

#' @description  Displays appropriate modal for phenoGET functionality.

#' @return This function returns nothing

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
    showModal(modalDialog(
      title = "Phenocam Image",
      "To download the image right click on the image and click 'Save image as...'",
      size = "l",
      tags$img(
        src = phenoURL),
      easyClose = TRUE))
  }
  else{
    showModal(modalDialog(
      title = "Phenocam Image",
      "No phenocam image available at ",siteID," for Date/Time",usrDateTime,
      size = "s",
      easyClose = TRUE))
  }
}
