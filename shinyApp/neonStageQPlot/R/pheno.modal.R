#' @name phenoModal

#' @author
#' James Ross \email{ross.james94@gmail.com} \cr

#' @description  Displays appropriate modal for phenoGET functionality.

#' @return This function returns a modal object based on phenoGET return.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export phenoModal

options(stringsAsFactors = F)

phenoModal <- function(phenoURL,usrDateTime,isGoodRequest){

  if(isGoodRequest==TRUE){
    return(phenoModalGood(phenoURL))
  }
  else{
    return(phenoModalBad(usrDateTime))
  }
}

#' @name phenoModalGood

#' @author
#' James Ross \email{ross.james94@gmail.com} \cr

#' @description  Displays appropriate good request modal for phenoGET functionality.

#' @return This function returns a modal object based on phenoGET return.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' @export phenoModalGood

phenoModalGood <- function(phenoURL)
{modalDialog(
  title = "Phenocam Image",
  "To download the image right click on the image and click 'Save image as...'",
  size = "l",
  tags$img(
    src = phenoURL),
  easyClose = TRUE)}

#' @name phenoModalBad

#' @author
#' James Ross \email{ross.james94@gmail.com} \cr

#' @description  Displays appropriate bad request modal for phenoGET functionality.

#' @return This function returns a modal object based on phenoGET return.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' @export phenoModalBad

phenoModalBad <- function(usrDateTime)
{modalDialog(
  title = "Phenocam Image",
  "No phenocam image available at ",siteID," for Date/Time",usrDateTime,
  size = "s",
  easyClose = TRUE)}
