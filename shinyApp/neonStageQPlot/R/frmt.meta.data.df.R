##############################################################################################
#' Format NEON AQU Site Hydrologic Metadata

#' @name frmt.meta.data.df

#' @author
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr

#' @description  This function formats reach-scale hydrologic and morphological metadata for a
#' NEON AQU site into a dataframe to be rendered in the discharge visualization shiny app UI.

#' @param input.list Required: list created by shiny that contains the site ID, start date, 
#' and end date selected by the user [list]
#' @param product.list Required: list stored as a reference table that contains the metadata
#' needed to build the data frame [data frame]

#' @return Returns a data frame.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export frmt.meta.data.df

# changelog and author contributions / copyrights
#   Divine Aseaku (2026-06-20)
#     original creation
##############################################################################################
# # Source packages and set options
options(stringsAsFactors = F)

frmt.meta.data.df <-function(input.list,product.list){
  
  if(missing(input.list)){
    stop('need input to format metadata table')
  }
  if(missing(product.list)){
    stop('need product list to format metadata table')
  }
  
  # Define site-specific metadata for rendering
  metaD <-  product.list%>%
    dplyr::filter(siteID == input.list$siteId)%>%
    dplyr::select(upstreamWatershedAreaKM2,reachSlopeM,averageBankfullWidthM,d50ParticleSizeMM)%>%
    dplyr::rename("Upstream watershed area (km^2)"= upstreamWatershedAreaKM2,
                  "Reach slope (m)" = reachSlopeM,
                  "Mean bankfull width (m)"= averageBankfullWidthM,
                  "D50 particle size (mm)"=d50ParticleSizeMM) %>%
    dplyr::mutate_all(as.character)%>%
    tidyr::pivot_longer(c("Upstream watershed area (km^2)","Reach slope (m)","Mean bankfull width (m)","D50 particle size (mm)"),
                        names_to = "MetaData",
                        values_to = "Values")
  
  return(metaD)
  
}
