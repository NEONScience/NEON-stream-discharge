##############################################################################################
#' @title Formats the MCMC cooked data to be 15 characters wide

#' @author
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function returns a number formatted as 15 character scientific notation.

#' @param inputNum input number in character format [character]

#' @return This writes out number formatted as 15 character scientific notation string

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-12-07)
#     original creation
#   Zachary L. Nickerson (2021-05-03)
#     fix bug when inputNum = 10
##############################################################################################
txt.fmt.MCMC.cooked <- function(inputNum){
  finalCharLength <- 15
  sciNotationLength <- 4 #Format as E+## or E-##
  expLength <- 2 #Length of exponent characters

  inputNum <- as.numeric(inputNum)
  inputFact <- 0
  while(!(abs(inputNum)<1 & abs(inputNum) >0.09999999) & abs(inputNum) != 1){
    if(abs(inputNum) > 1){
      inputNum <- inputNum/10
      inputFact <- inputFact + 1
    }else{
      inputNum <- inputNum*10
      inputFact <- inputFact - 1
    }
  }
  #Test whether + or -
  if(inputFact<0){
    sign<-"-"
    inputFact <- abs(inputFact)
  }else{
    sign<-"+"
  }
  #Add 0 in front of sci notation exponent if needed
  if(nchar(inputFact)<expLength){inputFact <- paste0("0",inputFact)}

  #Final formatting
  spaces <- paste(rep(" ",(finalCharLength - sciNotationLength - nchar(inputNum))),collapse = "")
  outputNum <- paste0(spaces,inputNum,"E",sign,inputFact)
  return(outputNum)
}
