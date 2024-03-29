% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/def.calc.Q.inj.R
\name{def.calc.Q.inj}
\alias{def.calc.Q.inj}
\title{Salt-based discharge calculations for a constant rate injection}
\usage{
def.calc.Q.inj(
  inputFile,
  injRate = inputFile$injRate,
  injConc = inputFile$injConc,
  backgroundConc = inputFile$backgroundConc,
  plateauConc = inputFile$plateauConc
)
}
\arguments{
\item{inputFile}{Name of the data frame containing the information needed to calculate stream discharge from a constant-rate injection. If the headers are named: "injRate", "injConc", "backgroundConc", and "plateauConc", no other inputs are required. Otherwise, the names of the columns need to be input for the function to work.}

\item{injRate}{Flow rate of tracer injection [mL/min]}

\item{injConc}{Concentration of the tracer injectate [mg/L]}

\item{backgroundConc}{Background concentration of the tracer in the stream [mg/L]}

\item{plateauConc}{Plateau concentration of the tracer in the stream[mg/L]}
}
\value{
This function returns stream discharge "Q.inj" [lps] appended as an additional column to the inputFile with flags set to 1 when the background corrected injectate concentration and/or plateau concentration are less than 0.
}
\description{
This function calculates discharge from a constant rate salt tracer injection.
}
\examples{
sbdDataPlusInjQ <- def.calc.Q.inj(inputFile = sbdFormatted)
}
\references{
License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
}
\seealso{
def.calc.Q.slug.R for calculating stream discharge from a slug injection
}
\author{
Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr
}
\keyword{constant-rate}
\keyword{discharge,}
\keyword{injection}
\keyword{rivers,}
\keyword{salt-based,}
\keyword{streams,}
\keyword{surface}
\keyword{tracer,}
\keyword{water,}
