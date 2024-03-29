% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{sbdFormatted}
\alias{sbdFormatted}
\title{Formatted salt-based discharge data.}
\format{
A data frame with 30 rows and 18 variables:
\describe{
  \item{siteID}{site of water sample collection, string}
  \item{namedLocation}{location of water sample collection, string}
  \item{startDate}{date and time of experiment, dateTime}
  \item{injectionType}{type of tracer injection; either: 'NaCl', 'NaBr', or 'model', string}
  \item{slugTracerMass}{mass of slug salt tracer, gram}
  \item{slugPourTime}{date and time of slug injection, dateTime}
  \item{injectateSampleID}{injectate sample identifier, string}
  \item{injConc}{salt concentration of the injectate sample, milligramsPerLiter}
  \item{constantRateTracerMass}{mass of constant rate salt tracer, gram}
  \item{carboyVolume}{colume of constant rate injectate solution, liter}
  \item{dripStartTime}{date and time of constant rate injection start, dateTime}
  \item{injRate}{constant rate salt tracer injection rate, millilitersPerMinute}
  \item{backgroundConc}{background stream salt concentration, milligramsPerLiter}
  \item{plateauConc}{salt concentration at plateau, milligramsPerLiter}
  \item{hoboSampleID}{identifier for logger file, string}
  \item{injConcCalcQF}{QF indicating that the injectate salt concentration was not available and could not be calculated}
  \item{plateauCond}{specific conductance at plateau, microsiemensPerCentimeter}
  ...
}
}
\source{
\url{http://data.neoninc.org/home/}
}
\usage{
sbdFormatted
}
\description{
A dataset containing data needed to calculate salt-based discharge for both constant rate and slug injections.
}
\keyword{datasets}
