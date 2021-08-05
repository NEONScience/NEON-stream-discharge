Package: neonStageQPlot
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- ****** Description ****** -->

Welcome\! This folder contains the R package **neonStageQPlot** that
allows users to view NEON’s [Stage-discharge rating curve
(DP4.00133.001)](https://data.neonscience.org/data-products/DP4.00133.001)
and [Continuous discharge
(DP4.00130.001)](https://data.neonscience.org/data-products/DP4.00130.001)
data products in the form of a user-friendly shiny app.

<!-- ****** Usage ****** -->

## How to Install & Run the App

The **neonStageQPlot** package is only available for install from
Github. To install the package:

`devtools::install_github(repo =
"NEONScience/NEON-stream-discharge/shinyApp/neonStageQPlot",
dependencies = TRUE, force = TRUE)`

To launch the shiny app, run the `run.RC.cont.Q.plot()` function.

## App Dependencies

The shiny app downloads published data from the [NEON Data
Portal](https://data.neonscience.org/home) using the
[neonUtilities](https://github.com/NEONScience/NEON-utilities) package.
Users must have the neonUtilities (available in
[CRAN](https://cran.r-project.org/web/packages/neonUtilities/index.html))
package installed to run the app.

The shiny app uses the
[stageQCurve](https://github.com/NEONScience/NEON-stream-discharge/tree/master/L4Discharge/stageQCurve)
package. The stageQCurve package is not available in CRAN and must be
installed directly from Github:

`devtools::install_github(repo =
"NEONScience/NEON-stream-discharge/L4Discharge/stageQCurve",
dependencies = TRUE, force = TRUE)`

To install the stageQCurve package, you may also need to install the
[geoNEON](https://github.com/NEONScience/NEON-geolocation/tree/master/geoNEON)
package that must also be installed directly from GutHub:

`devtools::install_github(repo = "NEONScience/NEON-geolocation/geoNEON",
dependencies = TRUE, force = TRUE)`

## Other Content in the shinyApp Folder

  - Data frame `aqu_dischargeDomainSiteList.csv` contains a list of the
    NEON domains and site IDs that have continuous discharge data
    published to the NEON Data Portal. The data frame also contains
    metadata for each site that is rendered in the application.

  - .Rds file `rcPlottingData.rds` contains the data needed to plot the
    posterior rating curve with associated uncertainties.

<!-- ****** Acknowledgements ****** -->

## Credits & Acknowledgements

Authors:

Divine Aseaku - email: <divineaseaku@gmail.com>

Zachary L. Nickerson - email: <nickerson@battelleecology.org>

<!-- HTML tags to produce image, resize, add hyperlink. -->

<!-- ONLY WORKS WITH HTML or GITHUB documents -->

<a href="http://www.neonscience.org/">
<img src="logo.png" width="300px" /> </a>

<!-- Acknowledgements text -->

The National Ecological Observatory Network is a project solely funded
by the National Science Foundation and managed under cooperative
agreement by Battelle. Any opinions, findings, and conclusions or
recommendations expressed in this material are those of the author(s)
and do not necessarily reflect the views of the National Science
Foundation.

<!-- ****** License ****** -->

## License

GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

<!-- ****** Disclaimer ****** -->

## Disclaimer

*Information and documents contained within this pachage are available
as-is. Codes or documents, or their use, may not be supported or
maintained under any program or service and may not be compatible with
data currently available from the NEON Data Portal.*
