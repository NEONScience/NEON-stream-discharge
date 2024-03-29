neonStageQPlot R package: Download and plot NEON hydrology data and
retrieve NEON PhenoCam images from PhenoCam API
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- ****** Description ****** -->

This package allows you view and interact with select hydrologic data
collected at 29 stream, river, and lake inflow/outflow sites across the
National Ecological Observatory Network (NEON). NEON spans the
continential United States and includes monitoring sites in Alaska and
Puerto Rico. In this package you can visually explore data from the
following NEON Data Products:

-   Stage-discharge rating curves
    [(DP4.00133.001)](https://data.neonscience.org/data-products/DP4.00133.001)
-   Continuous discharge
    [(DP4.00130.001)](https://data.neonscience.org/data-products/DP4.00130.001)
-   Precipitation
    [(DP1.00006.001)](https://data.neonscience.org/data-products/DP1.00006.001)
-   Land-water interface images
    [(DP1.20002.001)](https://data.neonscience.org/data-products/DP1.20002.001)

The `neonStageQPlot` package was developed to work in conjunction with
the
[openFlow](https://github.com/NEONScience/NEON-stream-discharge/tree/master/shiny-openFlow)
shiny application, but the functions can also be run independently
outside the openFlow app.

<!-- ****** Usage ****** -->

## How to Install & Run the App

The **neonStageQPlot** package is only available for install from
Github. To install the package:

`devtools::install_github(repo = "NEONScience/NEON-stream-discharge/neonStageQPlot", dependencies = TRUE, force = TRUE)`

## App Dependencies

The shiny app downloads published data from the [NEON Data
Portal](https://data.neonscience.org/home) using the
[neonUtilities](https://github.com/NEONScience/NEON-utilities) package.
Users must have the neonUtilities (available in
[CRAN](https://cran.r-project.org/web/packages/neonUtilities/index.html))
package installed to run the app.

## Functions

`get.cont.Q.NEON.API` - Downloads NEON continuous discharge and
precipitation data from the NEON API, smooths the data to a 20 minute
temporal resoltion, and outputs a data frame that contains the
summarized data.

`cont.Q.plot` - Plots NEON continuous discharge and precipitation data
using outputs from *get.cont.Q.NEON.API*.

`RC.plot` - Plots NEON stage-discharge rating curves data using outputs
from *get.cont.Q.NEON.API*.

`pheno.GET` - Generates a URL with which a user can render a PhenoCam
image from the PhenoCam API.

<!-- ****** Acknowledgements ****** -->

## Credits & Acknowledgements

Authors:

James Ross - email: <ross.james94@gmail.com>

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

*Information and documents contained within this package are available
as-is. Codes or documents, or their use, may not be supported or
maintained under any program or service and may not be compatible with
data currently available from the NEON Data Portal.*
