NEON Stream Discharge Repo
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- ****** Description ****** -->

This repository is for code related to the NEON discharge data products:
stream discharge field collection (NEON.DP1.20048), salt-based discharge
(NEON.DP1.20193), stream discharge rating curve (NEON.DP4.00133), and
stream discharge (NEON.DP4.00130).

<!-- ****** Usage ****** -->

## Usage

The **streamQ** package can be used for calculating stream discharge
from constant rate and slug salt tracer injections using the basic or
expanded data package for salt-based discharge (NEON.DP1.20193).

The /L4Discharge/ folder contains an R package, **stageQCurve**, for
calculating a stage-discharge rating curve using the same procedure
described in *NEON User Guide to Stream Discharge Rating Curve, revision
A*. Briefly, the NEON stage-discharge rating curve is developed using a
Bayesian modeling technique developed by the Bayesian Rating Curve
Advanced Graphical Environment (BaRatinAGE) development team (Le Coz et
al., 2013; Le Coz et al., 2014). The executable and/or a GUI is
available freely with an individual license by sending an email to:
<baratin.dev@lists.irstea.fr>. In addition to the R package, the
/L4Discharge/ folder contains a dockerfile and file structures needed to
run the BaM executable in a Docker container that should allow the R
scripts and executable to run regradless of operating system as long as
the user has Docker (<https://www.docker.com/>) installed.

The /shinyApp/ folder contains the R package, **neonStageQPlot**. When
run, the function will launch a shiny app that downloads Continuous
discharge (NEON.DP4.00130.001) and Stage-discharge rating curves
(NEON.DP4.00133.001) data products from the NEON Data Portal, and plots
the data in an interactive HTML format.

<!-- ****** Acknowledgements ****** -->

## Credits & Acknowledgements

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
