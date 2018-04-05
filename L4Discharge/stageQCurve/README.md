NEON Stage-Discharge Rating Curve
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- ****** Description ****** -->
The **stageQCurve** package can be used for calculating a stage-discharge rating curve using the same procedure described in *NEON User Guide to Stream Discharge Rating Curve, revision A*. Briefly, the NEON stage-discharge rating curve is developed using a Bayesian modeling technique developed by the Bayesian Rating Curve Advanced Graphical Environment (BaRatinAGE) development team (Le Coz et al., 2013; Le Coz et al., 2014). The executable and/or a GUI is available freely with an individual license by sending an email to: <baratin.dev@lists.irstea.fr>.

<!-- ****** Usage ****** -->
Usage Outside of a Docker Container
-----------------------------------

1.  Request a download and individual license for the BaM executable by sending an email to: <baratin.dev@lists.irstea.fr>. The BaM executable package will also include examples and documentation related to using the BaM executable.
2.  Copy the BaM\_beta folder into a cloned version of the NEON-stream-discharge GitHub repo (e.g. ~GitHub/NEON-stream-discharge/L4Discharge/BaM\_beta). Within the BaM\_beta folder there should be a copy of the executable (either BaM\_exe for linux or BaM\_MiniDMSL.exe for windows), a Config\_BaM.txt configuration file, and a BaM\_BaRatin (or other folder that's name matches the first line of the Config\_BaM.txt file) folder. In the BaM\_BaRatin folder there should be a set of configuration files (see BaM quickstart guide for more information about these configurations) and a data folder (or other folder that's name matches the path in the first line of the Config\_Data.txt file).
3.  Uncomment the section of run.stag.Q.curv.R that sets the environment variables for use outside of a Docker container.
    -   DIRPATH should be edited to match the path that contains the BaM\_beta folder
    -   BAMFOLD can usually be left as default of BaM\_beta/
    -   BAMFILE shoud be set to the name of the BaM executable file, usually either BaM\_exe for linux or BaM\_MiniDMSL.exe for windows
    -   DATAWS should be set to the location of the NEON zip files that were downloaded from the portal. A short path, e.g. Downloads, worked well. If the paths are too long (&gt;260 characters) there will be trouble.
    -   BAMWS can usually be left as default of BaM\_beta/BaM\_BaRatin/, the location of the BaM config files
    -   STARTDATE string in the format of YYYY-MM-DD for the start date of the analysis. If you enter a start date that is not YYYY-10-01, the script will determine the water year that started immediately before the date entered here and use it as the STARTDATE
    -   SITE 4 letter NEON site code, e.g. HOPB
4.  Running the run.stag.Q.curv.R script will unzip, format, and calculate a rating curve for the site and date specified. Outputs are written to the BaM\_BaRatin folder by default.

<!-- ****** Acknowledgements ****** -->
Credits & Acknowledgements
--------------------------

<!-- HTML tags to produce image, resize, add hyperlink. -->
<!-- ONLY WORKS WITH HTML or GITHUB documents -->
<a href="http://www.neonscience.org/"> <img src="logo.png" width="300px" /> </a>

<!-- Acknowledgements text -->
The National Ecological Observatory Network is a project solely funded by the National Science Foundation and managed under cooperative agreement by Battelle. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.

<!-- ****** License ****** -->
License
-------

GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

<!-- ****** Disclaimer ****** -->
Disclaimer
----------

*Information and documents contained within this pachage are available as-is. Codes or documents, or their use, may not be supported or maintained under any program or service and may not be compatible with data currently available from the NEON Data Portal.*
