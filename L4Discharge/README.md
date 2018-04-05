NEON Stage-Discharge Rating Curve
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- ****** Description ****** -->
Calculates the Stage-Discharge Rating Curve for a Site and Water Year

<!-- ****** Usage ****** -->
Usage
-----

The stageQCurve package can be used for calculating a stage-discharge rating curve using the same procedure described in *NEON User Guide to Stream Discharge Rating Curve, revision A*. Briefly, the NEON stage-discharge rating curve is developed using a Bayesian modeling technique developed by the Bayesian Rating Curve Advanced Graphical Environment (BaRatinAGE) development team (Le Coz et al., 2013; Le Coz et al., 2014). The executable and/or a GUI is available freely with an individual license by sending an email to: <baratin.dev@lists.irstea.fr>.

<!-- ****** Using a Docker Container ****** -->
Using a Docker Container
------------------------

Because the BaM executable is written for either a windows or linux operating system we have created a Docker container that can run the linux executable on any computer with Docker installed, including macOS. Details of Docker and a free download of the community edition can be found on their website (<https://docs.docker.com/install/>). In order to build and run the container based off of the dockerfile in this repo, users should use the following steps until they understand the process and can make customizations:

1.  Request a download and individual license for the BaM executable by sending an email to: <baratin.dev@lists.irstea.fr>. The BaM executable package will also include examples and documentation related to using the BaM executable.
2.  Copy the BaM\_beta folder into a cloned version of the NEON-stream-discharge GitHub repo (e.g. ~GitHub/NEON-stream-discharge/L4Discharge/BaM\_beta). Within the BaM\_beta folder there should be a copy of the executable (either BaM\_exe for linux or BaM\_MiniDMSL.exe for windows), a Config\_BaM.txt configuration file, and a BaM\_BaRatin (or other folder that's name matches the first line of the Config\_BaM.txt file) folder. In the BaM\_BaRatin folder there should be a set of configuration files (see BaM quickstart guide for more information about these configurations) and a data folder (or other folder that's name matches the path in the first line of the Config\_Data.txt file).
3.  The way the Docker container is set up it will write outputs to the BaM\_BaRatin folder. If you are using different paths you may need to update the dockerfile environment variables to match the BaM configuration files and your specific file structure.
4.  Download NEON expanded data packages for the following data products for the sites and dates that you would like to process: stream discharge field collection (NEON.DP1.20048), stream morphology map (NEON.DP4.00131), and stream discharge rating curve (NEON.DP4.00133). NEON and the scripts in this repo use the OCt 1<sup>st</sup> to September 30<sup>th</sup> water year. So, it is suggested to download data for entire water year in order to get the appropriate data. These zip files (no need to unzip) should be placed into the ~GitHub/NEON-stream-discharge/L4Discharge/data folder.
5.  Download and install Docker
6.  Make sure Docker is running and virtualization is set up properly for your computer (<https://docs.docker.com/get-started/>).
7.  In Docker settings enable the drive that Docker will need access to for writing out results, most likely your C: drive if you're on windows (<https://docs.docker.com/docker-for-windows/#general>).
8.  In a shell run the following command: docker build -t ratingCurve path/to/the/dockerfile (example path: C:/Users/Documents/GitHub/NEON-stream-discharge). Note the direction of the slashes, windows users.
9.  Once the container has been successfully built , which may take many minutes, run the following command: docker run -e SITE=HOPB -e STARTDATE=2017-10-01 -v path/to/output/folder:path/to/outputs/in/container ratingCurve (volume path example: C:/Users/Documents/GitHub/NEON-stream-discharge/L4Discharge/BaM\_beta/BaM\_BaRatin:/app/L4\_discharge/BaM\_beta/BaM\_BaRatin/)
10. The results of the BaM executable should now be available on the host machine. Keep in mind that Docker never automatically cleans up mounted volumes.
11. You do not need to re-build the container to run it again, you could just change the SITE and STARTDATE variables to run additional sites and dates. If you change the R code or file paths you will need to re-build the container for the changes to take effect when you run the container.

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
