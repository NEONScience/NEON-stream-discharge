# start with the r-base image tag 3.6.0
# https://hub.docker.com/r/rocker/r-base/
# Must be using the same or older version of R as used to make packrat snapshot
FROM rocker/r-ver:3.6.0

# Maintainer handle
MAINTAINER "Kaelin Cawley" kcawley@battelleecology.org

# Authors
LABEL authors="Kaelin Cawley kcawley@battelleecology.org, Zachary Nickerson nickerson@battelleecology.org"

#OS side and R dependencies needed to run the R packages
#Then clean up some of the installed files
#r-ver is based off of debian if you need to install additional packages
RUN	apt-get update && \
	apt-get dist-upgrade -y && \
	apt-get install -y \
		libcurl4-openssl-dev \
		libssl-dev \
		libmariadbclient-dev \
		libgfortran3 && \
	apt-get autoremove -y && \
	apt-get autoclean -y && \
	rm -rf /var/lib/apt/lists/*

#Environment variables that are "" need to be in the run command
#The paths are set here just to keep things all relative
ENV DIRPATH=/app/L4_discharge/ \
	BAMFOLD=BaM_beta/ \
	BAMFILE=./BaM_exe \
	DATAWS=/app/L4_discharge/data/ \
	BAMWS=BaM_beta/BaM_BaRatin/ \
	STARTDATE="" \
	SITE=""

# COPY GitHub repo to the container
# Have to copy before the packrat restore
COPY . $DIRPATH

# Have to set the working directory to the location of the packrat folder
WORKDIR $DIRPATH

#Install the R packages and give user access to BaM
#PackratVersion: 0.4.8.56 (2/20/18)
#XML Version: 3.98-1.9 (2/20/18)
RUN chmod 0755 $BAMFOLD$BAMFILE && \
	chmod 1777 $BAMWS

#Install the R packages and give user access to BaM
#Packrat Version: 0.5.0 (8/31/20)
#XML Version: 3.99-0.5 (8/31/20)
RUN R -e "install.packages(c('XML','BiocManager','packrat','data.table'),type='source'); BiocManager::install(c('rhdf5','Rhdf5lib'));packrat::restore()" &&\
	rm -rf /tmp/* &&\
	rm -rf /tmp/downloaded_packages/ /tmp/*.rds &&\ 
	chmod 0755 $BAMFOLD$BAMFILE && \
	chmod 1777 $BAMWS && \
	chmod 1777 $DIRPATH && \
	chmod 1777 $DIRPATH/packrat

#Users can also consider using something like packrat to lock in the version of the packages that the container is using.
#This will also speed up the build process to have static versions of the packages rather than installing each time
#(e.g. https://gist.github.com/philippmuench/a0135ac6e0ca9f502aad93cdf2ae8a45)

# This runs the r script when it's time to make a container out of the image
CMD ["Rscript", "run.stag.Q.curv.R"]
