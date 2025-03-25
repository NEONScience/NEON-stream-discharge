FROM rocker/shiny-verse:latest

ARG GITHUB_PAT
ENV GITHUB_PAT=$GITHUB_PAT

RUN rm -rf /srv/shiny-server/* && \
    touch /srv/shiny-server/.Renviron && \
    chown shiny:shiny /srv/shiny-server/.Renviron

COPY --chown=shiny:shiny src/ /srv/shiny-server/
COPY entrypoint.sh /

# use packagemanager.rstudio.com to determine necessary non-R system prerequisites to install 
# If the above tool doesn't have any SystemRequirements listed, use
#   maketools::package_sysdeps("package_name")
# on a linux system with the required R package already installed
# the dependencies below already included in the shiny-verse image
#RUN apt-get update && apt-get install -y --no-install-recommends \ 
	#libstdc++6 \
    #&& apt-get clean \
    #&& rm -rf /var/lib/apt/lists/*


# remotes::install_deps relies on the contents of src/DESCRIPTION
# and it must be copied to the container before running this command
RUN Rscript -e "remotes::install_deps('/srv/shiny-server/',repos='https://packagemanager.posit.co/cran/__linux__/jammy/latest')"

USER shiny:shiny
EXPOSE 3838
ENTRYPOINT [ "/entrypoint.sh" ]