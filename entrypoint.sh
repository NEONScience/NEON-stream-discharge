#!/bin/sh

# $NEON_TOKEN is an environment variable passed to the container at runtime
# This script makes the contents of that environment variable to shiny-server,
# then runs the shiny-server. shiny-server does not pass existing
# environment variables to the running web app, so we must use .Renviron

echo "DB_TOKEN=$DB_TOKEN" >> /srv/shiny-server/.Renviron
echo "DB_HOST=$DB_HOST" >> /srv/shiny-server/.Renviron
echo "HOST=$HOST" >> /srv/shiny-server/.Renviron
echo "GITHUB_PAT=$GITHUB_PAT" >> /srv/shiny-server/.Renviron
/usr/bin/shiny-server