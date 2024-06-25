FROM rocker/shiny-verse
LABEL maintainer="Julien Diot <juliendiot@ut-biomet.org>"

# EXPOSE 3838

RUN apt-get update && apt-get install -y \
  curl \
  libglpk-dev

# check package repository
# RUN if [ $(curl -s -o /dev/null -w "%{http_code}" https://cran.microsoft.com/snapshot/2020-03-17/) = "200" ] ; then echo "OK: R pkg repository server accessible." ; else echo "ERROR: R pkg repository server not accessible. status = $(curl -s -o /dev/null -w "%{http_code}" https://cran.microsoft.com/snapshot/2020-03-17/)" ; fi
# RUN touch ~/.Rprofile
# RUN echo "options(repos = c(CRAN = 'https://cran.microsoft.com/snapshot/2020-03-17/'))" >> ~/.Rprofile

# remove sample-apps
RUN rm -rf /srv/shiny-server/*

# install packages dependencies
COPY ./tools/ /srv/shiny-server/PlantBreedGame/tools
RUN Rscript /srv/shiny-server/PlantBreedGame/tools/installDeps.R

# copy app code
COPY ./src/ /srv/shiny-server/PlantBreedGame/src
COPY ./www/ /srv/shiny-server/PlantBreedGame/www
COPY ./global.R /srv/shiny-server/PlantBreedGame/.
COPY ./ui.R /srv/shiny-server/PlantBreedGame/.
COPY ./server.R /srv/shiny-server/PlantBreedGame/.

RUN mkdir srv/shiny-server/PlantBreedGame/data

RUN chown -R shiny.shiny srv/shiny-server/PlantBreedGame

USER shiny
