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

# get app code
# from host
COPY ./tools/ /srv/shiny-server/PlantBreedGame/tools
# install packages dependencies
RUN Rscript /srv/shiny-server/PlantBreedGame/tools/installDeps.R
COPY ./src/ /srv/shiny-server/PlantBreedGame/src
COPY ./global.R /srv/shiny-server/PlantBreedGame/.
COPY ./ui.R /srv/shiny-server/PlantBreedGame/.
COPY ./server.R /srv/shiny-server/PlantBreedGame/.
COPY ./plantbreedgame_setup.Rmd /srv/shiny-server/PlantBreedGame/.

# build data folder

RUN R -e "rmarkdown::render('/srv/shiny-server/PlantBreedGame/plantbreedgame_setup.Rmd')"

RUN chmod 664 srv/shiny-server/PlantBreedGame/data/breeding-game.sqlite
RUN chown -R shiny.shiny srv/shiny-server/PlantBreedGame

USER shiny
