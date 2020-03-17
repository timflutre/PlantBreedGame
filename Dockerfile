FROM rocker/shiny-verse
LABEL maintainer="Julien Diot <juliendiot@ut-biomet.org>"

# EXPOSE 3838

RUN apt-get update && apt-get install -y \
  curl

# check package repository
RUN if [ $(curl -s -o /dev/null -w "%{http_code}" https://cran.microsoft.com/snapshot/2020-03-17/) = "200" ] ; then echo "OK: R pkg repository server accessible." ; else echo "ERROR: R pkg repository server not accessible. status = $(curl -s -o /dev/null -w "%{http_code}" https://cran.microsoft.com/snapshot/2020-03-17/)" ; fi
RUN touch ~/.Rprofile
RUN echo "options(repos = c(CRAN = 'https://cran.microsoft.com/snapshot/2020-03-17/'))" >> ~/.Rprofile

# remove sample-apps
RUN rm -rf /srv/shiny-server/*

# get app code
# from host
COPY ./ /srv/shiny-server/PlantBreedGame

# from github
# RUN apt-get update && apt-get install -y \
#   git
# RUN git clone --depth=1 https://github.com/timflutre/PlantBreedGame.git
# RUN mv /PlantBreedGame /srv/shiny-server/PlantBreedGame

# install packages dependencies
RUN Rscript /srv/shiny-server/PlantBreedGame/tools/installDeps.R
RUN rm ~/.Rprofile

# build data folder
RUN R -e "rmarkdown::render('/srv/shiny-server/PlantBreedGame/plantbreedgame_setup.Rmd')"

RUN chown shiny.shiny srv/shiny-server/PlantBreedGame/data/*
RUN chmod 644 srv/shiny-server/PlantBreedGame/data/breeding-game.sqlite
