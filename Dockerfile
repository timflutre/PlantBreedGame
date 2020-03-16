FROM rocker/shiny-verse
LABEL maintainer="Julien Diot <juliendiot@ut-biomet.org>"

# EXPOSE 3838

RUN apt-get update && apt-get install -y \
    git

# remove sample-apps
RUN rm -rf /srv/shiny-server/*

# get app code
# from host
COPY ./ /srv/shiny-server/PlantBreedGame
# from github
# RUN git clone --depth=1 https://github.com/timflutre/PlantBreedGame.git
# RUN mv /PlantBreedGame /srv/shiny-server/PlantBreedGame

# install packages dependencies
RUN Rscript /srv/shiny-server/PlantBreedGame/tools/installDeps.R

# build data folder
RUN R -e "rmarkdown::render('/srv/shiny-server/PlantBreedGame/plantbreedgame_setup.Rmd')"

RUN chown shiny.shiny srv/shiny-server/PlantBreedGame/data/*
RUN chmod 644 srv/shiny-server/PlantBreedGame/data/breeding-game.sqlite
