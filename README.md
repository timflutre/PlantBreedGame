---
title: PlantBreedGame
---

<!-- pandoc README.md -t html -s -o README.html --toc -->

# Overview

`PlantBreedGame` is a software implementing a **serious game** to teach **selective breeding** via the example of a fictitious annual **plant species** to students at the master level.
It takes the form of a [Shiny](http://shiny.rstudio.com/) application, benefiting from the [R](https://www.r-project.org/) programming language and software environment for statistical computing.

This software is available under a **free software** license, the [GNU Affero General Public License](https://www.gnu.org/licenses/agpl.html) (version 3 and later); see the COPYING file.
The copyright is owned by the [Institut National de la Recherche Agronomique](http://www.inra.fr/) (INRA) and [Montpellier SupAgro](http://www.supagro.fr/).

It is versioned using the [git](http://www.git-scm.com/) software, the central repository being hosted [here](https://github.com/timflutre/PlantBreedGame) on GitHub, and the institutional repository being hosted [there](https://sourcesup.renater.fr/projects/plantbreedgame/) on SourceSup.
The `README` file is available at [https://sourcesup.renater.fr/plantbreedgame/](https://sourcesup.renater.fr/plantbreedgame/).

# Example

You can discover an example online [here](http://www.agap-sunshine.inra.fr/breeding-game/).

# Installation

The recomended way to run this web application is by using its [Docker](https://www.docker.com/) image. 

Before running the game make sure to install a recent version of Docker on your system. To install Docker please refer to [their official documentation](https://docs.docker.com/install/):

- [Install Docker Desktop on Windows](https://docs.docker.com/desktop/install/windows-install/)
- [Install Docker Desktop on Mac](https://docs.docker.com/desktop/install/mac-install/)


The docker images of this application is available on Docker Hub at [juliendiot/plantbreedgame](https://hub.docker.com/r/juliendiot/plantbreedgame/tags).

Those images are tagged in 3 different ways:
- `latest`: The latest stable version
- `development`: The latest development version
- `X.X.X`: The image corresponding to `plantBreedGame` version `X.X.X`

In the following commands, you can replace `juliendiot/plantbreedgame:latest` 
with the version you want to use eg. `juliendiot/plantbreedgame:1.1.0` or `juliendiot/plantbreedgame:development`.

Once docker is installed on your system you can start the game application with the command:

```sh
docker run -d --rm --name plantbreedgame -p 80:3838 \
    -v /host/path/to/plantBreedGameData:/var/lib/plantBreedGame \
    juliendiot/plantbreedgame:latest
```

The options used in this command are detailed in [the official documentation of the `docker run` command](https://docs.docker.com/reference/cli/docker/container/run/).

There is a quick breakdown to present some option you can modify:

- `--name plantbreedgame` specify the name of the container.
- `-p 80:3838` control the port mapping between the docker container and the host.
Here:
  - `80` is the port on the host (your machine). You can choose another available
  port if you like.
  - `3838` is the port the application is listenning to inside the container.
  You can not modify it.
- `-v /host/path/to/plantBreedGameData:/var/lib/plantBreedGame` control where 
the application will save data to allow persistent storage across container runs.
  - `/host/path/to/plantBreedGameData` is the path on the host where the game
  data is stored you should **replace with the actual path you want to use**.
  - `/var/lib/plantBreedGame` is the directory inside the container where the
  game will store its data. **You cannot change this path**.


Once you launched the game, the application will be available at: http://localhost:80

> Note: **For testing purpose**, you can ommit the option
`-v /host/path/to/plantBreedGameData:/var/lib/plantBreedGame`.
However, game's data will not persist if you stop the container.

To stop the container you can use:

```sh
docker stop plantbreedgame
```

> Note: In case you use a different container name, you should replace
`plantbreedgame` with the name of the container specify with the `--name` option
you used to start the container.

## Other installation method

For people on Mac or Linux (or Windows subsystem for linux), you can instal the game with [Nix](https://nixos.org/). 

To install nix you can visit: https://github.com/DeterminateSystems/nix-installer

Once Nix is installed you can lanch the game application with 

```sh
mkdir plantBreedGame # create a new directory
cd plantBreedGame # move to this directory
nix build github:timflutre/PlantBreedGame # build the application
./result/bin/plantBreedGame --host 127.0.0.1 --port 3838 # start the application
```

The game data will be stored at `$HOME/.local/share/plantBreedGame`.


## Installation for multiplayer session

This application currently suffer from optimization problems when several players
are connected and play at the same time. When one user make a long request to
the application, it becomes unresponsive for all the other users.

It is possible to work around these problems by launching several game instances
targeting **the same `plantBreedGame` volume**, and make each player use a different 
instance.

For example to start 2 instances you can run:

```sh
docker run -d --rm --name plantbreedgame_A -p 81:3838 \
    -v /host/path/to/plantBreedGameData:/var/lib/plantBreedGame \
    juliendiot/plantbreedgame:latest
docker run -d --rm --name plantbreedgame_B -p 82:3838 \
    -v /host/path/to/plantBreedGameData:/var/lib/plantBreedGame \
    juliendiot/plantbreedgame:latest
```

One instance will be accessible on port `81` and the other on port `82`.

You can then set-up your web server's router to serve each of these instances
on different URLs, or let users access directly port `81` or `82`.

Like that, when eg. *Player-A* make a long request to `plantbreedgame_A`,
`plantbreedgame_B` will not be affected and will be responsive for *Player-B*.

# Usage

## Game Initialisation

To start playing, the game need some specific data (eg. the genotypes and haplotypes of the initial population, a data-base...). This initialisation can be done through the game.

The first time you run the application, most of the game menus will show a message asking you to initialise the game. To do so you need to go to the `Admin` menu, and in `Game Initialisation` tab. There you will find some parametres you can tweak to customise the game and a button that will start the game initialisation. Once the initialisation is completed (which takes about 2 minutes), the page will automatically reload and you will be able to connect and play the game.

The game initialisation will automatically create an `admin` breeder with the default password `1234`.

If the game have already been initialise, it is also possible to re-initialise it to start a new "fresh game". However in such case **all the data of the game will be lost**.


## How to play

Once the application is installed and working, please read the game rules (tab `How to play?`) and start by downloading the initial data set as well as example files showing how requests should be formatted (all files listed at the bottom of the tab `How to play?`).

Before making any request, such as phenotyping, you need to log in (tab `Identification`).
To get a sense of how the interface works, you can use the "test" breeder with the "tester" status which doesn't require any password and isn't subject to time restriction.

All your files, whether they are inputs for a request or outputs of a request, are available for download in the tab `Identification` (once logged in).
But remember that regular players (not testers nor the game master) have time restrictions.
This means that, even if a plant material request is successful, you will have to wait before using your new genotypes in other crosses or before requesting data on it.
Similarly, even if a phenotyping/genotyping request is successfull, you will have to wait before downloading the output file.

For your selection to work, you need to analyze the initial data carefully.
The `Theory` tab can be helpful.

The `Evaluation` tab can be used to compare new genotypes (from different players) with the initial lines.

If you want to organize a real playing session, you need to create as many breeders as there are players or players teams.
For this, you need to log in as the "admin" breeder.
Initially, its password is set to `1234`, but this can (and should!) be changed via the tab `Admin`.
This tab also allows a game master to create new breeders and sessions, among other things.


# Development environment

## Nix :snowflake: 

This repository uses a "[nix](https://nixos.org/) flake" and [`direnv`](https://direnv.net/). If you have nix and direnv installed just `cd` in this repo, enable direnv (`direnv allow`) and all dependencies will be automatically installed and available in a dedicated environment.

After clonning this repository, and `cd` to it you can then launch the application with:

```sh
nix run
```

To install nix you can visit: https://github.com/DeterminateSystems/nix-installer


## Dependencies update

Most of the dependencies (eg. `R` and its packages) are handled with `nix`. To update the dependencies just run:

```sh
nix flake update
```

However, `playwright` (the tool used to test the UI) is handeled by `npm` but the web browser it uses are handled by `nix` and we need to use the same version of playwright in the node project as in nix, or else playwright will try to use browsers versions that aren't installed! (cf. [nixos wiki](https://nixos.wiki/wiki/Playwright)).

There is an example of such error:
```console
Test was interrupted.

    Error: browserType.launch: Executable doesn't exist at /nix/store/8hmp47k3dfdw38f82zj7yd7q351pzgh6-playwright-browsers-chromium/chromium-1080/chrome-linux/chrome
```

To make sure the same version are installed:
- 1. Check the nix playwright's version:
  ```sh
  which playwright # to make sure we are targeting nix's playwright
  playwright --version
  ```
- 2. Specify this version in `package.json`
  ```json
  "devDependencies": {
    "@playwright/test": "X.X.X",
  },
  ```
- 3. Run `npm install --ignore-scripts` to reinstall npm's playwright



# Citation

As the authors invested time and effort in creating this game, please cite the following [letter](https://dl.sciencesocieties.org/publications/cs/abstracts/0/0/cropsci2019.03.0183le) to the editor of Crop Science:

```
Flutre, T., Diot, J., and David, J. (2019). PlantBreedGame: A Serious Game that Puts Students in the Breeder’s Seat. Crop Science. DOI 10.2135/cropsci2019.03.0183le
```

# Acknowledgments

Thanks to Philippe Brabant (AgroParisTech) and Jean-Luc Jannink (Cornell) for their feedbacks.

# Issues

When encountering a problem with the package, you can report issues on GitHub directly ([here](https://github.com/timflutre/PlantBreedGame/issues)).

# Contributing

You can contribute in various ways:

- report an issue (online, see the above section);

- suggest improvements (in the same way as issues);

- propose a [pull request](https://help.github.com/articles/about-pull-requests/) (after creating a new branch).
