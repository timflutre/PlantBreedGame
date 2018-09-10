<!-- pandoc README.md -f commonmark -t html -s -o README.html -->

# PlantSelBreedGame

This directory contains the `PlantSelBreedGame` software implementing a **serious game** to teach **selective breeding** via the example of a fictitious annual **plant species** to students at the master level.
It takes the form of a [Shiny](http://shiny.rstudio.com/) application, benefiting from the [R](https://www.r-project.org/) programming language and software environment for statistical computing.

This software is available under a **free software** license, the [GNU Affero General Public License](https://www.gnu.org/licenses/agpl.html) (version 3 and later); see the COPYING file.
The copyright is owned by the [Institut National de la Recherche Agronomique](http://www.inra.fr/) (INRA) and [Montpellier SupAgro](http://www.supagro.fr/).

It is versioned using the [git](http://www.git-scm.com/) software, the central repository being hosted [here](https://github.com/timflutre/PlantSelBreedGame) on GitHub, and the institutional repository being hosted [there](https://sourcesup.renater.fr/projects/PlantSelBreedGame/) on SourceSup.


# Installation

## Dependencies

Before using this game, you need R as well as various packages, listed in the file `src/dependencies.R`.

## Locally on your computer

Retrieve the Shiny application, by [downloading](https://github.com/timflutre/PlantSelBreedGame/archive/master.zip) it as a ZIP archive (then unzip it and rename the directory):

```
wget https://github.com/timflutre/PlantSelBreedGame/archive/master.zip
unzip master.zip
mv PlantSelBreedGame-master PlantSelBreedGame
```

or by cloning the git repository:

```
git clone git@github.com:timflutre/PlantSelBreedGame.git
```

Enter into the `PlantSelBreedGame` directory; inside, download the `data.zip` archive (example data set retrievable [from the authors](mailto:timothee.flutre@inra.fr)) and unzip it.

Then, open an R session, and execute the following commands:

```
library(shiny)
setwd("path/to/PlantSelBreedGame")
runApp()
```

Once all requested packages are installed, a web page should automatically open in your default web browser, and you should be able to start playing.
See the explanations below in the section "Usage".

## To set up your own server

First, you need to have a [Shiny server](https://www.rstudio.com/products/shiny/shiny-server/) installed and running, say, in `/srv/shiny-server`.
We only tested with the open source version, but it should also work with the pro version.
For all this, have a look at the official [documentation](http://docs.rstudio.com/shiny-server/).

Second, you need to add an application to the server.
Start by downloading, for instance in your home, our Shiny application as a ZIP archive or clone the git repository (see explanations in the previous section):

```
cd ~
wget https://github.com/timflutre/PlantSelBreedGame/archive/master.zip
unzip master.zip
mv PlantSelBreedGame-master PlantSelBreedGame
```

Then, create a new directory for the application (let's call it `breeding-game` here):

```
mkdir /srv/shiny-server/breeding-game
 ```

and copy inside the content of our Shiny application you just downloaded:

```
cp -r ~/PlantSelBreedGame-master/* /srv/shiny-server/breeding-game
```

By default, the Shiny server runs as a unix user named `shiny`.
You hence need to create a unix group, named for instance `breeding`, to which the `shiny` user can be added (the Shiny server may need to be restarted for this to be taken into account).

Everything inside the `breeding-game` directory should be readable and writable for users who are part of the `breeding` group.
This is particularly the case for the data set (`data` directory, retrievable from the authors, see previous section).
Download the `data.zip` archive in your home on the server, copy it inside the `breeding-game` directory, unzip it, and set the read and write rights to the group `breeding`:

```
cp ~/data.zip /srv/shiny-server/breeding-game
cd /srv/shiny-server/breeding-game
unzip data.zip
chgrp -R breeding data
chmod -R ug+rw,o-rwx data
```

Now go to the URL of the `breeding-game` application made available by your Shiny server.
If you encounter an error, look at the log, for instance:

```
less /var/log/shiny-server/breeding-game-shiny-20180129-100752-39427.log
```

Errors may be due to missing packages, otherwise report an issue (see below).
Once all requested packages are installed, you should be able to start playing.
See the explanations below in the section "Usage".


# Citation

A scientific article is currently in preparation.
This repository is made available in the mean time for pedagogical reasons.
As the authors invested time and effort in creating this package, please cite it as:

```
Flutre T, Diot J, David J. PlantSelBreedGame: a serious game to teach plant selective breeding. 2018. https://github.com/timflutre/PlantSelBreedGame
```


# Example

You can discover an example online [here](http://www.agap-sunshine.inra.fr/breeding-game/).


# Usage

Once the application is installed and working, read the game rules (tab `How to play?`) and start by downloading the initial data set.

Before making any request, such as phenotyping, you need to log in (tab `Identification`).
To get a sense of how the interface works, you can use the "test" breeder with the "tester" status which doesn't require any password and isn't subject to time restriction.
Remember, for your selection to work, you need to analyze the initial data carefully.
The `Theory` tab can be helpful.
Enjoy!

If you want to organize a real playing session, you need to create as many breeders as there are players or player teams.
For this, you need to log in as the "admin" breeder with the "game-master" status.
Initially, its password is set as `1234`, but this can (and should!) be changed via the tab `Identification`.
This tab also allows a game master to create new breeders and sessions, among other things.


# Issues

When encountering a problem with the package, you can report issues on GitHub directly ([here](https://github.com/timflutre/PlantSelBreedGame/issues)).


# Contributing

You can contribute in various ways:

* report an issue (online, see the above section);

* suggest improvements (in the same way as issues);

* propose a [pull request](https://help.github.com/articles/about-pull-requests/) (after creating a new branch).
