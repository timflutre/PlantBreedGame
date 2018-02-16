# PlantSelBreedGame

This directory contains the `PlantSelBreedGame` software implementing a **serious game** to teach **selective breeding** via the example of a fictitious annual **plant species** to students at the master level.
It takes the form of a [Shiny](http://shiny.rstudio.com/) application, benefiting from the [R](https://www.r-project.org/) programming language and software environment for statistical computing.

This software is available under a **free software** license, the [GNU Affero General Public License](https://www.gnu.org/licenses/agpl.html) (version 3 and later); see the COPYING file.
The copyright is owned by the [Institut National de la Recherche Agronomique](http://www.inra.fr/) (INRA) and [Montpellier SupAgro](http://www.supagro.fr/).

It is versioned using the [git](http://www.git-scm.com/) software, the central repository being hosted [here](https://github.com/timflutre/PlantSelBreedGame) on GitHub, and the institutional repository being hosted [there](https://sourcesup.renater.fr/projects/PlantSelBreedGame/) on SourceSup.


# Installation

## Locally on your computer

Retrieve the Shiny application, by [downloading](https://github.com/timflutre/PlantSelBreedGame/archive/master.zip) it as a ZIP archive (then unzip it), or by cloning the git repository:

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

Once all requested packages are installed, a web page should automatically open in your default web browser, and you should be able to start playing!

## To set up your own server

First, you need to have a [Shiny server](https://www.rstudio.com/products/shiny/shiny-server/) installed and running, say, in `/srv/shiny-server`.
We only tested with the open source version, but it should also work with the pro version.
Then, you need to add an application to the server (let's call it `breeding` here) so that `/srv/shiny-server/breding` is an empty directory.
For all this, have a look at the official [documentation](http://docs.rstudio.com/shiny-server/).

Once your Shiny server is set up, go to the path of the `breeding` application and, inside, copy the content of our Shiny application, by downloading it as a ZIP archive (then unzip it), or by cloning the git repository (see explanations in the previous section).

By default, the Shiny server runs as a unix user named `shiny`.
You hence need to create a unix group, named for instance `breeding`, to which the `shiny` user can be added (the Shiny server may need to be restarted for this to be taken into account).

Everything inside the directory of the `breeding` application should be readable and writable for users who are part of the `breeding` group.
This is particularly the case for the data set (`data` directory, retrievable from the authors, see preceding section).
Download the `data.zip` archive in your home on the server, unzip it, copy its content inside the directory of the `breeding` application, and set the read and write rights to the group `breeding`:

```
chgrp -R breeding data
chmod -R ug+rw,o-rwx data
```


# Citation

A scientific article is currently in preparation.
This repository is made available in the mean time for pedagogical reasons.
As the authors invested time and effort in creating this package, please cite it as:

```
Flutre T, Diot J, David J. PlantSelBreedGame: a serious game to teach plant selective breeding. 2018. https://github.com/timflutre/PlantSelBreedGame
```


# Example

You can discover an example online [here](http://www.agap-sunshine.inra.fr/breeding-game/).


# Issues

When encountering a problem with the package, you can report issues on GitHub directly ([here](https://github.com/timflutre/PlantSelBreedGame/issues)).


# Contributing

You can contribute in various ways:

* report an issue (online, see the above section);

* suggest improvements (in the same way as issues);

* propose a [pull request](https://help.github.com/articles/about-pull-requests/) (after creating a new branch).
