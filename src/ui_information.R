## Copyright 2015,2016,2017,2018 Institut National de la Recherche Agronomique
## and Montpellier SupAgro.
##
## This file is part of PlantSelBreedGame.
##
## PlantSelBreedGame is free software: you can redistribute it and/or modify
## it under the terms of the GNU Affero General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## PlantSelBreedGame is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU Affero General Public
## License along with PlantSelBreedGame.  If not, see
## <http://www.gnu.org/licenses/>.



# UI of "information" part


tabItem(tabName="info",
  fluidRow(
    ## uiOutput("UIbreederInfo1"),




  # first box
  shinydashboard::box(width=12, title = NULL,
      div(id="ini_info",
          h1("Information")
      ),
      div(id="spec_bio",
          h2(em("Apimeta simulans"), ", a species with a bright future!"),
          p("Recently discovered on the borders of the upper valley of the Aghromonpe, ", em("Apimeta simulans"), " belongs to the ", em("Statisticeae"), " genus.",
            " It produces flowers which contain an alkaloid compound, named ", em("sepmetin"), ", which is consumed by students to avoid headaches during excessive intellectual effort.",
            " The market is therefore very important and growing rapidly.",
            " Producers are paid for the quantity produced, with yields in the order of 40 kg of flowers per hectare, but processors have managed to require that the average ", em("sepmetin"), " content of commercial lots be above 14 per thousand."),

          h2("Biology and ecology"),
          p(em("Apimeta simulans"), " is hermaphrodite and autogamous.",
            " It is produced as pure lines, and crosses easily.",
            " Up to two generations per year can be produced in greenhouses to accelerate fixation until homozygosity.",
            " It is also possible to produce doubled haploids.",
            " The propagation rate of the species is very high, each plant being able to produce more than 1000 seeds.",
            " However, ", em("A. simulans"), " is susceptible to various fungi, most notably the dreaded fluorescent rust, ", em("Putrida psychedelica"), "."),

          h2("Genomic resources"),
          p("The species is diploid, with 10 chromosomes, all of the same size.",
            " A physical map is also available.",
            " Two microarrays were constructed from the de novo sequencing of 20 individuals: a high-density chip with 10 000 SNP markers and a low-density one with 5000 SNP markers.",
            " KASPar genotyping can also be developed for single SNPs."),

          h2("Genetic resources and available data"),
          p("...")
      )

  )








))







