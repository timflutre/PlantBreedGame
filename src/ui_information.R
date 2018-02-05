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



tabItem(
    tabName="info",
    fluidRow(
        ## uiOutput("UIbreederInfo1"),

        ## first box
        shinydashboard::box(width=12, title=NULL,
  div(id="ini_info",
      h1("Information")
      ),

  div(id="spec_bio",
      h2(em("Apimeta simulans"), ", a species with a bright future!"),
      p("Recently discovered on the borders of the upper valley of the Aghromonpe, ", em("Apimeta simulans"), " belongs to the ", em("Statisticeae"), " genus.",
        " It produces flowers which contain an alkaloid compound, named ", em("sepmetin"), ", which is consumed by students to avoid headaches during excessive intellectual effort.",
        " The market is therefore very important and growing rapidly.",
        " Producers are paid for the quantity produced, with yields in the order of ", constants$mu.trait1, " kg of flowers per hectare, but processors have managed to require that the average ", em("sepmetin"), " content of commercial lots be above ", constants$mu.trait2, " per thousand."),


      h2("Biology and ecology"),
      p(em("Apimeta simulans"), " is annual, hermaphrodite and autogamous.",
        " It is commercially available as pure lines, and crosses easily.",
        " Up to ", format(12 / constants$duration.allof, digits=2), " generations per year can be produced in greenhouses to accelerate fixation until homozygosity.",
        " It is also possible to produce doubled haploids via haplodiploidisation.",
        " The multiplication rate of the species is very high, each plant being able to produce more than 1000 seeds.",
        " However, ", HTML("<em>A.&nbsp;simulans</em>"), " is susceptible to various fungi, most notably the dreaded fluorescent rust, ", em("Putrida psychedelica"), "."),


      h2("Genomic and genetic resources"),
      p("The species is diploid, with ", constants$nb.chrs, " chromosomes, all of the same size (", format(constants$chr.length / 10^6, digits=2), " Mb).",
        " A physical map is also available.",
        " Two microarrays were constructed from the ", HTML("<i>de novo</i>"), " sequencing of ", constants$nb.inds.denovo, " individuals: a high-density chip with ", constants$nb.snps.hd, " SNP markers and a low-density one with ", constants$nb.snps.ld, " SNP markers.",
        " KASPar genotyping can also be developed for single SNPs."),
      p("The species was domesticated recently.",
        " Despite the dangers in the uninhabited Aghromonpe valley, several sampling campaigns were conducted.",
        " As a result, numerous accessions were gathered into a genetic resources collection, from which ", constants$nb.phenotyped.coll, " lines were derived."),


      h2("Available data"),
      p("These lines were planted and phenotyped on the only experimental site consisting of ", constants$nb.plots, " plots.",
        " Starting in ", constants$first.year, ", each year for ", 10, " years, ", 150, " lines were planted, in ", 2, " plots each.",
        " In addition, most lines were planted two successive years.",
        " Each year, the trial hence includes ", 75, " lines already tested in the previous year, and ", 75, " new lines."),
      p("The data collected are flower production in kg/ha (", code("trait1"), "), ", em("sepmetin"), " content in g/kg (", code("trait2"), "), and the presence of symptoms caused by ", HTML("<em>P.&nbsp;psychedelica</em>"), "(", code("trait3"), ").",
        " For ", constants$nb.genotyped.coll, "lines tested the last years, genotypic data on the high-density chip are already available.",
        " Moreover, phenotypes of the ", constants$nb.controls, " controls used at the end of the game are also provided (", constants$nb.years.per.ctl, " years, ", constants$nb.plots.per.ctl, " plots per control)."),
      p(strong("Download the data"), " at the bottom of this page."),


      h2("Experimental and financial means"),
      p(strong("Experimental site"), ": the only experimental site, Agrom-sur-Lez (AZ), has ", strong(constants$nb.plots, " plots."),
        " Planting a plot should be requested before ", strong(format(as.Date(constants$max.upload.pheno.field, format="%m-%d"), "%B %d")), ", and requires about ", 500, " seeds.",
        " The cost of a single plot (seeding, phenotyping of the three traits and harvesting) is ", strong(constants$cost.pheno.field, " Mendels"), ", and is used as a reference for all other costs.",
        " Phenotypic data are available ", constants$duration.pheno.field, " months after, that is not before ", strong(format(seq.Date(as.Date(constants$max.upload.pheno.field,format="%m-%d"), length=2, by=paste0(constants$duration.pheno.field, " months"))[2], "%B %d")), "."),
      p(strong("Greenhouse"), ": it can be used all year long to phenotype ", HTML("<em>P.&nbsp;psychedelica</em>"), ", as well as perform crosses (allofecundation and autofecundation).",
        " ", strong("Rust phenotyping"), " has a ", strong(constants$duration.pheno.patho, "-month"), " delay and costs ", strong(constants$cost.pheno.patho, " plot"), " (", format(constants$cost.pheno.patho * constants$cost.pheno.field, digits=2), " Mendels).",
        " ", strong("Allofecundation"), " has a ", strong(constants$duration.allof, "-month"), " delay and costs ", strong(constants$cost.allof, " plot"), " (", format(constants$cost.allof * constants$cost.pheno.field, digits=2), " Mendels).",
        " ", strong("Autofecundation"), " has a ", strong(constants$duration.autof, "-month"), " delay and costs ", strong(constants$cost.autof, " plot"), " (", format(constants$cost.autof * constants$cost.pheno.field, digits=2), " Mendels)."),
      p(strong("Laboratory"), ": it can be used to perform haplodiploidisation (similar as for maize), and genotype samples on the various SNP chips.",
        " ", strong("Haplodiploidisation"), " has a ", strong(constants$duration.haplodiplo, "-month"), " delay, costs ", strong(constants$cost.haplodiplo, " plot"), " (", format(constants$cost.haplodiplo * constants$cost.pheno.field, digits=2), " Mendels), and a maximum of ", constants$max.nb.haplodiplos, " can be requested at once.",
        " ", strong("High-density genotyping"), " has a ", strong(constants$duration.geno.hd, "-month"), " delay and costs ", strong(constants$cost.geno.hd, " plot"), " (", format(constants$cost.geno.hd * constants$cost.pheno.field, digits=2), " Mendels).",
        " ", strong("Low-density genotyping"), " has a ", strong(constants$duration.geno.ld, "-month"), " delay and costs ", strong(format(constants$cost.geno.ld, digits=2), " plot"), " (", format(constants$cost.geno.ld * constants$cost.pheno.field, digits=2), " Mendels)."),
      p(strong("Budget"), ": each team starts with a total budget of ", strong(format(constants$cost.pheno.field*constants$nb.plots*10*1.3, digits=2,scientific=F), " Mendels"), ", fully available from the start."),


      h2("Final trial"),
      p("At the end of the game, each team will have to propose to register their best genotypes (up to five).",
        " The registration fee is ", format(constants$cost.register * constants$cost.pheno.field, digits=2), " Mendels per genotype."),
      p(" Each of them must meet the DHS criteria, which will be assessed primarily on their heterozygosity: < 3%.",
        " They must also meet the VATE criteria corresponding to a minimum of ", 100 * constants$register.min.trait1, "% of the flower production of the control lines (known at the beginning of the program).",
        " Varieties below the ", constants$register.min.trait2, " per thousand of ", em("sepmetin"), " will be eliminated.",
        " Resistant varieties will have a bonus."),
      p(" Availability of seed should be sufficient.",
        " The proposed genotype must therefore have been tested at least once in a plot to ensure that sufficient seed is available to send to the evaluators."),

      h2("Advice"),
      p("Key concepts: heritability, breeding values, additive genetic variance, expected selection gain, selection intensity, genetic architecture, QTL detection, genomic prediction"),
      p("Softwares:",
        " ", a("lme4", href="https://cran.r-project.org/package=lme4", target="_blank"),
        ", ", a("QTLRel", href="https://cran.r-project.org/package=QTLRel", target="blank_"),
        ", ", a("rrBLUP", href="https://cran.r-project.org/package=rrBLUP", target="blank_"),
        ", ", a("BGLR", href="https://cran.r-project.org/package=BGLR", target="blank_"),
        ", ", a("cvTools", href="https://cran.r-project.org/package=cvTools", target="blank_"),
        ", ", a("breedR", href="https://github.com/famuvie/breedR", target="blank_"),
        ", ", a("rgs3", href="https://github.com/INRA/rgs3", target="blank_"))

      )

  ), # close shinydashboard::box

  shinydashboard::box(height=200, width=12, title="Initial data",
                      p("Select and download each file:"),
                      selectInput("iniDataFile", "", choices=list.files("data/shared/initial_data")),
                      downloadButton("dwnlIniData", "Download your file")
  ),

  shinydashboard::box(height=300, width=12, title="debug",
                      verbatimTextOutput("infoDebug")
  )

  ) # close fluidRow
) # close tabItem
