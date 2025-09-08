## Copyright 2015,2016,2017,2018,2019 Institut National de la Recherche Agronomique
## and Montpellier SupAgro.
##
## This file is part of PlantBreedGame.
##
## PlantBreedGame is free software: you can redistribute it and/or modify
## it under the terms of the GNU Affero General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## PlantBreedGame is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU Affero General Public
## License along with PlantBreedGame.  If not, see
## <http://www.gnu.org/licenses/>.



tabItem(
  tabName = "info",
  fluidRow(
    ## uiOutput("UIbreederInfo1"),

    ## first box
    shinydashboard::box(
      width = 12, title = NULL,
      div(
        id = "ini_info",
        h1("Information")
      ),
      div(
        id = "spec_bio",
        h2(shiny::em("Apimeta simulans"), ", a species with a bright future!"),
        p(
          "Recently discovered on the borders of the upper valley of the Aghromonpe, ", shiny::em("Apimeta simulans"), " belongs to the ", shiny::em("Statisticeae"), " genus.",
          " It produces flowers which contain an alkaloid compound, named ", shiny::em("sepmetin"), ", which is consumed by students to avoid headaches during excessive intellectual effort.",
          " The market is therefore very important and growing rapidly.",
          " Producers are paid for the quantity produced, with yields in the order of ", constants_ui("info_mu.trait1"), " kg of flowers per hectare, but processors have managed to require that the average ", shiny::em("sepmetin"), " content of commercial lots be above ", constants_ui("info_mu.trait2"), " per thousand."
        ),
        h2("Biology and ecology"),
        p(
          shiny::em("Apimeta simulans"), " is annual, hermaphrodite and autogamous.",
          " It is commercially available as pure lines, and crosses easily.",
          " Up to ", constants_ui("info_generations.per.year"), " generations per year can be produced in greenhouses to accelerate fixation until homozygosity.",
          " It is also possible to produce doubled haploids via haplodiploidisation.",
          " The multiplication rate of the species is very high, each plant being able to produce more than 1000 seeds.",
          " However, ", HTML("<em>A.&nbsp;simulans</em>"), " is susceptible to various fungi, most notably the dreaded fluorescent rust, ", shiny::em("Putrida psychedelica"), "."
        ),
        h2("Genomic and genetic resources"),
        p(
          "The species is diploid, with ", constants_ui("info_nb.chrs"), " chromosomes, all of the same size (", constants_ui("info_chr.length.Mb"), " Mb).",
          " A physical map is also available.",
          " Two microarrays were constructed from the ", HTML("<i>de novo</i>"), " sequencing of ", constants_ui("info_nb.inds.denovo"), " individuals: a high-density chip with ", constants_ui("info_nb.snps.hd"), " SNP markers and a low-density one with ", constants_ui("info_nb.snps.ld"), " SNP markers.",
          " KASPar genotyping can also be developed for single SNPs."
        ),
        p(
          "The species was domesticated recently.",
          " Despite the dangers in the uninhabited Aghromonpe valley, several sampling campaigns were conducted.",
          " As a result, numerous accessions were gathered into a genetic resources collection, from which ", constants_ui("info_nb.phenotyped.coll"), " lines were derived."
        ),
        h2("Available data"),
        p(
          "These lines were planted and phenotyped on the only experimental site consisting of ", constants_ui("info_nb.plots"), " plots.",
          " Starting in ", constants_ui("info_first.year"), ", each year for ", 10, " years, ", 150, " lines were planted, in ", 2, " plots each.",
          " In addition, most lines were planted two successive years.",
          " Each year, the trial hence includes ", 75, " lines already tested in the previous year, and ", 75, " new lines."
        ),
        p(
          "The data collected are flower production in kg/ha (", code("trait1"), "), ", shiny::em("sepmetin"), " content in g/kg (", code("trait2"), "), and the presence of symptoms caused by ", HTML("<em>P.&nbsp;psychedelica</em>"), "(", code("trait3"), ").",
          " For ", constants_ui("info_nb.genotyped.coll"), "lines tested the last years, genotypic data on the high-density chip are already available.",
          " Moreover, phenotypes of the ", constants_ui("info_nb.controls"), " controls used at the end of the game are also provided (", constants_ui("info_nb.years.per.ctl"), " years, ", constants_ui("info_nb.plots.per.ctl"), " plots per control)."
        ),
        p(strong("Download the data"), " at the bottom of this page."),
        h2("Experimental and financial means"),
        p(
          strong("Experimental site"), ": the only experimental site, Agrom-sur-Lez (AZ), has ", strong(constants_ui("info_nb.plots2"), " plots."),
          " Planting a plot should be requested before ", strong(constants_ui("info_max.upload.pheno.field")), ", and requires about ", 500, " seeds.",
          " The cost of a single plot (seeding, phenotyping of the three traits and harvesting) is ", strong(constants_ui("info_cost.pheno.field"), " Mendels"), ", and is used as a reference for all other costs.",
          " Phenotypic data are available ", constants_ui("info_duration.pheno.field"), " months after, that is not before ", strong(constants_ui("info_pheno.data.availability.date")), "."
        ),
        p(
          strong("Greenhouse"), ": it can be used all year long to phenotype ", HTML("<em>P.&nbsp;psychedelica</em>"), ", as well as perform crosses (allofecundation and autofecundation).",
          " ", strong("Rust phenotyping"), " has a ", strong(constants_ui("info_duration.pheno.patho"), "-month"), " delay and costs ", strong(constants_ui("info_cost.pheno.patho"), " plot"), " (", constants_ui("info_cost.pheno.patho.mendels"), " Mendels).",
          " ", strong("Allofecundation"), " has a ", strong(constants_ui("info_duration.allof"), "-month"), " delay and costs ", strong(constants_ui("info_cost.allof"), " plot"), " (", constants_ui("info_cost.allof.mendels"), " Mendels).",
          " ", strong("Autofecundation"), " has a ", strong(constants_ui("info_duration.autof"), "-month"), " delay and costs ", strong(constants_ui("info_cost.autof"), " plot"), " (", constants_ui("info_cost.autof.mendels"), " Mendels)."
        ),
        p(
          strong("Laboratory"), ": it can be used to perform haplodiploidisation (similar as for maize), and genotype samples on the various SNP chips.",
          " ", strong("Haplodiploidisation"), " has a ", strong(constants_ui("info_duration.haplodiplo"), "-month"), " delay, costs ", strong(constants_ui("info_cost.haplodiplo"), " plot"), " (", constants_ui("info_cost.haplodiplo.mendels"), " Mendels), and a maximum of ", constants_ui("info_max.nb.haplodiplos"), " can be requested at once.",
          " ", strong("High-density genotyping"), " has a ", strong(constants_ui("info_duration.geno.hd"), "-month"), " delay and costs ", strong(constants_ui("info_cost.geno.hd"), " plot"), " (", constants_ui("info_cost.geno.hd.mendels"), " Mendels).",
          " ", strong("Low-density genotyping"), " has a ", strong(constants_ui("info_duration.geno.ld"), "-month"), " delay and costs ", strong(constants_ui("info_cost.geno.ld"), " plot"), " (", constants_ui("info_cost.geno.ld.mendels"), " Mendels).",
          " ", strong("Single-SNP genotyping"), " has a ", strong(constants_ui("info_duration.geno.single"), "-month"), " delay and costs ", strong(constants_ui("info_cost.geno.single"), " plot"), " (", constants_ui("info_cost.geno.single.mendels"), " Mendels)."
        ),
        p(strong("Budget"), ": each team starts with a total budget of ", strong(constants_ui("info_initial.budget"), " Mendels"), ", fully available from the start."),
        div(
          uiOutput("costSummaryTable")
        ),
        h2("Final trial"),
        p(
          "At the end of the game, each team will have to propose to register their best genotypes (excluding those from the initial collection and not more than", constants_ui("info_maxEvalInds"), ").",
          " The registration fee is ", constants_ui("info_cost.register.mendels")
        ),
        p(
          " Each of them must meet the DHS criteria, which will be assessed primarily on their heterozygosity: < 3%.",
          " They must also meet the VATE criteria corresponding to a minimum of ", constants_ui("info_register.min.trait1.percent"), "% of the flower production of the control lines (known at the beginning of the program).",
          " Varieties below the ", constants_ui("info_register.min.trait2"), " per thousand of ", shiny::em("sepmetin"), " will be eliminated.",
          " Resistant varieties will have a bonus."
        ),
        p(
          " Availability of seed should be sufficient.",
          " The proposed genotype must therefore have been tested at least once in a plot to ensure that sufficient seed is available to send to the evaluators."
        ),
        h2("Usage"),
        p(
          "Before making any request, such as phenotyping, you need to log in (tab 'Identification').",
          " To get a sense of how the interface works, you can use the 'test' breeder with the 'tester' status.",
          " If you are playing in a common session, ask your game master to create a breeder for you.",
          "Once you are all set, start to devise your strategy and then... let's play!"
        ),
        h2("Advice"),
        p("For your selection to work, you better analyze the initial data carefully: the 'Theory' tab can be helpful."),
        p("Key concepts: heritability, breeding values, additive genetic variance, expected selection gain, selection intensity, genetic architecture, QTL detection, genomic prediction"),
        p(
          "Softwares:",
          " ", a("beanplot", href = "https://cran.r-project.org/package=beanplot", target = "_blank"),
          ", ", a("lme4", href = "https://cran.r-project.org/package=lme4", target = "_blank"),
          ", ", a("MM4LMM", href = "https://cran.r-project.org/package=MM4LMM", target = "_blank"),
          ", ", a("SpATS", href = "https://cran.r-project.org/package=SpATS", target = "_blank"),
          ", ", a("breedR", href = "https://github.com/famuvie/breedR", target = "blank_"),
          ", ", a("MuMIn", href = "https://cran.r-project.org/package=MuMIn", target = "_blank"),
          ", ", a("QTLRel", href = "https://cran.r-project.org/package=QTLRel", target = "blank_"),
          ", ", a("rrBLUP", href = "https://cran.r-project.org/package=rrBLUP", target = "blank_"),
          ", ", a("BGLR", href = "https://cran.r-project.org/package=BGLR", target = "blank_"),
          ", ", a("glmnet", href = "https://cran.r-project.org/package=glmnet", target = "blank_"),
          ", ", a("varbvs", href = "https://cran.r-project.org/package=varbvs", target = "_blank"),
          ", ", a("mlmm.gwas", href = "https://cran.r-project.org/package=mlmm.gwas", target = "_blank"),
          ", ", a("cvTools", href = "https://cran.r-project.org/package=cvTools", target = "blank_"),
          ", ", a("caret", href = "https://cran.r-project.org/package=caret", target = "blank_")
        )
      )
    ), # close shinydashboard::box

    shinydashboard::box(
      height = 400, width = 12, title = "Initial data",
      p("Select and download each file:"),
      selectInput("iniDataFile", "", choices = list.files(DATA_INITIAL_DATA)),
      downloadButton("dwnlIniData", "Download your file")
    ),
    if (debugDisplay) {
      shinydashboard::box(
        height = 300, width = 12, title = "debug",
        verbatimTextOutput("infoDebug")
      )
    }
  ) # close fluidRow
) # close tabItem
