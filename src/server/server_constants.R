constantsReactive <- reactivePoll(
  5000,
  session,
  function() {
    if (file.exists(DATA_DB)) {
      file.info(DATA_DB)$mtime[1]
    } else {
      ""
    }
  },
  getBreedingGameConstants
)

# Information page: ("How to play?")
constants_server("chr.length.Mb", constantsReactive)("info_chr.length.Mb")
constants_server("cost.allof", constantsReactive)("info_cost.allof")
constants_server("cost.allof.mendels", constantsReactive)("info_cost.allof.mendels")
constants_server("cost.autof", constantsReactive)("info_cost.autof")
constants_server("cost.autof.mendels", constantsReactive)("info_cost.autof.mendels")
constants_server("cost.geno.hd", constantsReactive)("info_cost.geno.hd")
constants_server("cost.geno.hd.mendels", constantsReactive)("info_cost.geno.hd.mendels")
constants_server("cost.geno.ld", constantsReactive)("info_cost.geno.ld")
constants_server("cost.geno.ld.mendels", constantsReactive)("info_cost.geno.ld.mendels")
constants_server("cost.geno.single", constantsReactive)("info_cost.geno.single")
constants_server("cost.geno.single.mendels", constantsReactive)("info_cost.geno.single.mendels")
constants_server("cost.haplodiplo", constantsReactive)("info_cost.haplodiplo")
constants_server("cost.haplodiplo.mendels", constantsReactive)("info_cost.haplodiplo.mendels")
constants_server("cost.pheno.field", constantsReactive)("info_cost.pheno.field")
constants_server("cost.pheno.patho", constantsReactive)("info_cost.pheno.patho")
constants_server("cost.pheno.patho.mendels", constantsReactive)("info_cost.pheno.patho.mendels")
constants_server("cost.register.mendels", constantsReactive)("info_cost.register.mendels")
constants_server("duration.allof", constantsReactive)("info_duration.allof")
constants_server("duration.autof", constantsReactive)("info_duration.autof")
constants_server("duration.geno.hd", constantsReactive)("info_duration.geno.hd")
constants_server("duration.geno.ld", constantsReactive)("info_duration.geno.ld")
constants_server("duration.geno.single", constantsReactive)("info_duration.geno.single")
constants_server("duration.haplodiplo", constantsReactive)("info_duration.haplodiplo")
constants_server("duration.pheno.field", constantsReactive)("info_duration.pheno.field")
constants_server("duration.pheno.patho", constantsReactive)("info_duration.pheno.patho")
constants_server("first.year", constantsReactive)("info_first.year")
constants_server("generations.per.year", constantsReactive)("info_generations.per.year")
constants_server("initial.budget", constantsReactive)("info_initial.budget")
constants_server("max.nb.haplodiplos", constantsReactive)("info_max.nb.haplodiplos")
constants_server("max.upload.pheno.field", constantsReactive)("info_max.upload.pheno.field")
constants_server("maxEvalInds", constantsReactive)("info_maxEvalInds")
constants_server("mu.trait1", constantsReactive)("info_mu.trait1")
constants_server("mu.trait2", constantsReactive)("info_mu.trait2")
constants_server("nb.chrs", constantsReactive)("info_nb.chrs")
constants_server("nb.controls", constantsReactive)("info_nb.controls")
constants_server("nb.genotyped.coll", constantsReactive)("info_nb.genotyped.coll")
constants_server("nb.inds.denovo", constantsReactive)("info_nb.inds.denovo")
constants_server("nb.phenotyped.coll", constantsReactive)("info_nb.phenotyped.coll")
constants_server("nb.plots", constantsReactive)("info_nb.plots")
constants_server("nb.plots", constantsReactive)("info_nb.plots2")
constants_server("nb.plots.per.ctl", constantsReactive)("info_nb.plots.per.ctl")
constants_server("nb.snps.hd", constantsReactive)("info_nb.snps.hd")
constants_server("nb.snps.ld", constantsReactive)("info_nb.snps.ld")
constants_server("nb.years.per.ctl", constantsReactive)("info_nb.years.per.ctl")
constants_server("pheno.data.availability.date", constantsReactive)("info_pheno.data.availability.date")
constants_server("register.min.trait1.percent", constantsReactive)("info_register.min.trait1.percent")
constants_server("register.min.trait2", constantsReactive)("info_register.min.trait2")

# identification / home page:
constants_server("maxEvalInds", constantsReactive)("home_maxEvalInds")
constants_server("cost.register.mendels", constantsReactive)("home_cost.register.mendels")
constants_server("", constantsReactive)("")

# plant material:
constants_server("cost.allof", constantsReactive)("pltmat_cost.allof")
constants_server("cost.allof.mendels", constantsReactive)("pltmat_cost.allof.mendels")
constants_server("cost.autof", constantsReactive)("pltmat_cost.autof")
constants_server("cost.autof.mendels", constantsReactive)("pltmat_cost.autof.mendels")
constants_server("cost.haplodiplo", constantsReactive)("pltmat_cost.haplodiplo")
constants_server("cost.haplodiplo.mendels", constantsReactive)("pltmat_cost.haplodiplo.mendels")
constants_server("duration.allof", constantsReactive)("pltmat_duration.allof")
constants_server("duration.autof", constantsReactive)("pltmat_duration.autof")
constants_server("duration.haplodiplo", constantsReactive)("pltmat_duration.haplodiplo")
constants_server("max.nb.haplodiplos", constantsReactive)("pltmat_max.nb.haplodiplos")

# phenotyping page:
constants_server("cost.pheno.field", constantsReactive)("pheno_cost.pheno.field")
constants_server("cost.pheno.patho", constantsReactive)("pheno_cost.pheno.patho")
constants_server("cost.pheno.patho.mendels", constantsReactive)("pheno_cost.pheno.patho.mendels")
constants_server("duration.pheno.field", constantsReactive)("pheno_duration.pheno.field")
constants_server("duration.pheno.patho", constantsReactive)("pheno_duration.pheno.patho")
constants_server("max.upload.pheno.field", constantsReactive)("pheno_max.upload.pheno.field")
constants_server("nb.plots", constantsReactive)("pheno_nb.plots")
constants_server("nb.plots", constantsReactive)("pheno_nb.plots_2")
constants_server("pheno.data.availability.date", constantsReactive)("pheno_pheno.data.availability.date")


# genotyping page:
constants_server("cost.geno.hd", constantsReactive)("geno_cost.geno.hd")
constants_server("cost.geno.hd.mendels", constantsReactive)("geno_cost.geno.hd.mendels")
constants_server("cost.geno.ld", constantsReactive)("geno_cost.geno.ld")
constants_server("cost.geno.ld.mendels", constantsReactive)("geno_cost.geno.ld.mendels")
constants_server("cost.geno.single", constantsReactive)("geno_cost.geno.single")
constants_server("duration.geno.hd", constantsReactive)("geno_duration.geno.hd")
constants_server("duration.geno.ld", constantsReactive)("geno_duration.geno.ld")
constants_server("duration.geno.single", constantsReactive)("geno_duration.geno.single")
constants_server("nb.snps.hd", constantsReactive)("geno_nb.snps.hd")
constants_server("nb.snps.ld", constantsReactive)("geno_nb.snps.ld")
