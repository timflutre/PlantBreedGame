#! /usr/bin/env Rscript

show_help <- function() {
  cat('
Usage: bump_version.R <minor|major|patch>

Update VERSION file by increasing the version number by 1.

Arguments:
  minor (default) Update minor version eg. 1.2.3 -> 1.3.0
  major           Update major version eg. 1.2.3 -> 2.0.0
  patch           Update patch version eg. 1.2.3 -> 1.2.4

Example:
  ./bump_version.R patch
')
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 1) {
  cat("Error, too many arguments\n")
  show_help()
  q("no", 1)
}
if (length(args) == 0) {
  update <- "minor"
} else {
  update <- args[[1]]
}

accepted_update <- c("major", "minor", "patch")
if (!update %in% accepted_update) {
  cat(paste0("Error, unrecognised version update `", update, "`.\n"))
  show_help()
  q("no", 1)
}

init_version <- readLines("VERSION")
version <- as.numeric(strsplit(x = init_version, "\\.")[[1]])

if (identical(update, "major")) {
  version[1] <- version[1] + 1
  version[2] <- 0
  version[3] <- 0
}
if (identical(update, "minor")) {
  version[2] <- version[2] + 1
  version[3] <- 0
}
if (identical(update, "patch")) {
  version[3] <- version[3] + 1
}
version <- paste0(version, collapse = ".")
writeLines(version, "VERSION")

cat(paste("Version updated from", init_version, "to", version))

q("no", 0)
