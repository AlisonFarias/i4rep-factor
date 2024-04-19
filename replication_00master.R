# replication_00master.R

global.libraries <- c("ggtext", "viridis", "viridisLite", "tikzDevice", "scales", "ggmap", "rgeos", "gridExtra", "lfe",
                      "Matrix", "rgdal", "sp", "readstata13", "forcats", "stringr", "dplyr", "purrr", "readr", "tidyr",
                      "tibble", "ggplot2", "tidyverse", "magrittr", "foreign", "matrixStats", "httr", "jsonlite", "modelr",
                      "Formula", "assertthat", "cellranger", "pillar", "backports", "lattice", "glue", "gridtext", "rvest",
                      "colorspace", "sandwich", "plyr", "pkgconfig", "broom", "haven", "xtable", "jpeg", "generics",
                      "ellipsis", "withr", "cli", "crayon", "readxl", "fs", "fansi", "xml2", "tools", "hms", "RgoogleMaps",
                      "lifecycle", "munsell", "reprex", "compiler", "rlang", "rstudioapi", "rjson", "filehash", "bitops",
                      "gtable", "DBI", "R6", "zoo", "lubridate", "utf8", "stringi", "parallel", "Rcpp", "vctrs", "png",
                      "dbplyr", "tidyselect")
pkgTest <- function(x) {
  if(!require(x, character.only = TRUE)) {
    install.packages(x, dep = TRUE)
  }
  return("OK")
}
results <- sapply(as.list(global.libraries), pkgTest)
rm(list = c("pkgTest", "global.libraries", "results"))
restartSession()

# replicationfolder <- "/users/jeremymagruder/Dropbox/Irrigation_Rwanda/Data/analysis_master/20220408replication/"
# replicationfolder <- "C:/Users/sete9/Google Drive USC/Meu Drive/i4rep-factor/original replication files/"

projectfolder <- "C:/Users/sete9/Google Drive USC/Meu Drive/i4rep-factor/" # CHANGE THIS TO YOUR LOCAL PATH

# setwd(paste0(replicationfolder, "")) # does not work here
# install.packages(file.path(projectfolder, "rgdal_1.6-7.tar.gz"), repos = NULL, type = "source") # does not work

source("original_01construct.R") # you have to comment loading of rgdal and rgeos in original_01construct.R

# We take the cleaned datafiles output by 01construct.R and save these .rdas to .dta so that we may process in Stata. 

library(foreign)

dtafolder <- file.path(projectfolder, "dta")

write.dta(balanceh, file.path(dtafolder, "balanceh.dta"))
write.dta(balancehp, file.path(dtafolder, "balancehp.dta"))
write.dta(balancesphp, file.path(dtafolder, "balancesphp.dta"))
write.dta(balancemiphp, file.path(dtafolder, "balancemiphp.dta"))
write.dta(analysish, file.path(dtafolder, "analysish.dta"))
write.dta(analysishs, file.path(dtafolder, "analysishs.dta"))
write.dta(analysismiphps, file.path(dtafolder, "analysismiphps.dta"))
write.dta(analysislandsat, file.path(dtafolder, "analysislandsat.dta"))
write.dta(analysismiplandsat, file.path(dtafolder, "analysismiplandsat.dta"))
write.dta(analysishps, file.path(dtafolder, "analysishps.dta"))

source("original_02analysis.R")