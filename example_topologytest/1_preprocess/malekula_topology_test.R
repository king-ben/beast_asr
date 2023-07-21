setwd(dirname(rstudioapi::getSourceEditorContext()$path))


library(devtools)
library(XML)

# load functions from github
source_url("https://raw.githubusercontent.com/king-ben/beast_asr/main/asr_functions.R")

# vector of cognate names
wn <- find_site_names("Malekula.nex")

# table of concepts
wp <- find_partitions("Malekula.nex")

# make xml code to log likelihood of each cognate
sp <- siteprobs_blocks(wp, wn, logevery = 10000)

#save xml
saveXML(sp, "siteprobs.xml")
