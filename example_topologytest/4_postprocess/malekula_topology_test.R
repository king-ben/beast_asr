setwd(dirname(rstudioapi::getSourceEditorContext()$path))


library(devtools)
library(XML)

# load functions from github
source_url("https://raw.githubusercontent.com/king-ben/beast_asr/main/asr_functions.R")

tt <- topology_test("../2_unconstrained/site_likelihoods.txt", #path to output topology hypothesis 1 
                    "../3_constrained/site_likelihoods.txt", #path to output topology hypothesis 2 
                    "unconstrained",#label for plot topology 1
                    "constrained",#label for plot topology 2
                    burnin=0.2,#how much burnin to remove
                    hpd.level = 1.0,#which hpd distribution needs to be not overlapping to consider a cognate significant, lower numbers return more cognates
                    colors = c("#89a0bf","#87230d")
                    )

tt$plot