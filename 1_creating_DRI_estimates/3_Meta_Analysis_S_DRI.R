###################### GROUPING AND META-ANALYSIS
#######################################################################

############## PACKAGES #################
library(stringr)
library(dplyr)
library(data.table)
library(meta)
library(ggplot2)


source('1_creating_DRI_estimates/AMRUCR_functions.R')

########### DATA ########################
load("outputs/los_all_mapped.RData")
load("outputs/EuSA_S.RData")
load("data_inputs/lit_review/whoc_cc_2019USD.RData")
load("data_inputs/lit_review/who_whoc_wb.RData")

######### FUNCTIONS ###########
who_whoc_wb <- as.data.table(who_whoc_wb)
who_whoc_wb <- who_whoc_wb[!is.na(iso3c)] ## although none got dropped in last run just for running purposes

as.numeric.factor <- function(x) {as.numeric(as.character(x))}

# ## set the number of sample runs 
# n.samples <- 1000

## setting the names to match those used in the function below
los_all_mapped[ , TE := TE_S]
los_all_mapped[ , seTE := seTE_S]


################******** META ANALYSIS LOS ********############
los.output.cc.S <- meta.grouping.S(los_all_mapped)

save(los.output.cc.S, file="outputs/los_SE_TE.RData")
