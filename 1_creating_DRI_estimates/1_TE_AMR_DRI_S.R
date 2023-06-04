########### MAPPING Susceptible DATA 
############## PACKAGES #################

library(stringr)
library(dplyr)
library(data.table)
library(countrycode)
library(wbstats)
library(meta)
library(tabulizer)
library(tidyr)

############ READ IN FILES ##############

lit_all <- read.csv("data_inputs/lit_review/lit_input_all.csv")  ## literature review DET

############ READ IN DATA AND FUNCTIONS FROM AMRUCR ##############

source('1_creating_DRI_estimates/AMRUCR_functions.R')
load('data_inputs/lit_review/wan_table1.RData')
load('data_inputs/lit_review/wan_table2.RData')

##### creating csv file of the studies used ####

## grouping by the reference id 
amr <- subset(lit_all, lit_all$review_marker=="AMR" )
dri <- subset(lit_all, lit_all$review_marker=="DRI" )

## rename one df cols so that in merge can distinguish
dri <- dri %>% rename_all(paste0, "_DRI")

## revert merging column names
# added drug, bug, place in case studies have multiple
names(dri)[names(dri) == 'retrieval_DRI'] <- 'retrieval'
names(dri)[names(dri) == 'los_DRI'] <- 'los'
names(dri)[names(dri) == 'bacteria.code_DRI'] <- 'bacteria.code'
names(dri)[names(dri) == 'exposed.R_DRI'] <- 'exposed.R'
names(dri)[names(dri) == 'syndrome_DRI'] <- 'syndrome'
names(dri)[names(dri) == 'country_DRI'] <- 'country'

## merge together
all <- merge(amr,dri,by=c("retrieval","los","bacteria.code","exposed.R",
                          "syndrome","country"))

## just keeping LOS estimates for now to avoid complications in combinations 
all <- subset(all, all$los==1)

all <- as.data.table(all)

#### calculating treatment effects for each then merging together again

## inefficient in that other results are also calculated, but easy in terms 
# of treatment effect estimation and combination 

## grouping by the reference id 
amr <- as.data.table(subset(lit_all, lit_all$review_marker=="AMR"))
dri <-  as.data.table(subset(lit_all, lit_all$review_marker=="DRI" ))

as.numeric.factor <- function(x) {as.numeric(as.character(x))}

los.AMR <- TE_creator_S(amr)

## rename results needed for meta-analyses for when combined
names(los.AMR)[names(los.AMR) == 'TE'] <- 'TE_AMR'
names(los.AMR)[names(los.AMR) == 'seTE'] <- 'seTE_AMR'

los.DRI <- TE_creator_S(dri)
## rename one df cols so that in merge can distinguish
los.DRI <- los.DRI %>% rename_all(paste0, "_DRI")

## revert merging column names
# added drug, bug, place in case studies have multiple
names(los.DRI)[names(los.DRI) == 'retrieval_DRI'] <- 'retrieval'
names(los.DRI)[names(los.DRI) == 'los_DRI'] <- 'los'
names(los.DRI)[names(los.DRI) == 'bacteria.code_DRI'] <- 'bacteria.code'
names(los.DRI)[names(los.DRI) == 'exposed.R_DRI'] <- 'exposed.R'
names(los.DRI)[names(los.DRI) == 'syndrome_DRI'] <- 'syndrome'
names(los.DRI)[names(los.DRI) == 'country_DRI'] <- 'country'
names(los.DRI)[names(los.DRI) == 'study_ref_DRI'] <- 'study_ref'

## merge together
los.all <- merge(los.AMR,los.DRI,by=c("retrieval","study_ref","los","bacteria.code","exposed.R",
                          "syndrome","country"))

## just keeping LOS estimates for now to avoid complications in combinations 
los.all <- as.data.table(subset(los.all, los.all$los==1))
## drops 2 studies

# creating the susceptible cost difference
los.all[ , TE_S:= TE_DRI-TE_AMR]
los.all[ , seTE_S := sqrt(((seTE_AMR)^2)+((seTE_DRI)^2))]

save(los.all, file="outputs/los_all_amrdris.RData")
