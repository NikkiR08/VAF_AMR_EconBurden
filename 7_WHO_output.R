####### GETTING DATA TABLES FROM RESULTS ####################################

########## this is to provide tables that will be used in WHO reporting

########################## temporary update for hib and staph

#### loading packages
library(tidyr)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(readxl)

##### read in data

myfiles = list.files(path="C:/Users/nichola.naylor/Documents/WHO_2023/VAF_AMR_EconBurden/outputs/fulloutput_chunks/",
                     pattern="*.RData", full.names = TRUE)

#### final table for database ######
hospital_cost_l <- list()
for (i in 1:length(myfiles)){
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_cost <- vaccine_output_dt %>% group_by(WHO.Region, Pathogen, Infectious.syndrome,
                                                  vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE)) %>%
    group_by(WHO.Region, Pathogen, Infectious.syndrome, vaccine_id) %>%
    summarise(Mean_avert_costing = mean(who_region_cost_A, na.rm=TRUE),
              LOWUI_avert_costing = quantile(who_region_cost_A,0.025, na.rm = TRUE),
              HIGHUI_avert_costing = quantile(who_region_cost_A,0.975, na.rm = TRUE),
              Mean_total_costing = mean(who_region_cost_T, na.rm=TRUE),
              LOWUI_total_costing = quantile(who_region_cost_T,0.025, na.rm = TRUE),
              HIGHUI_total_costing = quantile(who_region_cost_T,0.975, na.rm = TRUE),
              Mean_avert_days = mean(who_region_days_A, na.rm=TRUE),
              LOWUI_avert_days = quantile(who_region_days_A,0.025, na.rm = TRUE),
              HIGHUI_avert_days = quantile(who_region_days_A,0.975, na.rm = TRUE),
              Mean_total_days = mean(who_region_days_T, na.rm=TRUE),
              LOWUI_total_days = quantile(who_region_days_T,0.025, na.rm = TRUE),
              HIGHUI_total_days = quantile(who_region_days_T,0.975, na.rm = TRUE)) %>%
    mutate(across(Mean_avert_costing:HIGHUI_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( mean_ci_total_cost = str_c(Mean_total_costing, " ( ", LOWUI_total_costing, "-",
                                  HIGHUI_total_costing," ) "),
            mean_ci_averted_cost = str_c(Mean_avert_costing, " ( ", LOWUI_avert_costing, "-",
                                   HIGHUI_avert_costing," ) "),
            mean_ci_total_days = str_c(Mean_total_days, " ( ", LOWUI_total_days, "-",
                                       HIGHUI_total_days," ) "),
            mean_ci_averted_days = str_c(Mean_avert_days, " ( ", LOWUI_avert_days, "-",
                                         HIGHUI_avert_days," ) ")
          ) %>%
    as.data.table()
  hospital_cost_l[[i]] <- hospital_cost
}

hospital_c <- rbindlist(hospital_cost_l)
hospital_c <- hospital_c[!is.na(Pathogen)]
hospital_c <- hospital_c[ , -c("Mean_avert_costing",  "LOWUI_avert_costing",
                               "HIGHUI_avert_costing", "Mean_total_costing" ,
                               "LOWUI_total_costing" , "HIGHUI_total_costing" ,
                               "Mean_avert_days",  "LOWUI_avert_days",
                               "HIGHUI_avert_days", "Mean_total_days" ,
                               "LOWUI_total_days" , "HIGHUI_total_days" )]
hospital_c <- dcast(hospital_c, Pathogen + Infectious.syndrome +
                      vaccine_id ~ WHO.Region, value.var = c("mean_ci_total_cost",
                                                             "mean_ci_averted_cost",
                                                             "mean_ci_total_days",
                                                             "mean_ci_averted_days"))

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

# ###!!! not including those to mapped to any vaccine ID
# hospital_c <- hospital_c[vaccine_id!="_NA_NA___"] ## not needed now removed earlier on

write.csv(hospital_c, file="outputs/END_hospital_costs_output.csv")
### note in the one sent there is also "total" but not added here in this version, find total vaccine/pathogen level results below
rm(hospital_c)
rm(hospital_cost_l)


#### global ######
hospital_cost_l <- list()
for (i in 1:length(myfiles)){
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_cost <- vaccine_output_dt %>% group_by(Pathogen, Infectious.syndrome,
                                                  vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE)) %>%
    group_by(Pathogen, Infectious.syndrome, vaccine_id) %>%
    summarise(Mean_avert_costing = mean(who_region_cost_A, na.rm=TRUE),
              LOWUI_avert_costing = quantile(who_region_cost_A,0.025, na.rm = TRUE),
              HIGHUI_avert_costing = quantile(who_region_cost_A,0.975, na.rm = TRUE),
              Mean_total_costing = mean(who_region_cost_T, na.rm=TRUE),
              LOWUI_total_costing = quantile(who_region_cost_T,0.025, na.rm = TRUE),
              HIGHUI_total_costing = quantile(who_region_cost_T,0.975, na.rm = TRUE),
              Mean_avert_days = mean(who_region_days_A, na.rm=TRUE),
              LOWUI_avert_days = quantile(who_region_days_A,0.025, na.rm = TRUE),
              HIGHUI_avert_days = quantile(who_region_days_A,0.975, na.rm = TRUE),
              Mean_total_days = mean(who_region_days_T, na.rm=TRUE),
              LOWUI_total_days = quantile(who_region_days_T,0.025, na.rm = TRUE),
              HIGHUI_total_days = quantile(who_region_days_T,0.975, na.rm = TRUE)) %>%
    mutate(across(Mean_avert_costing:HIGHUI_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( mean_ci_total_cost = str_c(Mean_total_costing, " ( ", LOWUI_total_costing, "-",
                                       HIGHUI_total_costing," ) "),
            mean_ci_averted_cost = str_c(Mean_avert_costing, " ( ", LOWUI_avert_costing, "-",
                                         HIGHUI_avert_costing," ) "),
            mean_ci_total_days = str_c(Mean_total_days, " ( ", LOWUI_total_days, "-",
                                       HIGHUI_total_days," ) "),
            mean_ci_averted_days = str_c(Mean_avert_days, " ( ", LOWUI_avert_days, "-",
                                         HIGHUI_avert_days," ) ")
    ) %>%
    as.data.table()
  hospital_cost_l[[i]] <- hospital_cost
}

hospital_c <- rbindlist(hospital_cost_l)
hospital_c <- hospital_c[!is.na(Pathogen)]
hospital_c <- hospital_c[ , -c("Mean_avert_costing",  "LOWUI_avert_costing",
                               "HIGHUI_avert_costing", "Mean_total_costing" ,
                               "LOWUI_total_costing" , "HIGHUI_total_costing" ,
                               "Mean_avert_days",  "LOWUI_avert_days",
                               "HIGHUI_avert_days", "Mean_total_days" ,
                               "LOWUI_total_days" , "HIGHUI_total_days" )]

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]
write.csv(hospital_c, file="outputs/END_hospital_costs_output_global.csv")
### note in the one sent there is also "total" but not added here in this version, find total vaccine/pathogen level results below
rm(hospital_c)
rm(hospital_cost_l)

############### PATHOGEN - ACROSS ALL SYNDROMES #######################
hospital_cost_l <- list()
for (i in 1:length(myfiles)){
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_cost <- vaccine_output_dt %>% group_by(WHO.Region, Pathogen, 
                                                  vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE)) %>%
    group_by(WHO.Region, Pathogen,vaccine_id) %>%
    summarise(Mean_avert_costing = mean(who_region_cost_A, na.rm=TRUE),
              LOWUI_avert_costing = quantile(who_region_cost_A,0.025, na.rm = TRUE),
              HIGHUI_avert_costing = quantile(who_region_cost_A,0.975, na.rm = TRUE),
              Mean_total_costing = mean(who_region_cost_T, na.rm=TRUE),
              LOWUI_total_costing = quantile(who_region_cost_T,0.025, na.rm = TRUE),
              HIGHUI_total_costing = quantile(who_region_cost_T,0.975, na.rm = TRUE),
              Mean_avert_days = mean(who_region_days_A, na.rm=TRUE),
              LOWUI_avert_days = quantile(who_region_days_A,0.025, na.rm = TRUE),
              HIGHUI_avert_days = quantile(who_region_days_A,0.975, na.rm = TRUE),
              Mean_total_days = mean(who_region_days_T, na.rm=TRUE),
              LOWUI_total_days = quantile(who_region_days_T,0.025, na.rm = TRUE),
              HIGHUI_total_days = quantile(who_region_days_T,0.975, na.rm = TRUE)) %>%
    mutate(across(Mean_avert_costing:HIGHUI_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( mean_ci_total_cost = str_c(Mean_total_costing, " ( ", LOWUI_total_costing, "-",
                                       HIGHUI_total_costing," ) "),
            mean_ci_averted_cost = str_c(Mean_avert_costing, " ( ", LOWUI_avert_costing, "-",
                                         HIGHUI_avert_costing," ) "),
            mean_ci_total_days = str_c(Mean_total_days, " ( ", LOWUI_total_days, "-",
                                       HIGHUI_total_days," ) "),
            mean_ci_averted_days = str_c(Mean_avert_days, " ( ", LOWUI_avert_days, "-",
                                         HIGHUI_avert_days," ) ")
    ) %>%
    as.data.table()
  hospital_cost_l[[i]] <- hospital_cost
}

hospital_c <- rbindlist(hospital_cost_l)
hospital_c <- hospital_c[!is.na(Pathogen)]
hospital_c <- hospital_c[ , -c("Mean_avert_costing",  "LOWUI_avert_costing",
                               "HIGHUI_avert_costing", "Mean_total_costing" ,
                               "LOWUI_total_costing" , "HIGHUI_total_costing" ,
                               "Mean_avert_days",  "LOWUI_avert_days",
                               "HIGHUI_avert_days", "Mean_total_days" ,
                               "LOWUI_total_days" , "HIGHUI_total_days" )]
hospital_c <- dcast(hospital_c, Pathogen +
                      vaccine_id ~ WHO.Region, value.var = c("mean_ci_total_cost",
                                                             "mean_ci_averted_cost",
                                                             "mean_ci_total_days",
                                                             "mean_ci_averted_days"))

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]


write.csv(hospital_c, file="outputs/END_hospital_costs_output_ALLSYNDROME.csv")
### note in the one sent there is also "total" but not added here in this version, find total vaccine/pathogen level results below
rm(hospital_c)
rm(hospital_cost_l)


#### global ######
hospital_cost_l <- list()
for (i in 1:length(myfiles)){
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_cost <- vaccine_output_dt %>% group_by(Pathogen, 
                                                  vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE)) %>%
    group_by(Pathogen, vaccine_id) %>%
    summarise(Mean_avert_costing = mean(who_region_cost_A, na.rm=TRUE),
              LOWUI_avert_costing = quantile(who_region_cost_A,0.025, na.rm = TRUE),
              HIGHUI_avert_costing = quantile(who_region_cost_A,0.975, na.rm = TRUE),
              Mean_total_costing = mean(who_region_cost_T, na.rm=TRUE),
              LOWUI_total_costing = quantile(who_region_cost_T,0.025, na.rm = TRUE),
              HIGHUI_total_costing = quantile(who_region_cost_T,0.975, na.rm = TRUE),
              Mean_avert_days = mean(who_region_days_A, na.rm=TRUE),
              LOWUI_avert_days = quantile(who_region_days_A,0.025, na.rm = TRUE),
              HIGHUI_avert_days = quantile(who_region_days_A,0.975, na.rm = TRUE),
              Mean_total_days = mean(who_region_days_T, na.rm=TRUE),
              LOWUI_total_days = quantile(who_region_days_T,0.025, na.rm = TRUE),
              HIGHUI_total_days = quantile(who_region_days_T,0.975, na.rm = TRUE)) %>%
    mutate(across(Mean_avert_costing:HIGHUI_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( mean_ci_total_cost = str_c(Mean_total_costing, " ( ", LOWUI_total_costing, "-",
                                       HIGHUI_total_costing," ) "),
            mean_ci_averted_cost = str_c(Mean_avert_costing, " ( ", LOWUI_avert_costing, "-",
                                         HIGHUI_avert_costing," ) "),
            mean_ci_total_days = str_c(Mean_total_days, " ( ", LOWUI_total_days, "-",
                                       HIGHUI_total_days," ) "),
            mean_ci_averted_days = str_c(Mean_avert_days, " ( ", LOWUI_avert_days, "-",
                                         HIGHUI_avert_days," ) ")
    ) %>%
    as.data.table()
  hospital_cost_l[[i]] <- hospital_cost
}

hospital_c <- rbindlist(hospital_cost_l)
hospital_c <- hospital_c[!is.na(Pathogen)]
hospital_c <- hospital_c[ , -c("Mean_avert_costing",  "LOWUI_avert_costing",
                               "HIGHUI_avert_costing", "Mean_total_costing" ,
                               "LOWUI_total_costing" , "HIGHUI_total_costing" ,
                               "Mean_avert_days",  "LOWUI_avert_days",
                               "HIGHUI_avert_days", "Mean_total_days" ,
                               "LOWUI_total_days" , "HIGHUI_total_days" )]

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

write.csv(hospital_c, file="outputs/END_hospital_costs_output_global_ALLSYNDROME.csv")
### note in the one sent there is also "total" but not added here in this version, find total vaccine/pathogen level results below
rm(hospital_c)
rm(hospital_cost_l)


################## VACCINE SPECIFIC GROUPINGS ##################

### psuedo  - "blood stream, lower respiratory and thorax"   
###### in our results equivalent to "BSI" +  "LRI and thorax infections"
### strep -"blood stream, central nervous system, cardiac, and lower respiratory"
##### in our results equivalent to "BSI", "CNS infections", "Cardiac infections" and "LRI and thorax infections"

#### psuedo #####
##### read in data

load( "outputs/fulloutput_chunks/resultsPseu8.RData")
psuedo1 <- vaccine_output_dt
load("outputs/fulloutput_chunks/resultsPseu15.RData")
psuedo2 <- vaccine_output_dt
rm(vaccine_output_dt)

psuedo <- rbind(psuedo1,psuedo2)
rm(psuedo1)
rm(psuedo2)

psuedo[WHO.Region=="PAHO", WHO.Region := "AMRO"]

## just keep syndromes wanted
psuedo <- psuedo[Infectious.syndrome=="BSI"|
                   Infectious.syndrome=="LRI and thorax infections"]

spec.syndrome <- function(dt){
  hospital_c <- dt %>% group_by(WHO.Region, Pathogen, 
                                                  vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE)) %>%
    group_by(WHO.Region, Pathogen, vaccine_id) %>%
    summarise(Mean_avert_costing = mean(who_region_cost_A, na.rm=TRUE),
              LOWUI_avert_costing = quantile(who_region_cost_A,0.025, na.rm = TRUE),
              HIGHUI_avert_costing = quantile(who_region_cost_A,0.975, na.rm = TRUE),
              Mean_total_costing = mean(who_region_cost_T, na.rm=TRUE),
              LOWUI_total_costing = quantile(who_region_cost_T,0.025, na.rm = TRUE),
              HIGHUI_total_costing = quantile(who_region_cost_T,0.975, na.rm = TRUE),
              Mean_avert_days = mean(who_region_days_A, na.rm=TRUE),
              LOWUI_avert_days = quantile(who_region_days_A,0.025, na.rm = TRUE),
              HIGHUI_avert_days = quantile(who_region_days_A,0.975, na.rm = TRUE),
              Mean_total_days = mean(who_region_days_T, na.rm=TRUE),
              LOWUI_total_days = quantile(who_region_days_T,0.025, na.rm = TRUE),
              HIGHUI_total_days = quantile(who_region_days_T,0.975, na.rm = TRUE)) %>%
    mutate(across(Mean_avert_costing:HIGHUI_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( mean_ci_total_cost = str_c(Mean_total_costing, " ( ", LOWUI_total_costing, "-",
                                       HIGHUI_total_costing," ) "),
            mean_ci_averted_cost = str_c(Mean_avert_costing, " ( ", LOWUI_avert_costing, "-",
                                         HIGHUI_avert_costing," ) "),
            mean_ci_total_days = str_c(Mean_total_days, " ( ", LOWUI_total_days, "-",
                                       HIGHUI_total_days," ) "),
            mean_ci_averted_days = str_c(Mean_avert_days, " ( ", LOWUI_avert_days, "-",
                                         HIGHUI_avert_days," ) "))%>%
    as.data.table()

hospital_c <- hospital_c[ , -c("Mean_avert_costing",  "LOWUI_avert_costing",
                               "HIGHUI_avert_costing", "Mean_total_costing" ,
                               "LOWUI_total_costing" , "HIGHUI_total_costing" ,
                               "Mean_avert_days",  "LOWUI_avert_days",
                               "HIGHUI_avert_days", "Mean_total_days" ,
                               "LOWUI_total_days" , "HIGHUI_total_days" )]
hospital_c <- dcast(hospital_c, Pathogen + 
                      vaccine_id ~ WHO.Region, value.var = c("mean_ci_total_cost",
                                                             "mean_ci_averted_cost",
                                                             "mean_ci_total_days",
                                                             "mean_ci_averted_days"))

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]


return(hospital_c)
}

hospital_c_region <- spec.syndrome(psuedo) 
hospital_c_region[ , Infectious.syndrome:="BSI & LRI and thorax infections"]


spec.syndrome.global <- function(dt){
hospital_c <- dt %>% group_by(Pathogen, 
                                  vaccine_id, run) %>%
  summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
            who_region_cost_T = sum(cost_cases, na.rm=TRUE),
            who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
            who_region_days_T = sum(days_cases, na.rm=TRUE)) %>%
  group_by(Pathogen, vaccine_id) %>%
  summarise(Mean_avert_costing = mean(who_region_cost_A, na.rm=TRUE),
            LOWUI_avert_costing = quantile(who_region_cost_A,0.025, na.rm = TRUE),
            HIGHUI_avert_costing = quantile(who_region_cost_A,0.975, na.rm = TRUE),
            Mean_total_costing = mean(who_region_cost_T, na.rm=TRUE),
            LOWUI_total_costing = quantile(who_region_cost_T,0.025, na.rm = TRUE),
            HIGHUI_total_costing = quantile(who_region_cost_T,0.975, na.rm = TRUE),
            Mean_avert_days = mean(who_region_days_A, na.rm=TRUE),
            LOWUI_avert_days = quantile(who_region_days_A,0.025, na.rm = TRUE),
            HIGHUI_avert_days = quantile(who_region_days_A,0.975, na.rm = TRUE),
            Mean_total_days = mean(who_region_days_T, na.rm=TRUE),
            LOWUI_total_days = quantile(who_region_days_T,0.025, na.rm = TRUE),
            HIGHUI_total_days = quantile(who_region_days_T,0.975, na.rm = TRUE)) %>%
  mutate(across(Mean_avert_costing:HIGHUI_total_days,
                ~ format(., big.mark = ",", scientific = F)))%>%
  mutate( mean_ci_total_cost = str_c(Mean_total_costing, " ( ", LOWUI_total_costing, "-",
                                     HIGHUI_total_costing," ) "),
          mean_ci_averted_cost = str_c(Mean_avert_costing, " ( ", LOWUI_avert_costing, "-",
                                       HIGHUI_avert_costing," ) "),
          mean_ci_total_days = str_c(Mean_total_days, " ( ", LOWUI_total_days, "-",
                                     HIGHUI_total_days," ) "),
          mean_ci_averted_days = str_c(Mean_avert_days, " ( ", LOWUI_avert_days, "-",
                                       HIGHUI_avert_days," ) "))%>%
  as.data.table()

hospital_c <- hospital_c[ , -c("Mean_avert_costing",  "LOWUI_avert_costing",
                               "HIGHUI_avert_costing", "Mean_total_costing" ,
                               "LOWUI_total_costing" , "HIGHUI_total_costing" ,
                               "Mean_avert_days",  "LOWUI_avert_days",
                               "HIGHUI_avert_days", "Mean_total_days" ,
                               "LOWUI_total_days" , "HIGHUI_total_days" )]

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]
return(hospital_c)
}

hospital_c_global <- spec.syndrome.global(psuedo)
hospital_c_global[ , Infectious.syndrome:="BSI & LRI and thorax infections"]

psuedo <- merge(hospital_c_global, hospital_c_region, by="vaccine_id")

write.csv(psuedo, file="outputs/END_hospital_costs_output_psuedomonas.csv")

######## Strep pnuemo #########
rm(psuedo)

 ## "BSI", "CNS infections", "Cardiac infections" and "LRI and thorax infections"

## load files ###!!! this might need redoing if order of runs changes - check 1st row after loading & before assigning
load("outputs/fulloutput_chunks/resultsStre19.RData")
serotype.58 <- vaccine_output_dt

load("outputs/fulloutput_chunks/resultsStre20.RData")
improv.7 <- vaccine_output_dt

load("outputs/fulloutput_chunks/resultsStre23.RData")
serotype.58.eld <- vaccine_output_dt

load("outputs/fulloutput_chunks/resultsStre24.RData")
improv.7.eld <- vaccine_output_dt

load("outputs/fulloutput_chunks/resultsStre25.RData")
current <- vaccine_output_dt

load("outputs/fulloutput_chunks/resultsStre34.RData")
serotype.27 <- vaccine_output_dt

load("outputs/fulloutput_chunks/resultsStre35.RData")
improv.5 <- vaccine_output_dt

load("outputs/fulloutput_chunks/resultsStre36.RData")
serotype.27.eld <- vaccine_output_dt

load("outputs/fulloutput_chunks/resultsStre37.RData")
improv.5.eld <- vaccine_output_dt

rm(vaccine_output_dt)

### functions for grouping

spec.syndrome.strep <- function(dt){
  dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_c <- dt %>% group_by(WHO.Region, Pathogen,run) %>% ## difference here not by vaccine_id
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE)) %>%
    group_by(WHO.Region, Pathogen ) %>%
    summarise(Mean_avert_costing = mean(who_region_cost_A, na.rm=TRUE),
              LOWUI_avert_costing = quantile(who_region_cost_A,0.025, na.rm = TRUE),
              HIGHUI_avert_costing = quantile(who_region_cost_A,0.975, na.rm = TRUE),
              Mean_total_costing = mean(who_region_cost_T, na.rm=TRUE),
              LOWUI_total_costing = quantile(who_region_cost_T,0.025, na.rm = TRUE),
              HIGHUI_total_costing = quantile(who_region_cost_T,0.975, na.rm = TRUE),
              Mean_avert_days = mean(who_region_days_A, na.rm=TRUE),
              LOWUI_avert_days = quantile(who_region_days_A,0.025, na.rm = TRUE),
              HIGHUI_avert_days = quantile(who_region_days_A,0.975, na.rm = TRUE),
              Mean_total_days = mean(who_region_days_T, na.rm=TRUE),
              LOWUI_total_days = quantile(who_region_days_T,0.025, na.rm = TRUE),
              HIGHUI_total_days = quantile(who_region_days_T,0.975, na.rm = TRUE)) %>%
    mutate(across(Mean_avert_costing:HIGHUI_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( mean_ci_total_cost = str_c(Mean_total_costing, " ( ", LOWUI_total_costing, "-",
                                       HIGHUI_total_costing," ) "),
            mean_ci_averted_cost = str_c(Mean_avert_costing, " ( ", LOWUI_avert_costing, "-",
                                         HIGHUI_avert_costing," ) "),
            mean_ci_total_days = str_c(Mean_total_days, " ( ", LOWUI_total_days, "-",
                                       HIGHUI_total_days," ) "),
            mean_ci_averted_days = str_c(Mean_avert_days, " ( ", LOWUI_avert_days, "-",
                                         HIGHUI_avert_days," ) "))%>%
    as.data.table()
  
  hospital_c <- hospital_c[ , -c("Mean_avert_costing",  "LOWUI_avert_costing",
                                 "HIGHUI_avert_costing", "Mean_total_costing" ,
                                 "LOWUI_total_costing" , "HIGHUI_total_costing" ,
                                 "Mean_avert_days",  "LOWUI_avert_days",
                                 "HIGHUI_avert_days", "Mean_total_days" ,
                                 "LOWUI_total_days" , "HIGHUI_total_days" )]
  hospital_c <- dcast(hospital_c, Pathogen ~ WHO.Region, value.var = c("mean_ci_total_cost",
                                                                       "mean_ci_averted_cost",
                                                                       "mean_ci_total_days",
                                                                       "mean_ci_averted_days"))
  
  
  
  hospital_c[ , Infectious.syndrome := "strep defined"]
  
  return(hospital_c)
}
spec.syndrome.global.strep <- function(dt){
  dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_c <- dt %>% group_by(Pathogen,  run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE)) %>%
    group_by(Pathogen) %>%
    summarise(Mean_avert_costing = mean(who_region_cost_A, na.rm=TRUE),
              LOWUI_avert_costing = quantile(who_region_cost_A,0.025, na.rm = TRUE),
              HIGHUI_avert_costing = quantile(who_region_cost_A,0.975, na.rm = TRUE),
              Mean_total_costing = mean(who_region_cost_T, na.rm=TRUE),
              LOWUI_total_costing = quantile(who_region_cost_T,0.025, na.rm = TRUE),
              HIGHUI_total_costing = quantile(who_region_cost_T,0.975, na.rm = TRUE),
              Mean_avert_days = mean(who_region_days_A, na.rm=TRUE),
              LOWUI_avert_days = quantile(who_region_days_A,0.025, na.rm = TRUE),
              HIGHUI_avert_days = quantile(who_region_days_A,0.975, na.rm = TRUE),
              Mean_total_days = mean(who_region_days_T, na.rm=TRUE),
              LOWUI_total_days = quantile(who_region_days_T,0.025, na.rm = TRUE),
              HIGHUI_total_days = quantile(who_region_days_T,0.975, na.rm = TRUE)) %>%
    mutate(across(Mean_avert_costing:HIGHUI_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( mean_ci_total_cost = str_c(Mean_total_costing, " ( ", LOWUI_total_costing, "-",
                                       HIGHUI_total_costing," ) "),
            mean_ci_averted_cost = str_c(Mean_avert_costing, " ( ", LOWUI_avert_costing, "-",
                                         HIGHUI_avert_costing," ) "),
            mean_ci_total_days = str_c(Mean_total_days, " ( ", LOWUI_total_days, "-",
                                       HIGHUI_total_days," ) "),
            mean_ci_averted_days = str_c(Mean_avert_days, " ( ", LOWUI_avert_days, "-",
                                         HIGHUI_avert_days," ) "))%>%
    as.data.table()
  
  hospital_c <- hospital_c[ , -c("Mean_avert_costing",  "LOWUI_avert_costing",
                                 "HIGHUI_avert_costing", "Mean_total_costing" ,
                                 "LOWUI_total_costing" , "HIGHUI_total_costing" ,
                                 "Mean_avert_days",  "LOWUI_avert_days",
                                 "HIGHUI_avert_days", "Mean_total_days" ,
                                 "LOWUI_total_days" , "HIGHUI_total_days" )]
  
  
  hospital_c[ , Infectious.syndrome := "strep defined"]
  return(hospital_c)
}


## Improved_with_elderly
improv.eld <- rbind(improv.7.eld,improv.5.eld)
rm(improv.5.eld)
rm(improv.7.eld)

improv.eld <- improv.eld[Infectious.syndrome=="BSI"|
                           Infectious.syndrome=="CNS infections"|
                           Infectious.syndrome=="Cardiac infections"|
                           Infectious.syndrome=="LRI and thorax infections"]

hospital_c_improv.eld <- spec.syndrome.strep(improv.eld)
hospital_c_improv.eld[ , scenario := "Improved+elderly"]
hospital_c_improv.eld.global <- spec.syndrome.global.strep(improv.eld)
hospital_c_improv.eld.global[ , scenario := "Improved+elderly"]

rm(improv.eld)

## Improved_without_elderly #### 
improv.noeld <- rbind(improv.7,improv.5)
rm(improv.7)
rm(improv.5)

   improv.noeld <- improv.noeld[Infectious.syndrome=="BSI"|
                               Infectious.syndrome=="CNS infections"|
                               Infectious.syndrome=="Cardiac infections"|
                               Infectious.syndrome=="LRI and thorax infections"]

   hospital_c_improv.noeld <- spec.syndrome.strep(improv.noeld)
   hospital_c_improv.noeld[ , scenario := "Improved"]
   hospital_c_improv.noeld.global <- spec.syndrome.global.strep(improv.noeld)
   hospital_c_improv.noeld.global[ , scenario := "Improved"]
   rm(improv.noeld)
   
## Serotype_with_elderly
serotype.eld <- rbind(serotype.27.eld,serotype.58.eld)
rm(serotype.27.eld)
rm(serotype.58.eld)

serotype.eld <- serotype.eld[Infectious.syndrome=="BSI"|
                Infectious.syndrome=="CNS infections"|
                Infectious.syndrome=="Cardiac infections"|
                Infectious.syndrome=="LRI and thorax infections"]

hospital_c_serotype.eld <- spec.syndrome.strep(serotype.eld)
hospital_c_serotype.eld [ , scenario := "Serotype elderly"]
hospital_c_serotype.eld.global <- spec.syndrome.global.strep(serotype.eld)
hospital_c_serotype.eld.global [ , scenario := "Serotype elderly"]
rm(serotype.eld)

## Serotype_without_elderly
serotype.noeld <- rbind(serotype.58,serotype.27)
rm(serotype.58)
rm(serotype.27)

serotype.noeld <- serotype.noeld[Infectious.syndrome=="BSI"|
                               Infectious.syndrome=="CNS infections"|
                               Infectious.syndrome=="Cardiac infections"|
                               Infectious.syndrome=="LRI and thorax infections"]

hospital_c_serotype.noeld <- spec.syndrome.strep(serotype.noeld)
hospital_c_serotype.noeld [ , scenario := "Serotype"]
hospital_c_serotype.noeld.global <- spec.syndrome.global.strep(serotype.noeld)
hospital_c_serotype.noeld.global[ , scenario := "Serotype"]
rm(serotype.noeld)

## Current_coverage_scenario
current <- current[Infectious.syndrome=="BSI"|
                                   Infectious.syndrome=="CNS infections"|
                                   Infectious.syndrome=="Cardiac infections"|
                                   Infectious.syndrome=="LRI and thorax infections"]

hospital_c_current <- spec.syndrome.strep(current)
hospital_c_current [ , scenario := "current"]
hospital_c_current.global <- spec.syndrome.global.strep(current)
hospital_c_current.global[ , scenario := "current"]
rm(current)

strep.region <- list(hospital_c_current,
                     hospital_c_improv.eld,
                     hospital_c_improv.noeld,
                     hospital_c_serotype.eld,
                     hospital_c_serotype.noeld)

keep.cols <- function(x){
  x <- as.data.table(x)
  x <- x[ , c("Pathogen"                 ,  "mean_ci_total_cost_AFRO" ,   "mean_ci_total_cost_EMRO"  , 
              "mean_ci_total_cost_EURO"   , "mean_ci_total_cost_AMRO"  ,  "mean_ci_total_cost_SEARO"  ,
              "mean_ci_total_cost_WPRO"    ,"mean_ci_averted_cost_AFRO" , "mean_ci_averted_cost_EMRO" ,
              "mean_ci_averted_cost_EURO",  "mean_ci_averted_cost_AMRO" , "mean_ci_averted_cost_SEARO",
              "mean_ci_averted_cost_WPRO",  "mean_ci_total_days_AFRO"  ,  "mean_ci_total_days_EMRO"   ,
              "mean_ci_total_days_EURO"    ,"mean_ci_total_days_AMRO"   , "mean_ci_total_days_SEARO"  ,
              "mean_ci_total_days_WPRO"   , "mean_ci_averted_days_AFRO",  "mean_ci_averted_days_EMRO", 
              "mean_ci_averted_days_EURO"  ,"mean_ci_averted_days_AMRO" , "mean_ci_averted_days_SEARO",
              "mean_ci_averted_days_WPRO" , "scenario"  )]
  return(x)
}

strep.region <- lapply(strep.region, keep.cols)
strep.region <- rbindlist(strep.region)

keep.cols.global <- function(x){
  x <- as.data.table(x)
  x <- x[ , c("Pathogen"  ,"mean_ci_total_cost"   , 
              "mean_ci_averted_cost"  , "mean_ci_total_days"   ,  "mean_ci_averted_days", 
               "scenario"  )]
  return(x)
}

strep.global <- list(hospital_c_current.global,
                     hospital_c_improv.eld.global,
                     hospital_c_improv.noeld.global,
                     hospital_c_serotype.eld.global,
                     hospital_c_serotype.noeld.global)

strep.global <- lapply(strep.global, keep.cols.global)
strep.global <- rbindlist(strep.global)

strep <- merge(strep.global,strep.region, by="scenario")

write.csv(strep, file="outputs/END_hospital_costs_outputs_strep.csv")

############# Hib #################################

load("outputs/fulloutput_chunks/resultsHaem28.RData")
Hib_93 <- vaccine_output_dt

load("outputs/fulloutput_chunks/resultsHaem29.RData")
Hib_69 <- vaccine_output_dt
rm(vaccine_output_dt)

Hib <- rbind(Hib_69,Hib_93)

Hib.region <- spec.syndrome.strep(Hib)  ## can use same function just need to then update infectious syndrome spec
Hib.region[ , Infectious.syndrome := "Hib defined"]
Hib.region[ , scenario := "H other"]
Hib.region <- Hib.region[Pathogen!="NA"]

Hib.global <- spec.syndrome.global.strep(Hib)
Hib.global[ , Infectious.syndrome := "Hib defined"]
Hib.global[ , scenario := "H other"]
Hib.global <- Hib.global[Pathogen!="NA"]

Hib <- merge(Hib.region,Hib.global, by="scenario")

write.csv(Hib, file="outputs/END_hospital_costs_outputs_hib.csv")

########### combine the other scenarios and add id NN matcher to WHO template #####

hib <- read.csv("outputs/END_hospital_costs_outputs_hib.csv")
hib <- as.data.table(hib)


## align column names across 
hib <- hib[, c("scenario"          ,         "Pathogen.x"     ,           
 "mean_ci_total_cost"    ,     "mean_ci_averted_cost"   ,    "mean_ci_total_days" ,       
"mean_ci_averted_days"   ,     "mean_ci_total_cost_AFRO"  , 
 "mean_ci_total_cost_EMRO"  ,  "mean_ci_total_cost_EURO" ,   "mean_ci_total_cost_AMRO",   
 "mean_ci_total_cost_SEARO" , "mean_ci_total_cost_WPRO"   , "mean_ci_averted_cost_AFRO" ,
 "mean_ci_averted_cost_EMRO",  "mean_ci_averted_cost_EURO" , "mean_ci_averted_cost_AMRO" ,
 "mean_ci_averted_cost_SEARO", "mean_ci_averted_cost_WPRO" , "mean_ci_total_days_AFRO",   
 "mean_ci_total_days_EMRO" ,   "mean_ci_total_days_EURO" ,   "mean_ci_total_days_AMRO" ,  
 "mean_ci_total_days_SEARO",   "mean_ci_total_days_WPRO"  ,  "mean_ci_averted_days_AFRO" ,
 "mean_ci_averted_days_EMRO" , "mean_ci_averted_days_EURO",  "mean_ci_averted_days_AMRO" ,
 "mean_ci_averted_days_SEARO", "mean_ci_averted_days_WPRO" )]

psuedo <- read.csv("outputs/END_hospital_costs_output_psuedomonas.csv")
psuedo <- as.data.table(psuedo)

psuedo[target.population.x=="6 weeks & elderly age group", scenario := "P 6 weeks"]
psuedo[target.population.x=="All age groups", scenario := "P All"]

psuedo <- psuedo[, c("scenario"          ,         "Pathogen.x"     ,           
 "mean_ci_total_cost"    ,     "mean_ci_averted_cost"   ,    "mean_ci_total_days" ,       
"mean_ci_averted_days"   ,     "mean_ci_total_cost_AFRO"  , 
 "mean_ci_total_cost_EMRO"  ,  "mean_ci_total_cost_EURO" ,   "mean_ci_total_cost_AMRO",   
 "mean_ci_total_cost_SEARO" , "mean_ci_total_cost_WPRO"   , "mean_ci_averted_cost_AFRO" ,
 "mean_ci_averted_cost_EMRO",  "mean_ci_averted_cost_EURO" , "mean_ci_averted_cost_AMRO" ,
 "mean_ci_averted_cost_SEARO", "mean_ci_averted_cost_WPRO" , "mean_ci_total_days_AFRO",   
 "mean_ci_total_days_EMRO" ,   "mean_ci_total_days_EURO" ,   "mean_ci_total_days_AMRO" ,  
 "mean_ci_total_days_SEARO",   "mean_ci_total_days_WPRO"  ,  "mean_ci_averted_days_AFRO" ,
 "mean_ci_averted_days_EMRO" , "mean_ci_averted_days_EURO",  "mean_ci_averted_days_AMRO" ,
 "mean_ci_averted_days_SEARO", "mean_ci_averted_days_WPRO" )]

strep <- read.csv("outputs/END_hospital_costs_outputs_strep.csv")
strep <- as.data.table(strep)         

strep  <- strep[, c("scenario"          ,         "Pathogen.x"     ,           
                     "mean_ci_total_cost"    ,     "mean_ci_averted_cost"   ,    "mean_ci_total_days" ,       
                     "mean_ci_averted_days"   ,     "mean_ci_total_cost_AFRO"  , 
                     "mean_ci_total_cost_EMRO"  ,  "mean_ci_total_cost_EURO" ,   "mean_ci_total_cost_AMRO",   
                     "mean_ci_total_cost_SEARO" , "mean_ci_total_cost_WPRO"   , "mean_ci_averted_cost_AFRO" ,
                     "mean_ci_averted_cost_EMRO",  "mean_ci_averted_cost_EURO" , "mean_ci_averted_cost_AMRO" ,
                     "mean_ci_averted_cost_SEARO", "mean_ci_averted_cost_WPRO" , "mean_ci_total_days_AFRO",   
                     "mean_ci_total_days_EMRO" ,   "mean_ci_total_days_EURO" ,   "mean_ci_total_days_AMRO" ,  
                     "mean_ci_total_days_SEARO",   "mean_ci_total_days_WPRO"  ,  "mean_ci_averted_days_AFRO" ,
                     "mean_ci_averted_days_EMRO" , "mean_ci_averted_days_EURO",  "mean_ci_averted_days_AMRO" ,
                     "mean_ci_averted_days_SEARO", "mean_ci_averted_days_WPRO" )]

other_scenarios <- list(hib,psuedo,strep)
other_scenarios <- rbindlist(other_scenarios)

other_scenarios[ scenario=="H other", Matcher_NN :="HI1"]
other_scenarios[ scenario=="P 6 weeks", Matcher_NN :="P1"]
other_scenarios[ scenario=="P All", Matcher_NN :="P2"]
other_scenarios[ scenario=="Improved+elderly", Matcher_NN := "ST1"]
other_scenarios[ scenario=="current", Matcher_NN := "ST2"]
other_scenarios[ scenario=="Serotype", Matcher_NN := "ST3"]
other_scenarios[ scenario=="Serotype elderly", Matcher_NN := "ST4"]

##### get main data in right format
main_scenarios <- read.csv("outputs/END_hospital_costs_output.csv")
#### totals 
main_scenarios_totals <- read.csv("outputs/END_hospital_costs_output_ALLSYNDROME.csv")
main_scenarios_totals$Infectious.syndrome <- "Total"
main_scenarios <- rbind(main_scenarios, main_scenarios_totals)
rm(main_scenarios_totals)

#### global
main_scenarios_global <- read.csv("outputs/END_hospital_costs_output_global.csv")
main_scenarios_global_total <- read.csv("outputs/END_hospital_costs_output_global_ALLSYNDROME.csv")
main_scenarios_global_total$Infectious.syndrome <- "Total"
main_scenarios_global <- rbind(main_scenarios_global, main_scenarios_global_total)
rm(main_scenarios_global_total)

### merge together
main_scenarios <- merge(main_scenarios, main_scenarios_global, by=c("vaccine_id",
                                                                    "Infectious.syndrome"))

rm(main_scenarios_global)

### match with id template
id_NN <- read.csv("outputs/id_template_outputs.csv") ## these are just for the ones that didn't need adjustment (e.g. not pseudo)
main_scenarios <- merge(main_scenarios, id_NN, by=c("vaccine_id","Infectious.syndrome"))

### bind with the other scenarios
## keep same cols

main_scenarios <- main_scenarios[ , c("Pathogen.x", "mean_ci_total_cost","mean_ci_averted_cost" ,
                    "mean_ci_total_days"  ,       "mean_ci_averted_days"     , 
 "mean_ci_total_cost_AFRO"  ,  "mean_ci_total_cost_EMRO"    ,"mean_ci_total_cost_EURO"   ,
 "mean_ci_total_cost_AMRO"  ,  "mean_ci_total_cost_SEARO"  , "mean_ci_total_cost_WPRO"   ,
 "mean_ci_averted_cost_AFRO","mean_ci_averted_cost_EMRO"  ,"mean_ci_averted_cost_EURO" ,
 "mean_ci_averted_cost_AMRO" , "mean_ci_averted_cost_SEARO", "mean_ci_averted_cost_WPRO" ,
 "mean_ci_total_days_AFRO"  ,  "mean_ci_total_days_EMRO"   , "mean_ci_total_days_EURO"   ,
 "mean_ci_total_days_AMRO"  ,  "mean_ci_total_days_SEARO"  , "mean_ci_total_days_WPRO"   ,
 "mean_ci_averted_days_AFRO",  "mean_ci_averted_days_EMRO" , "mean_ci_averted_days_EURO" ,
 "mean_ci_averted_days_AMRO" , "mean_ci_averted_days_SEARO", "mean_ci_averted_days_WPRO" ,
 "Matcher_NN")]                
other_scenarios <- other_scenarios[ ,-c("scenario")]

hospital <- rbind(main_scenarios, other_scenarios)


###### read in labour productivity
prod.loss <- read.csv("outputs/END_productivity_loss_deaths.csv")
prod.loss.global <- read.csv("outputs/END_productivity_loss_deaths_global.csv")

prod.loss <- merge(prod.loss, prod.loss.global, by=c("vaccine_id","Infectious.syndrome"))

##### add together hib, psuedo and strep - note if had sampling would have to do separately like in hospital costs
#### Hib
prod.loss <- as.data.table(prod.loss)

Hib <- prod.loss[vaccine_id=="Haemophilus influenzae type B_0.93_0.9_5 years_All_6, 10, 14 weeks"   |                                                     
                   vaccine_id=="Haemophilus influenzae type B_0.69_0.9_5 years_All_6, 10, 14 weeks" ]
Hib <- Hib[Infectious.syndrome=="Total"]

Hib <- Hib[, lapply(.SD, sum, na.rm=TRUE),
                   by = c("Pathogen.x",
                          "Infectious.syndrome"
                   ),
                   .SDcols=c("mean_total_AFRO",
                             "mean_total_EMRO"  ,
                             "mean_total_EURO" ,
                             "mean_total_AMRO",
                             "mean_total_SEARO"  ,
                             "mean_total_WPRO"  ,
                             "mean_averted_AFRO" ,
                             "mean_averted_EMRO",     
                             "mean_averted_EURO" ,
                             "mean_averted_AMRO",
                             "mean_averted_SEARO" ,
                             "mean_averted_WPRO" ,
                             "averted",
                             "total")]

Hib[ , Matcher_NN :="HI1"]

psuedo.1 <- prod.loss[vaccine_id=="Pseudomonas aeruginosa_0.7_0.7_5 years_BSI, LRI and thorax infections_6 weeks & elderly age group" ]
psuedo.1 <- psuedo.1[Infectious.syndrome=="LRI and thorax infections"|
                       Infectious.syndrome=="BSI"]
psuedo.1 <- psuedo.1[, lapply(.SD, sum, na.rm=TRUE),
           by = c("Pathogen.x"),
           .SDcols=c("mean_total_AFRO",
                     "mean_total_EMRO"  ,
                     "mean_total_EURO" ,
                     "mean_total_AMRO",
                     "mean_total_SEARO"  ,
                     "mean_total_WPRO"  ,
                     "mean_averted_AFRO" ,
                     "mean_averted_EMRO",     
                     "mean_averted_EURO" ,
                     "mean_averted_AMRO",
                     "mean_averted_SEARO" ,
                     "mean_averted_WPRO" ,
                     "averted",
                     "total")]
psuedo.1[ , Infectious.syndrome:="BSI & LRI and thorax infections"]
psuedo.1[ , Matcher_NN :="P1"]

psuedo.2 <-prod.loss[ vaccine_id== "Pseudomonas aeruginosa_0.7_0.7_5 years_BSI, LRI and thorax infections_All age groups"     ]
psuedo.2 <- psuedo.2[Infectious.syndrome=="LRI and thorax infections"|
                       Infectious.syndrome=="BSI"]
psuedo.2 <- psuedo.2[, lapply(.SD, sum, na.rm=TRUE),
                     by = c("Pathogen.x"),
                     .SDcols=c("mean_total_AFRO",
                                "mean_total_EMRO"  ,
                                "mean_total_EURO" ,
                                "mean_total_AMRO",
                                "mean_total_SEARO"  ,
                                "mean_total_WPRO"  ,
                                "mean_averted_AFRO" ,
                                "mean_averted_EMRO",     
                                "mean_averted_EURO" ,
                                "mean_averted_AMRO",
                                "mean_averted_SEARO" ,
                                "mean_averted_WPRO" ,
                                "averted",
                                "total")]
psuedo.2[ , Infectious.syndrome:="BSI & LRI and thorax infections"]

psuedo.2[ , Matcher_NN :="P2"]


strep.current <-prod.loss[vaccine_id=="Streptococcus pneumoniae_current_current_5 years_BSI, CNS infections, Cardiac infections, LRI_6, 10, 14 weeks"]
  
strep.current <- strep.current[Infectious.syndrome=="BSI"|
                                 Infectious.syndrome=="CNS infections"|
                                 Infectious.syndrome=="Cardiac infections"|
                                 Infectious.syndrome=="LRI and thorax infections"]

strep.current <- strep.current[, lapply(.SD, sum, na.rm=TRUE),
                     by = c("Pathogen.x"),
                     .SDcols=c("mean_total_AFRO",
                               "mean_total_EMRO"  ,
                               "mean_total_EURO" ,
                               "mean_total_AMRO",
                               "mean_total_SEARO"  ,
                               "mean_total_WPRO"  ,
                               "mean_averted_AFRO" ,
                               "mean_averted_EMRO",     
                               "mean_averted_EURO" ,
                               "mean_averted_AMRO",
                               "mean_averted_SEARO" ,
                               "mean_averted_WPRO" ,
                               "averted",
                               "total")]
strep.current[ , Infectious.syndrome:="BSI, CNS infections, Cardiac infections, LRI"]

strep.current[ , Matcher_NN := "ST2"]

strep.improv.eld <- prod.loss[vaccine_id==	"Streptococcus pneumoniae - Improved_0.5_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6 weeks & elderly age group"|
                            vaccine_id=="Streptococcus pneumoniae - Improved_0.7_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6 weeks & elderly age group"]

strep.improv.eld  <- strep.improv.eld[Infectious.syndrome=="BSI"|
                                 Infectious.syndrome=="CNS infections"|
                                 Infectious.syndrome=="Cardiac infections"|
                                 Infectious.syndrome=="LRI and thorax infections"]

strep.improv.eld  <- strep.improv.eld[, lapply(.SD, sum, na.rm=TRUE),
                               by = c("Pathogen.x"),
                               .SDcols=c("mean_total_AFRO",
                                         "mean_total_EMRO"  ,
                                         "mean_total_EURO" ,
                                         "mean_total_AMRO",
                                         "mean_total_SEARO"  ,
                                         "mean_total_WPRO"  ,
                                         "mean_averted_AFRO" ,
                                         "mean_averted_EMRO",     
                                         "mean_averted_EURO" ,
                                         "mean_averted_AMRO",
                                         "mean_averted_SEARO" ,
                                         "mean_averted_WPRO" ,
                                         "averted",
                                         "total")]
strep.improv.eld[ , Infectious.syndrome:="BSI, CNS infections, Cardiac infections, LRI"]

strep.improv.eld[ , Matcher_NN := "ST1"]

strep.serotype <- prod.loss[vaccine_id==	"Streptococcus pneumoniae_0.58_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6, 10, 14 weeks"  |                    
                              vaccine_id=="Streptococcus pneumoniae_0.27_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6, 10, 14 weeks"]                      


strep.serotype  <- strep.serotype[Infectious.syndrome=="BSI"|
                                        Infectious.syndrome=="CNS infections"|
                                        Infectious.syndrome=="Cardiac infections"|
                                        Infectious.syndrome=="LRI and thorax infections"]

strep.serotype  <- strep.serotype[, lapply(.SD, sum, na.rm=TRUE),
                                      by = c("Pathogen.x"),
                                      .SDcols=c("mean_total_AFRO",
                                                "mean_total_EMRO"  ,
                                                "mean_total_EURO" ,
                                                "mean_total_AMRO",
                                                "mean_total_SEARO"  ,
                                                "mean_total_WPRO"  ,
                                                "mean_averted_AFRO" ,
                                                "mean_averted_EMRO",     
                                                "mean_averted_EURO" ,
                                                "mean_averted_AMRO",
                                                "mean_averted_SEARO" ,
                                                "mean_averted_WPRO" ,
                                                "averted",
                                                "total")]
strep.serotype[ , Infectious.syndrome:="BSI, CNS infections, Cardiac infections, LRI"]

strep.serotype[, Matcher_NN := "ST3"]

strep.serotype.eld <- prod.loss[vaccine_id==	"Streptococcus pneumoniae_0.58_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6, 10, 14 weeks & elderly age group" | 
                                vaccine_id==	"Streptococcus pneumoniae_0.27_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6, 10, 14 weeks & elderly age group"  ]


strep.serotype.eld  <- strep.serotype.eld[Infectious.syndrome=="BSI"|
                                    Infectious.syndrome=="CNS infections"|
                                    Infectious.syndrome=="Cardiac infections"|
                                    Infectious.syndrome=="LRI and thorax infections"]

strep.serotype.eld  <- strep.serotype.eld[, lapply(.SD, sum, na.rm=TRUE),
                                  by = c("Pathogen.x"),
                                  .SDcols=c("mean_total_AFRO",
                                            "mean_total_EMRO"  ,
                                            "mean_total_EURO" ,
                                            "mean_total_AMRO",
                                            "mean_total_SEARO"  ,
                                            "mean_total_WPRO"  ,
                                            "mean_averted_AFRO" ,
                                            "mean_averted_EMRO",     
                                            "mean_averted_EURO" ,
                                            "mean_averted_AMRO",
                                            "mean_averted_SEARO" ,
                                            "mean_averted_WPRO" ,
                                            "averted",
                                            "total")]
strep.serotype.eld[ , Infectious.syndrome:="BSI, CNS infections, Cardiac infections, LRI"]


strep.serotype.eld[ , Matcher_NN := "ST4"]

other.prod <- list(Hib,psuedo.1,psuedo.2,strep.current,strep.improv.eld,
                   strep.serotype, strep.serotype.eld)

other.prod <- rbindlist(other.prod, use.names=TRUE)

#### create big productivity dataset
### match with id template
id_NN <- read.csv("outputs/id_template_outputs.csv")
main_scenarios_prod <- merge(prod.loss, id_NN, by=c("vaccine_id","Infectious.syndrome"))

main_scenarios_prod <- main_scenarios_prod[ ,c("Pathogen.x"   ,
                                               "Infectious.syndrome",
                                               "mean_total_AFRO",
                                               "mean_total_EMRO" ,
                                               "mean_total_EURO" ,  
                                               "mean_total_AMRO"  ,
                                               "mean_total_SEARO"  ,
                                               "mean_total_WPRO"   ,
                                               "mean_averted_AFRO" ,
                                               "mean_averted_EMRO" ,
                                               "mean_averted_EURO" ,
                                               "mean_averted_AMRO" ,
                                               "mean_averted_SEARO",
                                               "mean_averted_WPRO" ,
                                               "averted"           , 
                                               "total"         ,
                                               "Matcher_NN")]


#### combine main and other scenarios
l <- list(main_scenarios_prod, other.prod)
productivity <- rbindlist(l, use.names = TRUE)


##### combine into one big dataset with the ones needed
final_table <- merge(hospital,productivity, by="Matcher_NN")
final_table <- as.data.table(final_table)

#### read in template
template_outputs_GHO <- read_excel("outputs/template_outputs_GHO.xlsx")


## get correct column order 
colorder <- colnames(template_outputs_GHO)
colorder

final_table <- final_table[ ,c("Matcher_NN",               
                            "mean_ci_total_cost_AFRO"     ,          "mean_ci_total_cost_EMRO"    ,           
                             "mean_ci_total_cost_EURO"     ,          "mean_ci_total_cost_SEARO"   ,          
                             "mean_ci_total_cost_AMRO"      ,         "mean_ci_total_cost_WPRO"     ,         
                             "mean_ci_total_cost"            ,        "mean_ci_averted_cost_AFRO"    ,        
                             "mean_ci_averted_cost_EMRO"      ,       "mean_ci_averted_cost_EURO"     ,       
                            "mean_ci_averted_cost_SEARO"       ,     "mean_ci_averted_cost_AMRO"       ,     
                            "mean_ci_averted_cost_WPRO"         ,    "mean_ci_averted_cost" ,                
                            "mean_total_AFRO"                    ,   "mean_total_EMRO"       ,               
                             "mean_total_EURO"        ,               "mean_total_SEARO"      ,               
                            "mean_total_AMRO"          ,             "mean_total_WPRO"         ,             
                             "total"               ,             "mean_averted_AFRO"       ,             
                             "mean_averted_EMRO"         ,            "mean_averted_EURO"        ,            
                             "mean_averted_SEARO"         ,           "mean_averted_AMRO"         ,           
                             "mean_averted_WPRO"           ,          "averted"                ,         
                             "mean_ci_total_days_AFRO"      ,         "mean_ci_total_days_EMRO"    ,          
                             "mean_ci_total_days_EURO"       ,        "mean_ci_total_days_SEARO"     ,        
                             "mean_ci_total_days_AMRO"        ,       "mean_ci_total_days_WPRO"       ,       
                             "mean_ci_total_days"       ,             "mean_ci_averted_days_AFRO"      ,      
                            "mean_ci_averted_days_EMRO"  ,           "mean_ci_averted_days_EURO"        ,    
                        "mean_ci_averted_days_SEARO"      ,      "mean_ci_averted_days_AMRO"            ,
                             "mean_ci_averted_days_WPRO"   ,          "mean_ci_averted_days" )]


write.csv(final_table, file="outputs/final_table_GHO.csv")
