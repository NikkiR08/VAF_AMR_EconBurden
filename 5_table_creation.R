####### GETTING DATA TABLES FROM RESULTS ####################################

#### loading packages
library(tidyr)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

##### read in data

myfiles = list.files(path="C:/Users/nicho/OneDrive/Documents/VAF_AMR_EconBurden/outputs/fulloutput_chunks/",
                     pattern="*.RData", full.names = TRUE)

### split into the ones that need adjusting and those that don't
### compared to WHO tables - don't need to do pseudo or strep syndrome specifications
### as using all modelled syndromes
### do need to add hib and strep scenarios that = 1 scenario 

#### there is probaby a more efficient way of doing but trying to adapt into older code for now
#### note that the numbers used in the functions would need updating to adapt for different scenarios
#### could try to get 1 function where you just specify the group_by variables then just run that
### for each

adj <- c("C:/Users/nicho/OneDrive/Documents/VAF_AMR_EconBurden/outputs/fulloutput_chunks/resultsStre19.RData",
         "C:/Users/nicho/OneDrive/Documents/VAF_AMR_EconBurden/outputs/fulloutput_chunks/resultsStre20.RData",
        "C:/Users/nicho/OneDrive/Documents/VAF_AMR_EconBurden/outputs/fulloutput_chunks/resultsStre23.RData",
         "C:/Users/nicho/OneDrive/Documents/VAF_AMR_EconBurden/outputs/fulloutput_chunks/resultsStre24.RData",
       "C:/Users/nicho/OneDrive/Documents/VAF_AMR_EconBurden/outputs/fulloutput_chunks/resultsStre25.RData",
         "C:/Users/nicho/OneDrive/Documents/VAF_AMR_EconBurden/outputs/fulloutput_chunks/resultsStre35.RData",
         "C:/Users/nicho/OneDrive/Documents/VAF_AMR_EconBurden/outputs/fulloutput_chunks/resultsStre36.RData",
       "C:/Users/nicho/OneDrive/Documents/VAF_AMR_EconBurden/outputs/fulloutput_chunks/resultsStre37.RData",
       "C:/Users/nicho/OneDrive/Documents/VAF_AMR_EconBurden/outputs/fulloutput_chunks/resultsHaem28.RData",
       "C:/Users/nicho/OneDrive/Documents/VAF_AMR_EconBurden/outputs/fulloutput_chunks/resultsHaem29.RData")

nonadj <- setdiff(myfiles,adj) ## find ones that don't need adjusting
       

#### MEDIAN & IQR INSTEAD #################

mediqr1 <- function(vaccine_output_dt){
  x <- vaccine_output_dt%>% group_by(WHO.Region, Pathogen, Infectious.syndrome,
                                                vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE))  %>%
  group_by(WHO.Region, Pathogen, Infectious.syndrome, vaccine_id) %>%
    summarise(median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE)) %>%
    mutate(across(median_avert_costing:HIGHIQR_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( median_iqr_total_cost = str_c(median_total_costing, " ( ", LOWIQR_total_costing, "-",
                                       HIGHIQR_total_costing," ) "),
            median_iqr_averted_cost = str_c(median_avert_costing, " ( ", LOWIQR_avert_costing, "-",
                                         HIGHIQR_avert_costing," ) "),
            median_iqr_total_days = str_c(median_total_days, " ( ", LOWIQR_total_days, "-",
                                       HIGHIQR_total_days," ) "),
            median_iqr_averted_days = str_c(median_avert_days, " ( ", LOWIQR_avert_days, "-",
                                         HIGHIQR_avert_days," ) ")) %>%
    select(!c("median_avert_costing",  "LOWIQR_avert_costing",
                                 "HIGHIQR_avert_costing", "median_total_costing" ,
                                 "LOWIQR_total_costing" , "HIGHIQR_total_costing" ,
                                 "median_avert_days",  "LOWIQR_avert_days",
                                 "HIGHIQR_avert_days", "median_total_days" ,
                                 "LOWIQR_total_days" , "HIGHIQR_total_days")) %>%
    
  as.data.table()
  return(x)
}
########### nonadj ################
hospital_cost_l <- list()
for (i in 1:length(nonadj)){
  load(nonadj[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_cost_l[[i]] <- mediqr1(vaccine_output_dt)
}

########### adj ################
hospital_cost_l_adj <- list()

##### loading the datasets we need
### adding dataset ref by hand re. numbers so would need checking if updated 
### scenarios !!!
load("outputs/fulloutput_chunks/resultsStre19.RData")
serotype.58 <- vaccine_output_dt
load("outputs/fulloutput_chunks/resultsStre34.RData")
serotype <- rbind(vaccine_output_dt,serotype.58)
rm(serotype.58)
## ?? coming up with error but still creates the right data table ??
## updating vaccine id to match to enable use of function
unique(serotype$vaccine_id)
serotype[ , vaccine_id := "Streptococcus pneumoniae_both_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6, 10, 14 weeks"]
hospital_cost_l_adj[[1]] <- mediqr1(serotype)


load("outputs/fulloutput_chunks/resultsStre23.RData")
serotype.58.eld <- vaccine_output_dt
load("outputs/fulloutput_chunks/resultsStre36.RData")
serotype.eld <- rbind(vaccine_output_dt, serotype.58.eld)
rm(serotype.58.eld)
unique(serotype.eld$vaccine_id)
serotype.eld <- serotype.eld[!is.na(vaccine_id)] ## remove those not associated with any vaccine scenarios
serotype.eld[ , vaccine_id := "Streptococcus pneumoniae_both_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6, 10, 14 weeks & elderly age group"]
hospital_cost_l_adj[[2]] <- mediqr1(serotype.eld)

load("outputs/fulloutput_chunks/resultsStre20.RData")
improv.7 <- vaccine_output_dt
load("outputs/fulloutput_chunks/resultsStre35.RData")
improv <- rbind(vaccine_output_dt, improv.7)
rm(improv.7)
unique(improv$vaccine_id)
improv <- improv[!is.na(vaccine_id)] ## remove those not associated with any vaccine scenarios
improv[ , vaccine_id := "Streptococcus pneumoniae - Improved_both_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6 weeks"]
hospital_cost_l_adj[[3]] <- mediqr1(improv)

load("outputs/fulloutput_chunks/resultsStre37.RData")
improv.5.eld <- vaccine_output_dt
load("outputs/fulloutput_chunks/resultsStre24.RData")
improv.eld <- rbind(vaccine_output_dt,improv.5.eld)
rm(improv.5.eld)
unique(improv.eld$vaccine_id)
improv.eld <- improv.eld[!is.na(vaccine_id)] ## remove those not associated with any vaccine scenarios
improv.eld[ , vaccine_id := "Streptococcus pneumoniae - Improved_both_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6 weeks & elderly age group"]
hospital_cost_l_adj[[4]] <- mediqr1(improv.eld)


### HiB
load("outputs/fulloutput_chunks/resultsHaem28.RData")
Hib_93 <- vaccine_output_dt
load("outputs/fulloutput_chunks/resultsHaem29.RData")
Hib <- rbind(vaccine_output_dt,Hib_93)
rm(Hib_93)
unique(Hib$vaccine_id)
Hib <- Hib[!is.na(vaccine_id)] ## remove those not associated with any vaccine scenarios
Hib[ , vaccine_id := "Haemophilus influenzae type B_0.93_0.9_5 years_All_6, 10, 14 weeks"]
hospital_cost_l_adj[[5]] <- mediqr1(Hib)


hospital_c <- rbindlist(hospital_cost_l)
hospital_cA <- rbindlist(hospital_cost_l_adj)
hospital_c <- rbind(hospital_c,hospital_cA)

hospital_c <- dcast(hospital_c, Pathogen + Infectious.syndrome +
                      vaccine_id ~ WHO.Region, value.var = c( "median_iqr_total_cost" 
                                                              ,"median_iqr_averted_cost"
                                                              ,"median_iqr_total_days"  
                                                              ,"median_iqr_averted_days"))

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]


write.csv(hospital_c, file="outputs/END_hospital_costs_output_median.csv")


######## REGION + ALL LEVELS ############
mediqr1.all <- function(vaccine_output_dt){
  x <- vaccine_output_dt%>% group_by(WHO.Region, Pathogen, class, Infectious.syndrome,
                                     vaccine_id, run)  %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE))  %>%
    group_by(WHO.Region, Pathogen, class, Infectious.syndrome,
             vaccine_id)  %>%
    summarise(median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE)) %>%
    mutate(across(median_avert_costing:HIGHIQR_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( median_iqr_total_cost = str_c(median_total_costing, " ( ", LOWIQR_total_costing, "-",
                                          HIGHIQR_total_costing," ) "),
            median_iqr_averted_cost = str_c(median_avert_costing, " ( ", LOWIQR_avert_costing, "-",
                                            HIGHIQR_avert_costing," ) "),
            median_iqr_total_days = str_c(median_total_days, " ( ", LOWIQR_total_days, "-",
                                          HIGHIQR_total_days," ) "),
            median_iqr_averted_days = str_c(median_avert_days, " ( ", LOWIQR_avert_days, "-",
                                            HIGHIQR_avert_days," ) ")) %>%
    select(!c("median_avert_costing",  "LOWIQR_avert_costing",
              "HIGHIQR_avert_costing", "median_total_costing" ,
              "LOWIQR_total_costing" , "HIGHIQR_total_costing" ,
              "median_avert_days",  "LOWIQR_avert_days",
              "HIGHIQR_avert_days", "median_total_days" ,
              "LOWIQR_total_days" , "HIGHIQR_total_days")) %>%
    
    as.data.table()
  return(x)
}
########### nonadj ################
hospital_cost_l <- list()
for (i in 1:length(nonadj)){
  load(nonadj[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_cost_l[[i]] <- mediqr1.all(vaccine_output_dt)
}

########### adj ################
hospital_cost_l_adj <- list()

hospital_cost_l_adj[[1]] <- mediqr1.all(serotype)
hospital_cost_l_adj[[2]] <- mediqr1.all(serotype.eld)
hospital_cost_l_adj[[3]] <- mediqr1.all(improv)
hospital_cost_l_adj[[4]] <- mediqr1.all(improv.eld)
hospital_cost_l_adj[[5]] <- mediqr1.all(Hib)


hospital_c <- rbindlist(hospital_cost_l)
hospital_cA <- rbindlist(hospital_cost_l_adj)
hospital_c <- rbind(hospital_c,hospital_cA)
hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]

save(hospital_c,file="outputs/hospitalC_region_averted.RData")


######## REGION + CLASS ############

mediqr.reg.class <- function(vaccine_output_dt){
  x <- vaccine_output_dt%>% group_by(WHO.Region, class,
                                     vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE))  %>%
    group_by(WHO.Region, class,
             vaccine_id) %>%
    summarise(median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE)) %>%
    mutate(across(median_avert_costing:HIGHIQR_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( median_iqr_total_cost = str_c(median_total_costing, " ( ", LOWIQR_total_costing, "-",
                                          HIGHIQR_total_costing," ) "),
            median_iqr_averted_cost = str_c(median_avert_costing, " ( ", LOWIQR_avert_costing, "-",
                                            HIGHIQR_avert_costing," ) "),
            median_iqr_total_days = str_c(median_total_days, " ( ", LOWIQR_total_days, "-",
                                          HIGHIQR_total_days," ) "),
            median_iqr_averted_days = str_c(median_avert_days, " ( ", LOWIQR_avert_days, "-",
                                            HIGHIQR_avert_days," ) ")) %>%
    select(!c("median_avert_costing",  "LOWIQR_avert_costing",
              "HIGHIQR_avert_costing", "median_total_costing" ,
              "LOWIQR_total_costing" , "HIGHIQR_total_costing" ,
              "median_avert_days",  "LOWIQR_avert_days",
              "HIGHIQR_avert_days", "median_total_days" ,
              "LOWIQR_total_days" , "HIGHIQR_total_days")) %>%
    
    as.data.table()
  return(x)
}
########### nonadj ################
hospital_cost_l <- list()
for (i in 1:length(nonadj)){
  load(nonadj[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_cost_l[[i]] <- mediqr.reg.class(vaccine_output_dt)
}

########### adj ################
hospital_cost_l_adj <- list()

hospital_cost_l_adj[[1]] <- mediqr.reg.class(serotype)
hospital_cost_l_adj[[2]] <- mediqr.reg.class(serotype.eld)
hospital_cost_l_adj[[3]] <- mediqr.reg.class(improv)
hospital_cost_l_adj[[4]] <- mediqr.reg.class(improv.eld)
hospital_cost_l_adj[[5]] <- mediqr.reg.class(Hib)


hospital_c <- rbindlist(hospital_cost_l)
hospital_cA <- rbindlist(hospital_cost_l_adj)
hospital_c <- rbind(hospital_c,hospital_cA)

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]

save(hospital_c,file="outputs/hospitalC_region_averted_class.RData")


######## REGION + PATHOGEN ############

mediqr.reg.path <- function(vaccine_output_dt){
  x <- vaccine_output_dt%>% group_by(WHO.Region, Pathogen, 
                             vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE))  %>%
    group_by(WHO.Region, Pathogen, 
             vaccine_id) %>%
    summarise(median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE)) %>%
    mutate(across(median_avert_costing:HIGHIQR_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( median_iqr_total_cost = str_c(median_total_costing, " ( ", LOWIQR_total_costing, "-",
                                          HIGHIQR_total_costing," ) "),
            median_iqr_averted_cost = str_c(median_avert_costing, " ( ", LOWIQR_avert_costing, "-",
                                            HIGHIQR_avert_costing," ) "),
            median_iqr_total_days = str_c(median_total_days, " ( ", LOWIQR_total_days, "-",
                                          HIGHIQR_total_days," ) "),
            median_iqr_averted_days = str_c(median_avert_days, " ( ", LOWIQR_avert_days, "-",
                                            HIGHIQR_avert_days," ) ")) %>%
    select(!c("median_avert_costing",  "LOWIQR_avert_costing",
              "HIGHIQR_avert_costing", "median_total_costing" ,
              "LOWIQR_total_costing" , "HIGHIQR_total_costing" ,
              "median_avert_days",  "LOWIQR_avert_days",
              "HIGHIQR_avert_days", "median_total_days" ,
              "LOWIQR_total_days" , "HIGHIQR_total_days")) %>%
    
    as.data.table()
  return(x)
}
########### nonadj ################
hospital_cost_l <- list()
for (i in 1:length(nonadj)){
  load(nonadj[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_cost_l[[i]] <- mediqr.reg.path(vaccine_output_dt)
}

########### adj ################
hospital_cost_l_adj <- list()

hospital_cost_l_adj[[1]] <- mediqr.reg.path(serotype)
hospital_cost_l_adj[[2]] <- mediqr.reg.path(serotype.eld)
hospital_cost_l_adj[[3]] <- mediqr.reg.path(improv)
hospital_cost_l_adj[[4]] <- mediqr.reg.path(improv.eld)
hospital_cost_l_adj[[5]] <- mediqr.reg.path(Hib)


hospital_c <- rbindlist(hospital_cost_l)
hospital_cA <- rbindlist(hospital_cost_l_adj)
hospital_c <- rbind(hospital_c,hospital_cA)

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]

save(hospital_c,file="outputs/hospitalC_region_averted_pathogen.RData")


######## REGION + SYNDROME ############
mediqr.reg.syn <- function(vaccine_output_dt){
  x <- vaccine_output_dt%>% group_by(WHO.Region, Infectious.syndrome,
                                     vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE))  %>%
    group_by(WHO.Region, Infectious.syndrome,
             vaccine_id) %>%
    summarise(median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE)) %>%
    mutate(across(median_avert_costing:HIGHIQR_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( median_iqr_total_cost = str_c(median_total_costing, " ( ", LOWIQR_total_costing, "-",
                                          HIGHIQR_total_costing," ) "),
            median_iqr_averted_cost = str_c(median_avert_costing, " ( ", LOWIQR_avert_costing, "-",
                                            HIGHIQR_avert_costing," ) "),
            median_iqr_total_days = str_c(median_total_days, " ( ", LOWIQR_total_days, "-",
                                          HIGHIQR_total_days," ) "),
            median_iqr_averted_days = str_c(median_avert_days, " ( ", LOWIQR_avert_days, "-",
                                            HIGHIQR_avert_days," ) ")) %>%
    select(!c("median_avert_costing",  "LOWIQR_avert_costing",
              "HIGHIQR_avert_costing", "median_total_costing" ,
              "LOWIQR_total_costing" , "HIGHIQR_total_costing" ,
              "median_avert_days",  "LOWIQR_avert_days",
              "HIGHIQR_avert_days", "median_total_days" ,
              "LOWIQR_total_days" , "HIGHIQR_total_days")) %>%
    
    as.data.table()
  return(x)
}
########### nonadj ################
hospital_cost_l <- list()
for (i in 1:length(nonadj)){
  load(nonadj[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_cost_l[[i]] <- mediqr.reg.syn(vaccine_output_dt)
}

########### adj ################
hospital_cost_l_adj <- list()

hospital_cost_l_adj[[1]] <- mediqr.reg.syn(serotype)
hospital_cost_l_adj[[2]] <- mediqr.reg.syn(serotype.eld)
hospital_cost_l_adj[[3]] <- mediqr.reg.syn(improv)
hospital_cost_l_adj[[4]] <- mediqr.reg.syn(improv.eld)
hospital_cost_l_adj[[5]] <- mediqr.reg.syn(Hib)


hospital_c <- rbindlist(hospital_cost_l)
hospital_cA <- rbindlist(hospital_cost_l_adj)
hospital_c <- rbind(hospital_c,hospital_cA)

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]

save(hospital_c,file="outputs/hospitalC_region_averted_syndrome.RData")


######## GLOBAL + ALL LEVELS ############
mediqr.glob.all <- function(vaccine_output_dt){
  x <- vaccine_output_dt%>% group_by(Pathogen, class, Infectious.syndrome,
                                     vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE))  %>%
    group_by(Pathogen, class, Infectious.syndrome,
                     vaccine_id) %>%
    summarise(median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE)) %>%
    mutate(across(median_avert_costing:HIGHIQR_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( median_iqr_total_cost = str_c(median_total_costing, " ( ", LOWIQR_total_costing, "-",
                                          HIGHIQR_total_costing," ) "),
            median_iqr_averted_cost = str_c(median_avert_costing, " ( ", LOWIQR_avert_costing, "-",
                                            HIGHIQR_avert_costing," ) "),
            median_iqr_total_days = str_c(median_total_days, " ( ", LOWIQR_total_days, "-",
                                          HIGHIQR_total_days," ) "),
            median_iqr_averted_days = str_c(median_avert_days, " ( ", LOWIQR_avert_days, "-",
                                            HIGHIQR_avert_days," ) ")) %>%
    select(!c("median_avert_costing",  "LOWIQR_avert_costing",
              "HIGHIQR_avert_costing", "median_total_costing" ,
              "LOWIQR_total_costing" , "HIGHIQR_total_costing" ,
              "median_avert_days",  "LOWIQR_avert_days",
              "HIGHIQR_avert_days", "median_total_days" ,
              "LOWIQR_total_days" , "HIGHIQR_total_days")) %>%
    
    as.data.table()
  return(x)
}
########### nonadj ################
hospital_cost_l <- list()
for (i in 1:length(nonadj)){
  load(nonadj[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_cost_l[[i]] <- mediqr.glob.all(vaccine_output_dt)
}

########### adj ################
hospital_cost_l_adj <- list()

hospital_cost_l_adj[[1]] <- mediqr.glob.all(serotype)
hospital_cost_l_adj[[2]] <- mediqr.glob.all(serotype.eld)
hospital_cost_l_adj[[3]] <- mediqr.glob.all(improv)
hospital_cost_l_adj[[4]] <- mediqr.glob.all(improv.eld)
hospital_cost_l_adj[[5]] <- mediqr.glob.all(Hib)


hospital_c <- rbindlist(hospital_cost_l)
hospital_cA <- rbindlist(hospital_cost_l_adj)
hospital_c <- rbind(hospital_c,hospital_cA)

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]
save(hospital_c,file="outputs/hospitalC_global_averted.RData")


######## GLOBAL + CLASS ############

mediqr.glob.class <- function(vaccine_output_dt){
  x <- vaccine_output_dt%>% group_by(class, 
                                             vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE))  %>%
    group_by(class, 
             vaccine_id) %>%
    summarise(median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE)) %>%
    mutate(across(median_avert_costing:HIGHIQR_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( median_iqr_total_cost = str_c(median_total_costing, " ( ", LOWIQR_total_costing, "-",
                                          HIGHIQR_total_costing," ) "),
            median_iqr_averted_cost = str_c(median_avert_costing, " ( ", LOWIQR_avert_costing, "-",
                                            HIGHIQR_avert_costing," ) "),
            median_iqr_total_days = str_c(median_total_days, " ( ", LOWIQR_total_days, "-",
                                          HIGHIQR_total_days," ) "),
            median_iqr_averted_days = str_c(median_avert_days, " ( ", LOWIQR_avert_days, "-",
                                            HIGHIQR_avert_days," ) ")) %>%
    select(!c("median_avert_costing",  "LOWIQR_avert_costing",
              "HIGHIQR_avert_costing", "median_total_costing" ,
              "LOWIQR_total_costing" , "HIGHIQR_total_costing" ,
              "median_avert_days",  "LOWIQR_avert_days",
              "HIGHIQR_avert_days", "median_total_days" ,
              "LOWIQR_total_days" , "HIGHIQR_total_days")) %>%
    
    as.data.table()
  return(x)
}
########### nonadj ################
hospital_cost_l <- list()
for (i in 1:length(nonadj)){
  load(nonadj[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_cost_l[[i]] <- mediqr.glob.class(vaccine_output_dt)
}

########### adj ################
hospital_cost_l_adj <- list()

hospital_cost_l_adj[[1]] <- mediqr.glob.class(serotype)
hospital_cost_l_adj[[2]] <- mediqr.glob.class(serotype.eld)
hospital_cost_l_adj[[3]] <- mediqr.glob.class(improv)
hospital_cost_l_adj[[4]] <- mediqr.glob.class(improv.eld)
hospital_cost_l_adj[[5]] <- mediqr.glob.class(Hib)


hospital_c <- rbindlist(hospital_cost_l)
hospital_cA <- rbindlist(hospital_cost_l_adj)
hospital_c <- rbind(hospital_c,hospital_cA)

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]

save(hospital_c,file="outputs/hospitalC_global_averted_class.RData")


######## GLOBAL + PATHOGEN ############

mediqr.glob.path <- function(vaccine_output_dt){
  x <- vaccine_output_dt%>% group_by(Pathogen, 
                                     vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE))  %>%
    group_by(Pathogen, 
             vaccine_id) %>%
    summarise(median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE)) %>%
    mutate(across(median_avert_costing:HIGHIQR_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( median_iqr_total_cost = str_c(median_total_costing, " ( ", LOWIQR_total_costing, "-",
                                          HIGHIQR_total_costing," ) "),
            median_iqr_averted_cost = str_c(median_avert_costing, " ( ", LOWIQR_avert_costing, "-",
                                            HIGHIQR_avert_costing," ) "),
            median_iqr_total_days = str_c(median_total_days, " ( ", LOWIQR_total_days, "-",
                                          HIGHIQR_total_days," ) "),
            median_iqr_averted_days = str_c(median_avert_days, " ( ", LOWIQR_avert_days, "-",
                                            HIGHIQR_avert_days," ) ")) %>%
    select(!c("median_avert_costing",  "LOWIQR_avert_costing",
              "HIGHIQR_avert_costing", "median_total_costing" ,
              "LOWIQR_total_costing" , "HIGHIQR_total_costing" ,
              "median_avert_days",  "LOWIQR_avert_days",
              "HIGHIQR_avert_days", "median_total_days" ,
              "LOWIQR_total_days" , "HIGHIQR_total_days")) %>%
    
    as.data.table()
  return(x)
}
########### nonadj ################
hospital_cost_l <- list()
for (i in 1:length(nonadj)){
  load(nonadj[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_cost_l[[i]] <- mediqr.glob.path(vaccine_output_dt)
}

########### adj ################
hospital_cost_l_adj <- list()

hospital_cost_l_adj[[1]] <- mediqr.glob.path(serotype)
hospital_cost_l_adj[[2]] <- mediqr.glob.path(serotype.eld)
hospital_cost_l_adj[[3]] <- mediqr.glob.path(improv)
hospital_cost_l_adj[[4]] <- mediqr.glob.path(improv.eld)
hospital_cost_l_adj[[5]] <- mediqr.glob.path(Hib)


hospital_c <- rbindlist(hospital_cost_l)
hospital_cA <- rbindlist(hospital_cost_l_adj)
hospital_c <- rbind(hospital_c,hospital_cA)

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]

save(hospital_c,file="outputs/hospitalC_global_averted_pathogen.RData")

######## GLOBAL + SYNDROME ############

mediqr.glob.syn <- function(vaccine_output_dt){
  x <- vaccine_output_dt%>% group_by( Infectious.syndrome,
                                      vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE))  %>%
    group_by( Infectious.syndrome,
              vaccine_id) %>%
    summarise(median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE)) %>%
    mutate(across(median_avert_costing:HIGHIQR_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( median_iqr_total_cost = str_c(median_total_costing, " ( ", LOWIQR_total_costing, "-",
                                          HIGHIQR_total_costing," ) "),
            median_iqr_averted_cost = str_c(median_avert_costing, " ( ", LOWIQR_avert_costing, "-",
                                            HIGHIQR_avert_costing," ) "),
            median_iqr_total_days = str_c(median_total_days, " ( ", LOWIQR_total_days, "-",
                                          HIGHIQR_total_days," ) "),
            median_iqr_averted_days = str_c(median_avert_days, " ( ", LOWIQR_avert_days, "-",
                                            HIGHIQR_avert_days," ) ")) %>%
    select(!c("median_avert_costing",  "LOWIQR_avert_costing",
              "HIGHIQR_avert_costing", "median_total_costing" ,
              "LOWIQR_total_costing" , "HIGHIQR_total_costing" ,
              "median_avert_days",  "LOWIQR_avert_days",
              "HIGHIQR_avert_days", "median_total_days" ,
              "LOWIQR_total_days" , "HIGHIQR_total_days")) %>%
    
    as.data.table()
  return(x)
}
########### nonadj ################
hospital_cost_l <- list()
for (i in 1:length(nonadj)){
  load(nonadj[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_cost_l[[i]] <- mediqr.glob.syn(vaccine_output_dt)
}

########### adj ################
hospital_cost_l_adj <- list()

hospital_cost_l_adj[[1]] <- mediqr.glob.syn(serotype)
hospital_cost_l_adj[[2]] <- mediqr.glob.syn(serotype.eld)
hospital_cost_l_adj[[3]] <- mediqr.glob.syn(improv)
hospital_cost_l_adj[[4]] <- mediqr.glob.syn(improv.eld)
hospital_cost_l_adj[[5]] <- mediqr.glob.syn(Hib)


hospital_c <- rbindlist(hospital_cost_l)
hospital_cA <- rbindlist(hospital_cost_l_adj)
hospital_c <- rbind(hospital_c,hospital_cA)

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]

save(hospital_c,file="outputs/hospitalC_global_averted_syndrome.RData")


######## GLOBAL + PATHOGEN + SYNDROME ############
mediqr.glob.syn.path <- function(vaccine_output_dt){
  x <- vaccine_output_dt%>% group_by(Pathogen,Infectious.syndrome,
                                     vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_days_T = sum(days_cases, na.rm=TRUE))  %>%
    group_by(Pathogen,Infectious.syndrome,
             vaccine_id) %>%
    summarise(median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE)) %>%
    mutate(across(median_avert_costing:HIGHIQR_total_days,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate( median_iqr_total_cost = str_c(median_total_costing, " ( ", LOWIQR_total_costing, "-",
                                          HIGHIQR_total_costing," ) "),
            median_iqr_averted_cost = str_c(median_avert_costing, " ( ", LOWIQR_avert_costing, "-",
                                            HIGHIQR_avert_costing," ) "),
            median_iqr_total_days = str_c(median_total_days, " ( ", LOWIQR_total_days, "-",
                                          HIGHIQR_total_days," ) "),
            median_iqr_averted_days = str_c(median_avert_days, " ( ", LOWIQR_avert_days, "-",
                                            HIGHIQR_avert_days," ) ")) %>%
    select(!c("median_avert_costing",  "LOWIQR_avert_costing",
              "HIGHIQR_avert_costing", "median_total_costing" ,
              "LOWIQR_total_costing" , "HIGHIQR_total_costing" ,
              "median_avert_days",  "LOWIQR_avert_days",
              "HIGHIQR_avert_days", "median_total_days" ,
              "LOWIQR_total_days" , "HIGHIQR_total_days")) %>%
    
    as.data.table()
  return(x)
}
########### nonadj ################
hospital_cost_l <- list()
for (i in 1:length(nonadj)){
  load(nonadj[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_cost_l[[i]] <- mediqr.glob.syn.path(vaccine_output_dt)
}

########### adj ################
hospital_cost_l_adj <- list()

hospital_cost_l_adj[[1]] <- mediqr.glob.syn.path(serotype)
hospital_cost_l_adj[[2]] <- mediqr.glob.syn.path(serotype.eld)
hospital_cost_l_adj[[3]] <- mediqr.glob.syn.path(improv)
hospital_cost_l_adj[[4]] <- mediqr.glob.syn.path(improv.eld)
hospital_cost_l_adj[[5]] <- mediqr.glob.syn.path(Hib)


hospital_c <- rbindlist(hospital_cost_l)
hospital_cA <- rbindlist(hospital_cost_l_adj)
hospital_c <- rbind(hospital_c,hospital_cA)

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]

save(hospital_c,file="outputs/hospitalC_global_averted_pathogen_syndrome.RData")


######## GLOBAL + UNIT ############
hospital_c_l <- list()

for (i in 1:length(myfiles)){
  
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospitalC_region_averted <- vaccine_output_dt %>% 
    filter(!is.na(Pathogen)& vaccine_id!="_NA_NA___") %>%
    group_by(iso3c, Pathogen, class, Infectious.syndrome, run) %>%
    slice(1) %>%
    group_by(Pathogen, class, Infectious.syndrome) %>%
    summarise(Median_unitcost = median(los.cost, na.rm=TRUE),
              LOWIQR_unitcost = quantile(los.cost,0.25, na.rm = TRUE),
              HIGIQR_unitcost = quantile(los.cost,0.75, na.rm = TRUE),
              Median_los = median(los.DRI, na.rm=TRUE),
              LOWIQR_los = quantile(los.DRI,0.25, na.rm = TRUE),
              HIGIQR_los = quantile(los.DRI,0.75, na.rm = TRUE),)%>%
    as.data.table()
  hospital_c_l[[i]] <- hospitalC_region_averted
}


hospital_c<- rbindlist(hospital_c_l)
save(hospital_c,file="outputs/hospitalC_global_los.RData")
rm(hospital_c)
rm(hospitalC_region_averted)
rm(hospital_c_l)

######## GLOBAL Hospital Costs to get 1 final big number ############

mediqr.glob <- function(vaccine_output_dt){
  x <-   vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  x <- x %>% 
    filter(!is.na(Pathogen)& vaccine_id!="_NA_NA___") %>%
    group_by(vaccine_id, Infectious.syndrome, run) %>%
    summarise(hospital_cost =sum(cost_cases, na.rm=TRUE),
              avert_hospital_cost=sum(avertable_cost_cases,na.rm=TRUE),
              hospital_days=sum(days_cases, na.rm=TRUE),
              avert_hospital_days=sum(avertable_days_cases, na.rm=TRUE))%>%
    as.data.table()
  return(x)
}
########### nonadj ################
hospital_cost_l <- list()
for (i in 1:length(nonadj)){
  load(nonadj[i])
  hospital_cost_l[[i]] <- mediqr.glob(vaccine_output_dt)
}

########### adj ################
hospital_cost_l_adj <- list()

hospital_cost_l_adj[[1]] <- mediqr.glob(serotype)
hospital_cost_l_adj[[2]] <- mediqr.glob(serotype.eld)
hospital_cost_l_adj[[3]] <- mediqr.glob(improv)
hospital_cost_l_adj[[4]] <- mediqr.glob(improv.eld)
hospital_cost_l_adj[[5]] <- mediqr.glob(Hib)

hospital_c <- rbindlist(hospital_cost_l)
rm(hospital_cost_l)
hospital_cA <- rbindlist(hospital_cost_l_adj)
rm(hospital_cost_l_adj)

hospital_c <- rbind(hospital_c,hospital_cA)

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]


save(hospital_c,file="outputs/total_global_cost.RData")

