####### GETTING DATA TABLES FROM RESULTS ####################################

#### loading packages
library(tidyr)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

##### read in data

myfiles = list.files(path="C:/Users/nichola.naylor/Documents/WHO_2023/VAF_globalburden/outputs/fulloutput_chunks/",
                     pattern="*.RData", full.names = TRUE)

#### final table for database ######
hospital_cost_l <- list()
for (i in 1:length(myfiles)){
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
hospital_cost <- vaccine_output_dt %>% group_by(WHO.Region, Pathogen, Infectious.syndrome,
                                                vaccine_id, run) %>%
  summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
            who_region_cost_T = sum(cost_cases, na.rm=TRUE)) %>%
  group_by(WHO.Region, Pathogen, Infectious.syndrome, vaccine_id) %>%
  summarise(Mean_avert_costing = mean(who_region_cost_A, na.rm=TRUE),
            LOWUI_avert_costing = quantile(who_region_cost_A,0.025, na.rm = TRUE),
            HIGHUI_avert_costing = quantile(who_region_cost_A,0.975, na.rm = TRUE),
            Mean_total_costing = mean(who_region_cost_T, na.rm=TRUE),
            LOWUI_total_costing = quantile(who_region_cost_T,0.025, na.rm = TRUE),
            HIGHUI_total_costing = quantile(who_region_cost_T,0.975, na.rm = TRUE)) %>%
  mutate(across(Mean_avert_costing:HIGHUI_total_costing,
                ~ format(., big.mark = ",", scientific = F)))%>%
  mutate(mean_ci_averted = str_c(Mean_avert_costing, " ( ", LOWUI_avert_costing, "-",
                                 HIGHUI_avert_costing," ) "),
         mean_ci_total = str_c(Mean_total_costing, " ( ", LOWUI_total_costing, "-",
                                 HIGHUI_total_costing," ) ")) %>%
  as.data.table()
hospital_cost_l[[i]] <- hospital_cost
}

hospital_c <- rbindlist(hospital_cost_l)
hospital_c <- hospital_c[!is.na(Pathogen)]
hospital_c <- hospital_c[ , -c("Mean_avert_costing",  "LOWUI_avert_costing",
                               "HIGHUI_avert_costing", "Mean_total_costing" ,
                               "LOWUI_total_costing" , "HIGHUI_total_costing" )]
hospital_c <- dcast(hospital_c, Pathogen + Infectious.syndrome +
                             vaccine_id ~ WHO.Region, value.var = c("mean_ci_total",
                                                                    "mean_ci_averted"))

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
                       "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]
write.csv(hospital_c, file="outputs/END_hospital_costs_output.csv")
### note in the one sent there is also "total" but not added here in this version, find total vaccine/pathogen level results below
rm(hospital_c)
rm(hospital_cost_l)


#### SAME BUT FOR BEDDAYS ######
hospital_cost_l <- list()
for (i in 1:length(myfiles)){
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_cost <- vaccine_output_dt %>% group_by(WHO.Region, Pathogen, Infectious.syndrome,
                                                  vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_days_cases, na.rm=TRUE),
              who_region_cost_T = sum(days_cases, na.rm=TRUE)) %>%
    group_by(WHO.Region, Pathogen, Infectious.syndrome, vaccine_id) %>%
    summarise(Mean_avert_costing = mean(who_region_cost_A, na.rm=TRUE),
              LOWUI_avert_costing = quantile(who_region_cost_A,0.025, na.rm = TRUE),
              HIGHUI_avert_costing = quantile(who_region_cost_A,0.975, na.rm = TRUE),
              Mean_total_costing = mean(who_region_cost_T, na.rm=TRUE),
              LOWUI_total_costing = quantile(who_region_cost_T,0.025, na.rm = TRUE),
              HIGHUI_total_costing = quantile(who_region_cost_T,0.975, na.rm = TRUE)) %>%
    mutate(across(Mean_avert_costing:HIGHUI_total_costing,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate(mean_ci_averted = str_c(Mean_avert_costing, " ( ", LOWUI_avert_costing, "-",
                                   HIGHUI_avert_costing," ) "),
           mean_ci_total = str_c(Mean_total_costing, " ( ", LOWUI_total_costing, "-",
                                 HIGHUI_total_costing," ) ")) %>%
    as.data.table()
  hospital_cost_l[[i]] <- hospital_cost
}

hospital_c <- rbindlist(hospital_cost_l)
hospital_c <- hospital_c[!is.na(Pathogen)]
hospital_c <- hospital_c[ , -c("Mean_avert_costing",  "LOWUI_avert_costing",
                               "HIGHUI_avert_costing", "Mean_total_costing" ,
                               "LOWUI_total_costing" , "HIGHUI_total_costing" )]
hospital_c <- dcast(hospital_c, Pathogen + Infectious.syndrome +
                      vaccine_id ~ WHO.Region, value.var = c("mean_ci_total",
                                                             "mean_ci_averted"))

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]
write.csv(hospital_c, file="outputs/END_hospital_costs_output_beddays.csv")
### note in the one sent there is also "total" but not added here in this version, find total vaccine/pathogen level results below
rm(hospital_c)
rm(hospital_cost_l)

#### Global but to put in final GHO database ######
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
    mutate(mean_ci_averted_cost = str_c(Mean_avert_costing, " ( ", LOWUI_avert_costing, "-",
                                   HIGHUI_avert_costing," ) "),
           mean_ci_total_cost = str_c(Mean_total_costing, " ( ", LOWUI_total_costing, "-",
                                 HIGHUI_total_costing," ) "),
           mean_ci_averted_days = str_c(Mean_avert_days, " ( ", LOWUI_avert_days, "-",
                                        HIGHUI_avert_days," ) "),
           mean_ci_total_days = str_c(Mean_total_days, " ( ", LOWUI_total_days, "-",
                                      HIGHUI_total_days," ) ")) %>%
    as.data.table()
  hospital_cost_l[[i]] <- hospital_cost
}

hospital_c <- rbindlist(hospital_cost_l)
hospital_c <- hospital_c[!is.na(Pathogen)]
hospital_c <- hospital_c[ , -c("Mean_avert_costing",  "LOWUI_avert_costing",
                               "HIGHUI_avert_costing", "Mean_total_costing" ,
                               "LOWUI_total_costing" , "HIGHUI_total_costing",
                               "Mean_avert_days",  "LOWUI_avert_days",
                               "HIGHUI_avert_days", "Mean_total_days" ,
                               "LOWUI_total_days" , "HIGHUI_total_days")]

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]
write.csv(hospital_c, file="outputs/END_hospital_costs_output_global.csv")
### note in the one sent there is also "total" but not added here in this version, find total vaccine/pathogen level results below
rm(hospital_c)
rm(hospital_cost_l)

#### SAME TABLE BUT MEDIAN & IQR INSTEAD #################
hospital_cost_l <- list()
for (i in 1:length(myfiles)){
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospital_cost <- vaccine_output_dt %>% group_by(WHO.Region, Pathogen, Infectious.syndrome,
                                                  vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE)) %>%
    group_by(WHO.Region, Pathogen, Infectious.syndrome, vaccine_id) %>%
    summarise(Median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGHIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              Median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE)) %>%
    mutate(across(Median_avert_costing:HIGHIQR_total_costing,
                  ~ format(., big.mark = ",", scientific = F)))%>%
    mutate(median_iqr_averted = str_c(Median_avert_costing, " ( ", LOWIQR_avert_costing, "-",
                                   HIGHIQR_avert_costing," ) "),
           median_iqr_total = str_c(Median_total_costing, " ( ", LOWIQR_total_costing, "-",
                                 HIGHIQR_total_costing," ) ")) %>%
    as.data.table()
  hospital_cost_l[[i]] <- hospital_cost
}

hospital_c <- rbindlist(hospital_cost_l)
hospital_c <- hospital_c[!is.na(Pathogen)]
hospital_c <- hospital_c[ , -c("Median_avert_costing",  "LOWIQR_avert_costing",
                               "HIGHIQR_avert_costing", "Median_total_costing" ,
                               "LOWIQR_total_costing" , "HIGHIQR_total_costing" )]
hospital_c <- dcast(hospital_c, Pathogen + Infectious.syndrome +
                      vaccine_id ~ WHO.Region, value.var = c("median_iqr_total",
                                                             "median_iqr_averted"))

hospital_c[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
               "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those to mapped to any vaccine ID
hospital_c <- hospital_c[vaccine_id!="_NA_NA___"]
write.csv(hospital_c, file="outputs/END_hospital_costs_output_median.csv")
### note in the one sent there is also "total" but not added here in this version, find total vaccine/pathogen level results below
rm(hospital_c)
rm(hospital_cost_l)


######## REGION + ALL LEVELS ############
hospital_region_averted_l <- list()

for (i in 1:length(myfiles)){
  
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospitalC_region_averted <- vaccine_output_dt %>% 
    filter(!is.na(Pathogen)& vaccine_id!="_NA_NA___") %>%
    group_by(WHO.Region, Pathogen, class, Infectious.syndrome,
                                   vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases,na.rm=TRUE),
              who_region_days_T = sum(days_cases,na.rm=TRUE)) %>%
    group_by(WHO.Region, Pathogen, class, Infectious.syndrome,
             vaccine_id) %>%
    summarise(Median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              Median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              Median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              Median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE))%>%
    as.data.table()
  hospital_region_averted_l[[i]] <- hospitalC_region_averted
}


hospital_region_averted<- rbindlist(hospital_region_averted_l)
save(hospital_region_averted,file="outputs/hospitalC_region_averted.RData")
rm(hospital_region_averted)
rm(hospitalC_region_averted)
rm(hospital_region_averted_l)


######## REGION + CLASS ############
hospital_region_averted_l <- list()

for (i in 1:length(myfiles)){
  
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospitalC_region_averted <- vaccine_output_dt %>% 
    filter(!is.na(Pathogen)& vaccine_id!="_NA_NA___") %>%
    group_by(WHO.Region, class,
             vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases,na.rm=TRUE),
              who_region_days_T = sum(days_cases,na.rm=TRUE)) %>%
    group_by(WHO.Region, class,
             vaccine_id) %>%
    summarise(Median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              Median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              Median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              Median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE))%>%
    as.data.table()
  hospital_region_averted_l[[i]] <- hospitalC_region_averted
}


hospital_region_averted<- rbindlist(hospital_region_averted_l)
save(hospital_region_averted,file="outputs/hospitalC_region_averted_class.RData")
rm(hospital_region_averted)
rm(hospitalC_region_averted)
rm(hospital_region_averted_l)


######## REGION + PATHOGEN ############
hospital_region_averted_l <- list()

for (i in 1:length(myfiles)){
  
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospitalC_region_averted <- vaccine_output_dt %>% 
    filter(!is.na(Pathogen)& vaccine_id!="_NA_NA___") %>%
    group_by(WHO.Region, Pathogen, 
             vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases,na.rm=TRUE),
              who_region_days_T = sum(days_cases,na.rm=TRUE)) %>%
    group_by(WHO.Region, Pathogen, 
             vaccine_id) %>%
    summarise(Median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              Median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              Median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              Median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE))%>%
    as.data.table()
  hospital_region_averted_l[[i]] <- hospitalC_region_averted
}


hospital_region_averted<- rbindlist(hospital_region_averted_l)
save(hospital_region_averted,file="outputs/hospitalC_region_averted_pathogen.RData")
rm(hospital_region_averted)
rm(hospitalC_region_averted)
rm(hospital_region_averted_l)

######## REGION + SYNDROME ############
hospital_region_averted_l <- list()

for (i in 1:length(myfiles)){
  
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospitalC_region_averted <- vaccine_output_dt %>% 
    filter(!is.na(Pathogen)& vaccine_id!="_NA_NA___") %>%
    group_by(WHO.Region, Infectious.syndrome,
             vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases,na.rm=TRUE),
              who_region_days_T = sum(days_cases,na.rm=TRUE)) %>%
    group_by(WHO.Region, Infectious.syndrome,
             vaccine_id) %>%
    summarise(Median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              Median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              Median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              Median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE))%>%
    as.data.table()
  hospital_region_averted_l[[i]] <- hospitalC_region_averted
}


hospital_region_averted<- rbindlist(hospital_region_averted_l)
save(hospital_region_averted,file="outputs/hospitalC_region_averted_syndrome.RData")
rm(hospital_region_averted)
rm(hospitalC_region_averted)
rm(hospital_region_averted_l)

######## GLOBAL + ALL LEVELS ############
hospital_region_averted_l <- list()

for (i in 1:length(myfiles)){
  
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospitalC_region_averted <- vaccine_output_dt %>% 
    filter(!is.na(Pathogen)& vaccine_id!="_NA_NA___") %>%
    group_by(Pathogen, class, Infectious.syndrome,
             vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases,na.rm=TRUE),
              who_region_days_T = sum(days_cases,na.rm=TRUE)) %>%
    group_by(Pathogen, class, Infectious.syndrome,
             vaccine_id) %>%
    summarise(Median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              Median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              Median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              Median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE))%>%
    as.data.table()
  hospital_region_averted_l[[i]] <- hospitalC_region_averted
}


hospital_region_averted<- rbindlist(hospital_region_averted_l)
save(hospital_region_averted,file="outputs/hospitalC_global_averted.RData")
rm(hospital_region_averted)
rm(hospitalC_region_averted)
rm(hospital_region_averted_l)

######## GLOBAL + CLASS ############
hospital_region_averted_l <- list()

for (i in 1:length(myfiles)){
  
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospitalC_region_averted <- vaccine_output_dt %>% 
    filter(!is.na(Pathogen)& vaccine_id!="_NA_NA___") %>%
    group_by(class, 
             vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases,na.rm=TRUE),
              who_region_days_T = sum(days_cases,na.rm=TRUE)) %>%
    group_by(class, 
             vaccine_id) %>%
    summarise(Median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              Median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              Median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              Median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE))%>%
    as.data.table()
  hospital_region_averted_l[[i]] <- hospitalC_region_averted
}


hospital_region_averted<- rbindlist(hospital_region_averted_l)
save(hospital_region_averted,file="outputs/hospitalC_global_averted_class.RData")
rm(hospital_region_averted)
rm(hospitalC_region_averted)
rm(hospital_region_averted_l)

######## GLOBAL + PATHOGEN ############
hospital_region_averted_l <- list()

for (i in 1:length(myfiles)){
  
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospitalC_region_averted <- vaccine_output_dt %>% 
    filter(!is.na(Pathogen)& vaccine_id!="_NA_NA___") %>%
    group_by(Pathogen, 
             vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases,na.rm=TRUE),
              who_region_days_T = sum(days_cases,na.rm=TRUE)) %>%
    group_by(Pathogen, 
             vaccine_id) %>%
    summarise(Median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              Median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              Median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              Median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE))%>%
    as.data.table()
  hospital_region_averted_l[[i]] <- hospitalC_region_averted
}


hospital_region_averted<- rbindlist(hospital_region_averted_l)
save(hospital_region_averted,file="outputs/hospitalC_global_averted_pathogen.RData")
rm(hospital_region_averted)
rm(hospitalC_region_averted)
rm(hospital_region_averted_l)

######## GLOBAL + SYNDROME ############
hospital_region_averted_l <- list()

for (i in 1:length(myfiles)){
  
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospitalC_region_averted <- vaccine_output_dt %>% 
    filter(!is.na(Pathogen)& vaccine_id!="_NA_NA___") %>%
    group_by( Infectious.syndrome,
             vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases,na.rm=TRUE),
              who_region_days_T = sum(days_cases,na.rm=TRUE)) %>%
    group_by(Infectious.syndrome,
             vaccine_id) %>%
    summarise(Median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              Median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              Median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              Median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE))%>%
    as.data.table()
  hospital_region_averted_l[[i]] <- hospitalC_region_averted
}


hospital_region_averted<- rbindlist(hospital_region_averted_l)
save(hospital_region_averted,file="outputs/hospitalC_global_averted_syndrome.RData")
rm(hospital_region_averted)
rm(hospitalC_region_averted)
rm(hospital_region_averted_l)

######## GLOBAL + PATHOGEN + SYNDROME ############
hospital_region_averted_l <- list()

for (i in 1:length(myfiles)){
  
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospitalC_region_averted <- vaccine_output_dt %>% 
    filter(!is.na(Pathogen)& vaccine_id!="_NA_NA___") %>%
    group_by(Pathogen,Infectious.syndrome,
             vaccine_id, run) %>%
    summarise(who_region_cost_A = sum(avertable_cost_cases, na.rm=TRUE),
              who_region_cost_T = sum(cost_cases, na.rm=TRUE),
              who_region_days_A = sum(avertable_days_cases,na.rm=TRUE),
              who_region_days_T = sum(days_cases,na.rm=TRUE)) %>%
    group_by(Pathogen, Infectious.syndrome,
             vaccine_id) %>%
    summarise(Median_avert_costing = median(who_region_cost_A, na.rm=TRUE),
              LOWIQR_avert_costing = quantile(who_region_cost_A,0.25, na.rm = TRUE),
              HIGIQR_avert_costing = quantile(who_region_cost_A,0.75, na.rm = TRUE),
              Median_total_costing = median(who_region_cost_T, na.rm=TRUE),
              LOWIQR_total_costing = quantile(who_region_cost_T,0.25, na.rm = TRUE),
              HIGHIQR_total_costing = quantile(who_region_cost_T,0.75, na.rm = TRUE),
              Median_avert_days = median(who_region_days_A, na.rm=TRUE),
              LOWIQR_avert_days = quantile(who_region_days_A,0.25, na.rm = TRUE),
              HIGIQR_avert_days = quantile(who_region_days_A,0.75, na.rm = TRUE),
              Median_total_days = median(who_region_days_T, na.rm=TRUE),
              LOWIQR_total_days = quantile(who_region_days_T,0.25, na.rm = TRUE),
              HIGHIQR_total_days = quantile(who_region_days_T,0.75, na.rm = TRUE))%>%
    as.data.table()
  hospital_region_averted_l[[i]] <- hospitalC_region_averted
}


hospital_region_averted<- rbindlist(hospital_region_averted_l)
save(hospital_region_averted,file="outputs/hospitalC_global_averted_pathogen_syndrome.RData")
rm(hospital_region_averted)
rm(hospitalC_region_averted)
rm(hospital_region_averted_l)

######## GLOBAL + UNIT ############
hospital_region_averted_l <- list()

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
  hospital_region_averted_l[[i]] <- hospitalC_region_averted
}


hospital_region_averted<- rbindlist(hospital_region_averted_l)
save(hospital_region_averted,file="outputs/hospitalC_global_los.RData")
rm(hospital_region_averted)
rm(hospitalC_region_averted)
rm(hospital_region_averted_l)

######## GLOBAL Hospital Costs to get 1 final big number ############

hospital_region_averted_l <- list()

for (i in 1:length(myfiles)){
  
  load(myfiles[i])
  vaccine_output_dt[WHO.Region=="PAHO", WHO.Region := "AMRO"]
  hospitalC_region_averted <- vaccine_output_dt %>% 
    filter(!is.na(Pathogen)& vaccine_id!="_NA_NA___") %>%
    group_by(vaccine_id, Infectious.syndrome, run) %>%
    summarise(hospital_cost =sum(cost_cases, na.rm=TRUE),
              avert_hospital_cost=sum(avertable_cost_cases,na.rm=TRUE),
              hospital_days=sum(days_cases, na.rm=TRUE),
              avert_hospital_days=sum(avertable_days_cases, na.rm=TRUE))%>%
    as.data.table()
  hospital_region_averted_l[[i]] <- hospitalC_region_averted
}


hospital_region_averted<- rbindlist(hospital_region_averted_l)
save(hospital_region_averted,file="outputs/total_global_cost.RData")
rm(hospital_region_averted)
rm(hospitalC_region_averted)
rm(hospital_region_averted_l)
