############## CREATING PLOTS FROM OUTPUTS ##############


#### loading packages
library(tidyr)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)


### formatting ###
options(scipen=10000) ## turn off scientific notation
`%!in%` = Negate(`%in%`) ## create function for negating %in%

## getting the different 1 scenario per bug
load("outputs/hospitalC_global_averted.RData")

unique(hospital_c$vaccine_id)


keep_vac <- c(               
 "Haemophilus influenzae type B_both_0.9_5 years_All_6, 10, 14 weeks"                                                           
,"Salmonella Typhi_0.85_0.7_15 years_All_9 months"                                                                              
, "Staphylococcus aureus_0.6_0.7_5 years_All_All age groups"                                                               
, "E. coli - non-diarrhogenic_0.7_0.7_5 years_All except Diarrhoea_All age groups"                                      
, "ETEC_0.6_0.7_5 years_Diarrhoea_6 months"                                                                   
, "Group A streptococcus_0.7_0.7_5 years_All_6 weeks"                                                                            
, "Klebsiella pneumoniae - all_0.7_0.7_5 years_All_All age groups"                                                      
, "Streptococcus pneumoniae - Improved_both_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6 weeks & elderly age group"
, "Pseudomonas aeruginosa_0.7_0.7_5 years_BSI, LRI and thorax infections_All age groups"                                         
,"Enterococcus faecium_0.7_0.7_5 years_All_All age groups"                                                                      
, "Salmonella paratyphi_0.7_0.7_5 years_All_9 months"                                                                            
, "Acinetobacter baumannii - all_0.7_0.7_5 years_All_All age groups"                                                            
, "Mycobacterium tuberculosis - Improved_0.8_0.7_10 years_All_0 weeks + boost every 10 years"                                    
, "Shigella_0.6_0.7_5 years_All_6 months"  ) 
### need to specify different ones for productivity as didn't group
## vaccine scenarios like had done for 
keep_vac_prod <- c(               
  "Haemophilus influenzae type B_0.93_0.9_5 years_All_6, 10, 14 weeks"  
  ,"Haemophilus influenzae type B_0.69_0.9_5 years_All_6, 10, 14 weeks" 
  ,"Streptococcus pneumoniae - Improved_0.7_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6 weeks & elderly age group"
  ,"Streptococcus pneumoniae - Improved_0.5_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6 weeks & elderly age group"
  ,"Salmonella Typhi_0.85_0.7_15 years_All_9 months"                                                                              
  , "Staphylococcus aureus_0.6_0.7_5 years_All_All age groups"                                                               
  , "E. coli - non-diarrhogenic_0.7_0.7_5 years_All except Diarrhoea_All age groups"                                      
  , "ETEC_0.6_0.7_5 years_Diarrhoea_6 months"                                                                   
  , "Group A streptococcus_0.7_0.7_5 years_All_6 weeks"                                                                            
  , "Klebsiella pneumoniae - all_0.7_0.7_5 years_All_All age groups"                                                      
  , "Pseudomonas aeruginosa_0.7_0.7_5 years_BSI, LRI and thorax infections_All age groups"                                         
  ,"Enterococcus faecium_0.7_0.7_5 years_All_All age groups"                                                                      
  , "Salmonella paratyphi_0.7_0.7_5 years_All_9 months"                                                                            
  , "Acinetobacter baumannii - all_0.7_0.7_5 years_All_All age groups"                                                            
  , "Mycobacterium tuberculosis - Improved_0.8_0.7_10 years_All_0 weeks + boost every 10 years"                                    
  , "Shigella_0.6_0.7_5 years_All_6 months"  ) 

######### !!! need to add back in the removal of double counting ETEC UTI etc.
##### 

## selecting ones that have the theoretical biggest impact (e.g. most age groups/syndromes)

cleaning.dt <- function(hospital_c){
  global_hospital <- hospital_c[hospital_c$vaccine_id %in% keep_vac]
  
  ### need to specify for E.coli target syndromes so no double counting
  global_hospital[ , flag := 0]
  global_hospital[(vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months" & 
                Infectious.syndrome!="Diarrhoea")|(
                  vaccine_id=="E. coli - non-diarrhogenic_0.7_0.7_5 years_All except Diarrhoea_All age groups"& 
                    Infectious.syndrome=="Diarrhoea"),
             flag := 1]
  global_hospital <-  global_hospital[flag==0]
  global_hospital <-  global_hospital[ ,-c("flag")]
  
  
  return(global_hospital)
}

cleaning.dt.prod <- function(hospital_c){
  global_hospital <- hospital_c[hospital_c$vaccine_id %in% keep_vac_prod]
  
  ### need to specify for E.coli target syndromes so no double counting
  global_hospital[ , flag := 0]
  global_hospital[(vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months" & 
                     Infectious.syndrome!="Diarrhoea")|(
                       vaccine_id=="E. coli - non-diarrhogenic_0.7_0.7_5 years_All except Diarrhoea_All age groups"& 
                         Infectious.syndrome=="Diarrhoea"),
                  flag := 1]
  global_hospital <-  global_hospital[flag==0]
  global_hospital <-  global_hospital[ ,-c("flag")]
  
  
  return(global_hospital)
}

###### ******global totals ********#####################################

#### HOSPITAL COSTS ##########################
load("outputs/hospitalC_global_averted.RData")

hospital_c <- cleaning.dt(hospital_c)

### getting vaccine ids 
vacs <- data.table(vacsid = unique(hospital_c$vaccine_id))
vacs[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
         "duration", "target disease","target population") := tstrsplit(vacsid, "_", fixed=TRUE)]
write.csv(vacs,file="outputs/vaccine_ids.csv")

hospital_c[class=="Fluoroquinolones or MDR in Salmonella",class :="fluoroquinolones & mdr"]
hospital_c[Infectious.syndrome=="BSI", Infectious.syndrome := "Bloodstream infections"]
hospital_c[Infectious.syndrome=="UTI", Infectious.syndrome := "Urinary tract infections"]
hospital_c[Infectious.syndrome=="TB", Infectious.syndrome := "Tuberculosis"]

hospital_c[, c("cost_med","other1") := tstrsplit(median_iqr_total_cost, "(", fixed=TRUE)]
hospital_c[, c("avert_cost_med","other1") := tstrsplit(median_iqr_averted_cost, "(", fixed=TRUE)]

hospital_c[ , cost_med := as.numeric(gsub(",", "", cost_med))]
hospital_c[ , avert_cost_med := as.numeric(gsub(",", "", avert_cost_med))]

 ## billions
(hospital_c[, lapply(.SD, sum, na.rm=TRUE),
         .SDcols=c("cost_med",
                   "avert_cost_med")])*1/1e9


classrank <- (hospital_c[, lapply(.SD, sum, na.rm=TRUE),
                      by="class",
                      .SDcols=c("cost_med",
                                "avert_cost_med")])

hospital_c <- setorder(hospital_c, cost_med)
hospital_c$Pathogen <- factor(hospital_c$Pathogen , 
                                   levels = 
                                     unique(hospital_c$Pathogen[order(hospital_c$cost_med)]))

path <- hospital_c$Pathogen

hospital_c$class <- factor(hospital_c$class, levels=unique(hospital_c$class))

abxcolours <-
  setNames( c("#660099", "#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
            , levels(hospital_c$class)  )

ggplot(hospital_c, aes(Pathogen, cost_med,
                            fill=class)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = abxcolours)+
  theme(axis.text.x = element_text(angle = -90),
        text= element_text(size = 15))+
  scale_y_continuous(label= scales::comma)+
  xlab("Pathogen")+
  ylab("Total Hospital Costs (USD)")+
  labs(fill="Antibiotic Class") 

ggplot(hospital_c, aes(Pathogen, cost_med,
                            fill=Infectious.syndrome)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = -90),
        text= element_text(size = 15))+
  scale_y_continuous(label= scales::comma)+
  xlab("Pathogen")+
  ylab("Total Hospital Costs (USD)")+
  labs(fill="Syndrome")


############## getting a global median and IQR
load("outputs/total_global_cost.RData")

global_hospital_totals <- cleaning.dt(hospital_c)

global_hospital_totals <- global_hospital_totals %>% 
  group_by(run) %>%
  summarise(cost = sum(hospital_cost, na.rm=TRUE),
            days = sum(hospital_days, na.rm=TRUE),
            cost_avert = sum(avert_hospital_cost,na.rm=TRUE),
            days_avert = sum(avert_hospital_days,na.rm=TRUE)) %>%
  summarise(avert_cost_med = median(cost_avert, na.rm=TRUE),
            avert_cost_lo = quantile(cost_avert,0.25, na.rm = TRUE),
            avert_cost_hi = quantile(cost_avert,0.75, na.rm = TRUE),
            cost_med = median(cost, na.rm=TRUE),
            cost_lo = quantile(cost,0.25, na.rm = TRUE),
            cost_hi = quantile(cost,0.75, na.rm = TRUE),
            avert_days_med = median(days_avert, na.rm=TRUE),
            avert_days_lo = quantile(days_avert,0.25, na.rm = TRUE),
            avert_days_hi = quantile(days_avert,0.75, na.rm = TRUE),
            days_med = median(days, na.rm=TRUE),
            days_lo = quantile(days,0.25, na.rm = TRUE),
            days_hi = quantile(days,0.75, na.rm = TRUE))%>%
  as.data.table()

global_hospital_totals <- global_hospital_totals/1e9

print(global_hospital_totals)

write.csv(global_hospital_totals, file="outputs/global_hospital_totals.csv")

#### Global productivity ##########################

prod_deaths <- read.csv("outputs/productivity_loss_deaths_all.csv")
prod_deaths <- as.data.table(prod_deaths)
### getting one vaccine/one DRI per group
prod_deaths <- cleaning.dt.prod(prod_deaths)

prod_deaths[Infectious.syndrome=="BSI", Infectious.syndrome := "Bloodstream infections"]
prod_deaths[Infectious.syndrome=="UTI", Infectious.syndrome := "Urinary tract infections"]
prod_deaths[Infectious.syndrome=="TB", Infectious.syndrome := "Tuberculosis"]
prod_deaths[Antibiotic.class=="Methicillin", Antibiotic.class := "Penicillin"]
prod_deaths[Antibiotic.class=="Multi-drug resistance excluding extensive drug resistance in TB",
            Antibiotic.class := "MDR"]
prod_deaths[Antibiotic.class=="Multi-drug resistance in Salmonella Typhi and Paratyphi",
            Antibiotic.class := "MDR"]

(prod_deaths[, lapply(.SD, sum, na.rm=TRUE),
            .SDcols=c("HC_cost",
                      "averted_HC")])*1/1e9

global_prod_deaths <- prod_deaths[, lapply(.SD, sum, na.rm=TRUE),
                                  by = c("Pathogen",
                                         "Antibiotic.class",
                                         "Infectious.syndrome"),
                                  .SDcols=c("HC_cost",
                                            "averted_HC")]


### get it so the productivity and hc costs are the same order of pathogens 
### and the same colours for antibiotic classes

abx_dic <- unique(global_prod_deaths[,c("Antibiotic.class")])
abx_dic[Antibiotic.class=="Carbapenems", class:="carbapenems"]
abx_dic[Antibiotic.class=="Fluoroquinolones", class:="fluoroquinolones"]
abx_dic[Antibiotic.class=="Third-generation cephalosporins", class:="3g cephalosporins"]
abx_dic[Antibiotic.class=="Macrolide", class:="macrolides"]
abx_dic[Antibiotic.class=="MDR", class:="mdr"]
abx_dic[Antibiotic.class=="Penicillin", class:="penicillins"]
abx_dic[Antibiotic.class=="Vancomycin", class:="glycopeptides"]

global_prod_deaths <- merge(global_prod_deaths,abx_dic,by="Antibiotic.class")

global_prod_deaths$Pathogen <- factor(global_prod_deaths$Pathogen , 
                              levels = levels(path))

ggplot(global_prod_deaths, aes(Pathogen, HC_cost,
                               fill=class)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = -90),
        text= element_text(size = 15))+
  scale_fill_manual(values = abxcolours)+
  scale_y_continuous(label= scales::comma)+
  xlab("Pathogen")+
  ylab("Total Productivity Losses (USD)")+
  labs(fill='Antibiotic Class') 

ggplot(global_prod_deaths, aes(Pathogen, HC_cost,
                               fill=Infectious.syndrome)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = -90),
        text= element_text(size = 15))+
  scale_y_continuous(label= scales::comma)+
  xlab("Pathogen")+
  ylab("Total Productivity Losses (USD)")+
  labs(fill='Syndrome')

############******* REGIONAL PLOTS******* ################

########## HOSPITAL ########################
load("outputs/hospitalC_region_averted.RData")

regional_hospital_totals <- cleaning.dt(hospital_c)

regional_hospital_totals[WHO.Region=="PAHO", WHO.Region := "AMRO"]

getting_numbers_bk <- function(regional_hospital_totals){
### remove commas from numeric strings
regional_hospital_totals[ , median_iqr_total_cost := gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", median_iqr_total_cost)) ]
regional_hospital_totals[ , median_iqr_averted_cost := gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", median_iqr_averted_cost)) ]
regional_hospital_totals[ , median_iqr_total_days := gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", median_iqr_total_days)) ]
regional_hospital_totals[ , median_iqr_averted_days := gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", median_iqr_averted_days)) ]

### separate out numbers in/across brackets
Separate <- function(...) separate(..., sep = "[^[:alnum:].]+", convert = TRUE)

regional_hospital_totals <- regional_hospital_totals %>%
  Separate(median_iqr_total_cost, into = c("cost_med", "cost_lo", "cost_hi", NA)) %>%
  Separate(median_iqr_averted_cost, into = c("avert_cost_med", "avert_cost_lo", "avert_cost_hi", NA)) %>%
  Separate(median_iqr_total_days, into = c("days_med", "days_lo", "days_hi", NA)) %>%
  Separate(median_iqr_averted_days, into = c("avert_days_med", "avert_days_lo", "avert_days_hi", NA)) %>%
  as.data.table()
return(regional_hospital_totals)
}

regional_hospital_totals <- getting_numbers_bk(regional_hospital_totals)
## regional
totals.temp <- (regional_hospital_totals[, lapply(.SD, sum, na.rm=TRUE),
 by="WHO.Region",
 .SDcols=c("cost_med",
           "avert_cost_med")])

write.csv(totals.temp,file="outputs/regional_median_HC.csv")
rm(totals.temp)

##################### by pathogen - averted hospital costs ####
load("outputs/hospitalC_region_averted_pathogen.RData")

regional_hospital_totals <-    hospital_c[hospital_c$vaccine_id %in% keep_vac]
####!!! don't use for totals pre-vaccine as will have double counting for e.coli
### trying to account for this
### accounting for duplication of E. coli total amounts for ETEC & EXPEC
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         cost_med :=0]
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         cost_lo :=0]
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         cost_hi :=0]
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         days_med :=0]
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         days_lo :=0]
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         days_hi :=0]

regional_hospital_totals <- getting_numbers_bk(regional_hospital_totals)

regional_hospital_totals[WHO.Region=="PAHO", WHO.Region := "AMRO"]

save(regional_hospital_totals, file="outputs/regional_hospital_4additionalplots.RData")

total_averted <- regional_hospital_totals[, lapply(.SD, sum, na.rm=TRUE),
                                           by = c("Pathogen"),
                                           .SDcols=c("avert_cost_med")]

total_averted_los <-  regional_hospital_totals[, lapply(.SD, sum, na.rm=TRUE),
                                              by = c("Pathogen"),
                                              .SDcols=c("avert_days_med")]

total_averted <- merge(total_averted, total_averted_los, by="Pathogen")

ggplot(regional_hospital_totals, aes(x = Pathogen, y = avert_cost_med,
                                              fill=WHO.Region)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = avert_cost_lo, ymax = avert_cost_hi),
                position = position_dodge(0.9), width = .2)+
  xlab("Resistance Exposure") +
  ylab("Averted Hospital Costs (2019 USD)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text = element_text(size = 15))+
  scale_y_continuous(label= scales::comma)+
  labs(fill="WHO Region")


ggplot(regional_hospital_totals, aes(x = Pathogen, y = avert_days_med,
                                                fill=WHO.Region)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = avert_days_lo, ymax = avert_days_hi),
                position = position_dodge(0.9), width = .2)+
  xlab("Pathogen") +
  ylab("Averted Hospital Bed Days") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text = element_text(size = 15))+
  scale_y_continuous(label= scales::comma)+
  labs(fill="WHO Region")


#### regional productivity loss #######

prod_deaths <- read.csv("outputs/productivity_loss_deaths_all.csv")
prod_deaths <- as.data.table(prod_deaths)
### getting one vaccine/one DRI per group

prod_deaths <- cleaning.dt.prod(prod_deaths)

prod_deaths[Infectious.syndrome=="BSI", Infectious.syndrome := "Bloodstream infections"]
prod_deaths[Infectious.syndrome=="UTI", Infectious.syndrome := "Urinary tract infections"]
prod_deaths[Infectious.syndrome=="TB", Infectious.syndrome := "Tuberculosis"]
prod_deaths[Antibiotic.class=="Methicillin", Antibiotic.class := "Penicillin"]
prod_deaths[Antibiotic.class=="Multi-drug resistance excluding extensive drug resistance in TB",
            Antibiotic.class := "MDR"]
prod_deaths[Antibiotic.class=="Multi-drug resistance in Salmonella Typhi and Paratyphi",
            Antibiotic.class := "MDR"]

save(prod_deaths, file="outputs/prod_4additionalplots.RData")


### total productivity loss by syndrome
syndromerank <- prod_deaths[, lapply(.SD, sum, na.rm=TRUE),
                  by = c("Infectious.syndrome"),
                  .SDcols=c("HC_cost")]

write.csv(syndromerank, file="outputs/global_syndrome_rankings_productivity.csv")


### averted by pathogen
regional_prod_path  <-prod_deaths[, lapply(.SD, sum, na.rm=TRUE),
                                    by = c("Pathogen",
                                           ".id"),
                                    .SDcols=c("averted_HC")]

ggplot(regional_prod_path, aes(x = Pathogen, y = averted_HC,
                               fill=.id)) +
  geom_col(position = "dodge") +
  xlab("Pathogen") +
  ylab("Averted Productivity Losses (USD)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(label= scales::comma)+
  scale_fill_discrete(name = "WHO Region")

####
load("outputs/working_life_years_lost.Rdata")

output_hc <- as.data.table(output_hc)
### getting one vaccine/one DRI per group
output_hc <- cleaning.dt(output_hc)

regional_prod_path  <-output_hc[, lapply(.SD, sum, na.rm=TRUE),
                                  by = c("Pathogen",
                                         ".id"),
                                  .SDcols=c("averted_HC")]
regional_prod_path[.id=="PAHO",.id := "AMRO"]

ggplot(regional_prod_path  , aes(x = Pathogen, y = averted_HC,
                               fill=.id)) +
  geom_col(position = "dodge") +
  xlab("Pathogen") +
  ylab("Averted Productivity Losses (WLYL)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(label= scales::comma)+
  scale_fill_discrete(name = "WHO Region")


###### table of combining average values per vaccine ####

load("outputs/hospitalC_global_averted.RData")

global_hospital_totals <- cleaning.dt(hospital_c)

global_hospital_totals[class=="Fluoroquinolones or MDR in Salmonella",Antibiotic.class :="any"]
global_hospital_totals[Infectious.syndrome=="BSI", Infectious.syndrome := "Bloodstream infections"]
global_hospital_totals[Infectious.syndrome=="UTI", Infectious.syndrome := "Urinary tract infections"]
global_hospital_totals[Antibiotic.class=="Methicillin", Antibiotic.class := "Penicillin"]
global_hospital_totals[Infectious.syndrome=="TB", Infectious.syndrome := "Tuberculosis"]

global_hospital_totals <- getting_numbers_bk(global_hospital_totals)

vaccine_hospital <- global_hospital_totals[, lapply(.SD, sum, na.rm=TRUE),
                                        by = c("vaccine_id"),
                                        .SDcols=c("avert_cost_med")]

prod_deaths <- read.csv("outputs/productivity_loss_deaths_all.csv")
prod_deaths <- as.data.table(prod_deaths)
### getting one vaccine/one DRI per group
prod_deaths <- cleaning.dt.prod(prod_deaths)

prod_deaths[Infectious.syndrome=="BSI", Infectious.syndrome := "Bloodstream infections"]
prod_deaths[Infectious.syndrome=="UTI", Infectious.syndrome := "Urinary tract infections"]
prod_deaths[Infectious.syndrome=="TB", Infectious.syndrome := "Tuberculosis"]
prod_deaths[Antibiotic.class=="Methicillin", Antibiotic.class := "Penicillin"]
prod_deaths[Antibiotic.class=="Multi-drug resistance excluding extensive drug resistance in TB",
            Antibiotic.class := "MDR"]
prod_deaths[Antibiotic.class=="Multi-drug resistance in Salmonella Typhi and Paratyphi",
            Antibiotic.class := "MDR"]

vaccine_productivity <- prod_deaths[, lapply(.SD, sum, na.rm=TRUE),
         by = c("vaccine_id"),
         .SDcols=c("averted_HC")]

totalcostingtemp <- merge(vaccine_hospital,vaccine_productivity, by="vaccine_id")

totalcostingtemp[ ,total.cost := avert_cost_med+averted_HC]

write.csv(totalcostingtemp, file="outputs/totalcosting.csv")

#### regional comparing regional friction cost to human capital #########

fc.prod <- read.csv("outputs/vaccine_avertable_FC.csv")

fc.prod <- as.data.table(fc.prod)
### getting one vaccine/one DRI per group
fc.prod <- cleaning.dt.prod(fc.prod)

fc.prod[Infectious.syndrome=="BSI", Infectious.syndrome := "Bloodstream infections"]
fc.prod[Infectious.syndrome=="UTI", Infectious.syndrome := "Urinary tract infections"]
fc.prod[Infectious.syndrome=="TB", Infectious.syndrome := "Tuberculosis"]
fc.prod[Antibiotic.class=="Methicillin", Antibiotic.class := "Penicillin"]
fc.prod[Antibiotic.class=="Multi-drug resistance excluding extensive drug resistance in TB",
            Antibiotic.class := "MDR"]
fc.prod[Antibiotic.class=="Multi-drug resistance in Salmonella Typhi and Paratyphi",
            Antibiotic.class := "MDR"]

(fc.prod[, lapply(.SD, sum, na.rm=TRUE),
             .SDcols=c("wage_loss_total",
                       "wage_loss_averted")])*1/1e9

##### sc2
sc2.prod <- read.csv("outputs/productivity_loss_deaths_all_sc2.csv")

sc2.prod <- as.data.table(sc2.prod)
sc2.prod <- cleaning.dt.prod(sc2.prod)


sc2.prod[Infectious.syndrome=="BSI", Infectious.syndrome := "Bloodstream infections"]
sc2.prod[Infectious.syndrome=="UTI", Infectious.syndrome := "Urinary tract infections"]
sc2.prod[Infectious.syndrome=="TB", Infectious.syndrome := "Tuberculosis"]
sc2.prod[Antibiotic.class=="Methicillin", Antibiotic.class := "Penicillin"]
sc2.prod[Antibiotic.class=="Multi-drug resistance excluding extensive drug resistance in TB",
        Antibiotic.class := "MDR"]
sc2.prod[Antibiotic.class=="Multi-drug resistance in Salmonella Typhi and Paratyphi",
        Antibiotic.class := "MDR"]

(sc2.prod[, lapply(.SD, sum, na.rm=TRUE),
         .SDcols=c("HC_cost",
                   "averted_HC")])*1/1e9

load("outputs/prod_4additionalplots.RData")
sc1.prod <- cleaning.dt.prod(prod_deaths)
## rename columns pre-merge
names(sc2.prod)[names(sc2.prod) == 'HC_cost'] <- 'sc2_total'
names(sc2.prod)[names(sc2.prod) == 'averted_HC'] <- 'sc2_averted'
names(sc1.prod)[names(sc1.prod) == 'HC_cost'] <- 'sc1_total'
names(sc1.prod)[names(sc1.prod) == 'averted_HC'] <- 'sc1_averted'
names(fc.prod)[names(fc.prod) == 'wage_loss_total'] <- 'fc_total'
names(fc.prod)[names(fc.prod) == 'wage_loss_averted'] <- 'fc_averted'

all.prod.methods <- merge(sc1.prod,sc2.prod, by=c(".id",
                                                  "Pathogen",
                                                  "vaccine_id",
                                                  "Antibiotic.class",
                                                  "Infectious.syndrome"))

all.prod.methods <- merge(all.prod.methods,fc.prod, by.x=c(".id",
                                                  "Pathogen",
                                                  "vaccine_id",
                                                  "Antibiotic.class",
                                                  "Infectious.syndrome"),
                          by.y=c("who.region",
                                 "Pathogen",
                                 "vaccine_id",
                                 "Antibiotic.class",
                                 "Infectious.syndrome"))

save(all.prod.methods, file="outputs/prod_additional4plots_sc2.RData")

all.prod.methods.long <- data.table::melt(all.prod.methods, id.vars = c(".id",
                                                                        "Pathogen" ,
                                          "Infectious.syndrome" ,
                                          "vaccine_id"), 
                 measure.vars = list(c("sc1_averted",
                                       "sc2_averted",
                                       "fc_averted"),
                                     c('sc1_total',
                                       "sc2_total",
                                       "fc_total")),
                 value.name = c('averted','total'),
                 variable.name="scenario")
all.prod.methods.long[scenario==1 , scenario := "Human Capital Scenario 1"]
all.prod.methods.long[scenario==2 , scenario := "Human Capital Scenario 2"]
all.prod.methods.long[scenario==3 , scenario := "Friction Cost"]


### group by region

all.prod.methods.global <- all.prod.methods[, lapply(.SD, sum, na.rm=TRUE),
                                by = c(".id"),
                                .SDcols=c("sc1_total",
                                          "sc2_total",
                                          "fc_total")]

all.prod.methods.global.temp <- data.table::melt(all.prod.methods.global, id.vars = c(".id"), 
                                          measure.vars =c('sc1_total',
                                                                "sc2_total"),
                                          value.name = c('total'),
                                          variable.name="scenario")

all.prod.methods.fc <- all.prod.methods.global[ , c(".id","fc_total")]
all.prod.methods.global <- merge(all.prod.methods.global.temp,all.prod.methods.fc,by=".id",
                                all=TRUE)

all.prod.methods.global[scenario=="sc1_total" , scenario := "Human Capital Scenario 1"]
all.prod.methods.global[scenario=="sc2_total" , scenario := "Human Capital Scenario 2"]
all.prod.methods.global[scenario=="fc_total" , scenario := "Friction Cost"]

#### radar chart 

all.prod.methods.global[, `Region and Method`:=paste(.id, "-", scenario)]

plt <- ggplot(all.prod.methods.global) +
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:3) * 1000),
    color = "lightgrey"
  ) + 
  # Add bars to represent the cumulative track lengths
  # str_wrap(region, 5) wraps the text so each line has at most 5 characters
  # (but it doesn't break long words!)
  geom_col(
    aes(
      x = reorder(str_wrap(`Region and Method`, 5), total),
      y = total,
      fill = scenario
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  
  # Add dots to represent the mean gain
  geom_point(
    aes(
      x = reorder(str_wrap(`Region and Method`, 5),fc_total),
      y = fc_total
    ),
    size = 3,
    color = "gray12"
  ) +
  xlab("Region and Scenario") +
  ylab("Total Labour Productivity Loss from Deaths (USD)") +
  scale_y_continuous(label= scales::comma)+
  # Make it circular!
  coord_polar()

plt

###############################################################
#### newer plots #####
#### code for plotting final outputs by Rhys Kingston 2023
### adapted for this repo


if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "tidyverse"
  , "readr"
  , "here"
  , "janitor"
  , "scales"
  , "rgdal"
  , "sf"
  , "countrycode"
  , "maps"
  , "rgeos"
  , "devtools"
  , "treemapify"
  , "ggbeeswarm"
  , "viridis"
  , "plotly"
)

########## loading data
load("outputs/regional_hospital_4additionalplots.RData") ## adding sc2 for productivity
load("outputs/prod_additional4plots_sc2.RData")
# load("outputs/prod_4additionalplots.RData")## adding sc2 for productivity

prod_deaths <- cleaning.dt.prod(all.prod.methods)

## function doesnt work for this hospital data as there is 
regional_hospital_totals <-    regional_hospital_totals[regional_hospital_totals$vaccine_id %in% keep_vac]
####!!! don't use for totals pre-vaccine as will have double counting for e.coli
### trying to account for this
### accounting for duplication of E. coli total amounts for ETEC & EXPEC
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         cost_med :=0]
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         cost_lo :=0]
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         cost_hi :=0]
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         days_med :=0]
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         days_lo :=0]
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         days_hi :=0]

regional_prod_deaths <- prod_deaths[, lapply(.SD, sum, na.rm=TRUE),
                                    by = c(".id","Pathogen","vaccine_id"),
                                    .SDcols=c("sc1_total","sc2_total",
                                              "sc1_averted","sc2_averted")]

### merge together
all.dt <- merge(regional_prod_deaths, regional_hospital_totals, by.x=c(".id","Pathogen","vaccine_id"),
                by.y=c("WHO.Region","Pathogen","vaccine_id"))
### !! note that the total costing set to 0 for ETEC is just because all e.coli is already costed (included diarrheoa)
## in the EXPEC - so applicable to both but removed to remove double counting

## sum across vaccines for pathogens
all.dt <- all.dt[, lapply(.SD, sum, na.rm=TRUE),
                 by = c("Pathogen",
                        ".id"),
                 .SDcols=c(    "sc1_total","sc2_total",
                               "sc1_averted","sc2_averted" ,         
                               "avert_cost_med"  ,"avert_cost_lo",
                               "avert_cost_hi" ,
                               "cost_med",  "cost_lo" ,
                               "cost_hi", "avert_days_med"    ,
                               "avert_days_lo"  ,   "avert_days_hi" ,
                               "days_med"  ,   "days_lo"   , 
                               "days_hi"  )]

all.dt.temp <- all.dt[,.(median_cost_sum_G=sum(cost_med)),
                      by=c("Pathogen")]

all.dt <- merge(all.dt, all.dt.temp, by=c("Pathogen"),
                allow.cartesian = TRUE)

all.dt.temp <- all.dt[,.(median_prod_cost_sum_G=sum(sc1_total)),
                      by=c("Pathogen")]

all.dt <- merge(all.dt, all.dt.temp, by=c("Pathogen"),
                allow.cartesian = TRUE)

all.dt[ , global_total := median_prod_cost_sum_G+median_cost_sum_G]

##### Line plot for hospital costs
options(scipen=10000) ## turn off scientific notation

# ######## bed days averted ##################
# all.dt %>%
#   ggplot() +
#   geom_pointrange(
#     mapping = aes(
#       y = fct_reorder(Pathogen, median_cost_sum_G)
#       , x = avert_days_med
#       , xmin = avert_days_lo
#       , xmax = avert_days_hi
#       , colour = .id
#     )
#     , position = position_dodge(width = 0.5)
#     , size = 0.7
#     , lwd = 0.7
#   ) +
#   scale_colour_brewer(palette = "Dark2") +
#   scale_x_log10(
#     labels = comma
#     , position = "top"
#   ) +
#   labs(
#     title = "Median Hospital Days averted due to vaccinations, by WHO region"
#     , subtitle = "Capacity in Days. Line ranges represent the IQR.\n"
#   ) +
#   theme_minimal() +
#   theme(
#     panel.grid.minor.x = element_blank()
#     , panel.grid.major.y = element_blank()
#     , axis.line.y = element_line(
#       colour = "black"
#       , linewidth = 1
#     )
#     , axis.text.y = element_text(
#       face = "bold.italic"
#       , family = "sans"
#       , size = 14
#       , colour = "black"
#     )
#     , axis.text.x.top = element_text(
#       family = "sans"
#       , size = 12
#       , margin = margin(
#         b = 10
#       )
#     )
#     , axis.title.y = element_blank()
#     , axis.title.x = element_blank()
#     , legend.position = c(0.20, 0.98)
#     , legend.title = element_blank()
#     , plot.title = element_text(
#       face = "bold"
#       , family = "sans"
#       , size = 20
#       , colour = "black"
#     )
#     , plot.subtitle = element_text(
#       face = "bold"
#       , family = "sans"
#       , size = 16
#       , colour = "grey50"
#     )
#   ) +
#   guides(
#     alpha = "none"
#     , colour = guide_legend(
#       override.aes = list(
#         size = 2
#       )
#       , nrow = 1))+
#   theme(panel.grid.major.y =element_line(colour="black")
#   )
# 
# ########### hospital cost averted ####################
# all.dt %>%
#   ggplot() +
#   geom_pointrange(
#     mapping = aes(
#       y = fct_reorder(Pathogen, median_cost_sum_G)
#       , x = avert_cost_med
#       , xmin = avert_cost_lo
#       , xmax = avert_cost_hi
#       , colour = .id
#     )
#     , position = position_dodge(width = 0.5)
#     , size = 0.7
#     , lwd = 0.7
#   ) +
#   scale_colour_viridis(discrete = TRUE) +
#   scale_x_log10(
#     labels = dollar
#     , position = "top"
#   ) +
#   labs(
#     title = "Median Hospital Costs averted due to vaccinations, by WHO region"
#     , subtitle = "Cost in 2019 USD. Line ranges represent the IQR.\n"
#   ) +
#   theme_minimal() +
#   theme(
#     panel.grid.minor.x = element_blank()
#     , panel.grid.major.y = element_blank()
#     , axis.line.y = element_line(
#       colour = "grey"
#       , linewidth = 1
#     )
#     , axis.text.y = element_text(
#       face = "bold.italic"
#       , family = "sans"
#       , size = 14
#       , colour = "black"
#     )
#     , axis.text.x.top = element_text(
#       family = "sans"
#       , size = 12
#       , margin = margin(
#         b = 10
#       )
#     )
#     , axis.title.y = element_blank()
#     , axis.title.x = element_blank()
#     , legend.position = c(0.20, 0.98)
#     , legend.title = element_blank()
#     , plot.title = element_text(
#       face = "bold"
#       , family = "sans"
#       , size = 18
#       , colour = "black"
#     )
#     , plot.subtitle = element_text(
#       face = "bold"
#       , family = "sans"
#       , size = 16
#       , colour = "grey50"
#     )
#   ) +
#   guides(
#     alpha = "none"
#     , colour = guide_legend(
#       override.aes = list(
#         size = 2
#       )
#       , nrow = 1)
#   )

########### total costs ####################

total.cost.plot <- function (all.dt){
  all.dt %>%
    ggplot(
      mapping = aes(
        x = cost_med
        , size = sc1_total
        , y = fct_reorder(Pathogen,global_total)
        , colour = .id
        , alpha = .8
      )
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_y_discrete(
      expand = c(
        0
        , 1
      )
    ) +
    geom_beeswarm(
      priority = "ascending"
      , cex = 2
    ) +
    scale_size_continuous(
      range = c(
        2
        , 30
      )
    ) +
    scale_x_log10(
      labels = scales::dollar_format(scale = 1e-6, suffix = "mn")
      , position = "top"
    ) +
    # labs(
    #   title = "Total Costs caused by DRIs, by WHO region"
    #   , subtitle = "Log scaled costs in 2019 USD. The y-axis represents hospital costs. Circles are sized by productivity loss due to DRI deaths, and colour indicates WHO region.\n"
    # ) +
    theme_minimal() +
    theme(
      axis.line.y = element_line(
        colour = "black"
        , linewidth = 1
      )
      , axis.text.y = element_text(
        face = "bold.italic"
        , family = "sans"
        , size = 12
        , colour = "black"
      )
      , axis.text.x.top = element_text(
        family = "sans"
        , size = 10.5
        , margin = margin(
          b = 5
        )
      )
      , axis.title.y = element_blank()
      , axis.title.x = element_blank()
      , legend.position = c(0.20, 0.98)
      , legend.title = element_blank()
      , plot.title = element_text(
        face = "bold"
        , family = "sans"
        , size = 20
        , colour = "black"
      )
      , plot.subtitle = element_text(
        face = "bold"
        , family = "sans"
        , size = 16
        , colour = "grey50"
      )
    ) +
    guides(
      alpha = "none"
      , size = "none"
      , colour = guide_legend(
        override.aes = list(
          size = 8
        )
        , nrow = 1) )+
  theme(panel.grid.major.y =element_line(colour="black"))
}


total.cost.plot(all.dt)


#### unit cost plotting code #####

#### running using global unit cost from combining_cost_cases_NEW

# ggplot(UNITcost_averted_global, aes(x=interaction(Exposure_Group), y=Median_unitcost, 
#                                     fill=Infectious.syndrome)) + 
#   geom_bar(position=position_dodge(), stat="identity",
#            colour="black", # Use black outlines,
#            size=.3) +      # Thinner lines
#   geom_errorbar(aes(ymin=LOWIQR_unitcost, ymax=HIGIQR_unitcost),
#                 size=.3,    # Thinner lines
#                 width=.2,
#                 position=position_dodge(.9)) +
#   xlab("Resistance & Bacterial Exposure") +
#   ylab("Hospital Cost per Case (2019 USD)") +
#   ggtitle("Global Averages") +
#   scale_fill_viridis_d()+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   coord_cartesian(ylim=c(NA, 30000), expand = FALSE)

#### log scale
# ggplot(UNITcost_averted_global, aes(x=interaction(Exposure_Group), y=Median_unitcost, 
#                                     fill=Infectious.syndrome)) + 
#   geom_bar(position=position_dodge(), stat="identity",
#            colour="black", # Use black outlines,
#            size=.3) +      # Thinner lines
#   geom_errorbar(aes(ymin=LOWIQR_unitcost, ymax=HIGIQR_unitcost),
#                 size=.3,    # Thinner lines
#                 width=.2,
#                 position=position_dodge(.9)) +
#   xlab("Resistance & Bacterial Exposure") +
#   ylab("Hospital Cost per Case (2019 USD)") +
#   ggtitle("Global Averages") +
#   scale_fill_viridis_d()+
#   scale_y_continuous(trans="log10")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### multi panel by gram stain
load("outputs/hospitalC_global_los_npop.RData")
### doesn't matter about vaccine scenarios for this

## remove TB !!!
UNITcost_averted_global <- as.data.table(hospital_c) 
UNITcost_averted_global <- UNITcost_averted_global[Infectious.syndrome !="TB"]
UNITcost_averted_global[ ,Exposure_Group := paste(class,Pathogen)]

## link gram stain
bug_dic <- read.csv("data_all/bug_gram.csv")

UNITcost_averted_global2 <- merge(UNITcost_averted_global,bug_dic, 
                                  by.x="Pathogen",
                                  by.y="bacteria",
                                  all.x=TRUE,
                                  all.y=FALSE)

### fill in by hand miss-matches
UNITcost_averted_global2[Pathogen=="Haemophilus influenzae"
                         |Pathogen=="Non-typhoidal Salmonella" 
                         |Pathogen=="Salmonella Paratyphi"  
                         |Pathogen=="Salmonella Typhi"
                         |Pathogen=="Shigella spp.", gram.stain:="gn"]  

UNITcost_averted_global2[gram.stain=="gn", gram.stain := "Gram-negative"]
UNITcost_averted_global2[gram.stain=="gp", gram.stain := "Gram-positive"]

## shorten some long exposure group names
UNITcost_averted_global2[Exposure_Group=="Fluoroquinolones or MDR in Salmonella Salmonella Paratyphi" ,
                         Exposure_Group:="Fluoroquinolones/MDR Salmonella Paratyphi" ]
UNITcost_averted_global2[Exposure_Group=="Fluoroquinolones or MDR in Salmonella Salmonella Typhi" ,
                         Exposure_Group:="Fluoroquinolones/MDR Salmonella Salmonella Typhi" ]
UNITcost_averted_global2[Exposure_Group=="Fluoroquinolones or MDR in Salmonella Non-typhoidal Salmonella",
                         Exposure_Group:= "Fluoroquinolones/MDR Non-typhoidal Salmonella" ]

ggplot(UNITcost_averted_global2, aes(x=interaction(Exposure_Group), y=Median_unitcost, 
                                    fill=Infectious.syndrome)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=LOWIQR_unitcost, ymax=HIGIQR_unitcost),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Resistance & Bacterial Exposure") +
  ylab("Hospital Cost per Case (2019 USD)") +
  scale_fill_brewer(palette = "Paired")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text = element_text(size = 15))+
  coord_cartesian(ylim=c(NA, 10000), expand = FALSE)+
  facet_wrap(~gram.stain, scales="free")




######## productivity cost averted ##################

### create min and max values
all.dt[sc1_averted<sc2_averted, min_prod := sc1_averted]
all.dt[sc2_averted<sc1_averted, min_prod := sc2_averted]
all.dt[sc1_averted<sc2_averted, max_prod := sc2_averted]
all.dt[sc2_averted<sc1_averted, max_prod := sc1_averted]

all.dt %>%
  ggplot() +
  geom_pointrange(
    mapping = aes(
      y = fct_reorder(Pathogen, median_cost_sum_G)
      , x = sc1_averted
      , xmin= min_prod
      , xmax= max_prod
      , colour = .id
    )
    , position = position_dodge(width = 0.5)
    , size = 0.7
    , lwd = 0.7
  ) +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_log10(
    labels = scales::dollar_format(scale = 1e-6, suffix = "mn")
    , position = "top"
  ) +
  # labs(
  #   title = "Productivity Losses Averted"
  #   , subtitle = "Point estimates are base case scenario. The line represents min and max values across scenarios \n"
  # ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank()
    , panel.grid.major.y = element_blank()
    , axis.line.y = element_line(
      colour = "black"
      , linewidth = 1
    )
    , axis.text.y = element_text(
      face = "bold.italic"
      , family = "sans"
      , size = 14
      , colour = "black"
    )
    , axis.text.x.top = element_text(
      family = "sans"
      , size = 12
      , margin = margin(
        b = 10
      )
    )
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    , legend.position = c(0.20, 0.98)
    , legend.title = element_blank()
    , plot.title = element_text(
      face = "bold"
      , family = "sans"
      , size = 20
      , colour = "black"
    )
    , plot.subtitle = element_text(
      face = "bold"
      , family = "sans"
      , size = 16
      , colour = "grey50"
    )
  ) +
  guides(
    alpha = "none"
    , colour = guide_legend(
      override.aes = list(
        size = 2
      )
      , nrow = 1))+
  theme(panel.grid.major.y =element_line(colour="black"),
        plot.margin=margin(10,50,10,10)
  )

##### WLYL####
load("outputs/working_life_years_lost.Rdata")

output_hc <- as.data.table(output_hc)
### getting one vaccine/one DRI per group
output_hc <- cleaning.dt(output_hc)

regional_prod_path  <-output_hc[, lapply(.SD, sum, na.rm=TRUE),
                                  by = c("Pathogen",
                                         ".id"),
                                  .SDcols=c("averted_HC")]
regional_prod_path[.id=="PAHO",.id := "AMRO"]

all.dt2 <- merge(regional_prod_path,all.dt, by=c("Pathogen",".id"))

all.dt2 %>%
  ggplot() +
  geom_point(
    mapping = aes(
      y = fct_reorder(Pathogen, median_cost_sum_G)
      , x = averted_HC
      , colour = .id
    )
    , position = position_dodge(width = 0.5)
    , size = 7
    , lwd = 0.7
  ) +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_log10(
    labels = unit_format(unit = "mn", scale = 1e-6)
    , position = "top"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank()
    , panel.grid.major.y = element_blank()
    , axis.line.y = element_line(
      colour = "black"
    )
    , axis.text.y = element_text(
      face = "bold.italic"
      , family = "sans"
      , size = 14
      , colour = "black"
    )
    , axis.text.x.top = element_text(
      family = "sans"
      , size = 12
      , margin = margin(
        b = 10
      )
    )
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    , legend.position = c(0.20, 0.98)
    , legend.title = element_blank()
    , plot.title = element_text(
      face = "bold"
      , family = "sans"
      , size = 20
      , colour = "black"
    )
    , plot.subtitle = element_text(
      face = "bold"
      , family = "sans"
      , size = 16
      , colour = "grey50"
    )
  ) +
  guides(
    alpha = "none"
    , colour = guide_legend(
      override.aes = list(
        size = 7
      )
      , nrow = 1))+
  theme(panel.grid.major.y =element_line(colour="black"),
        plot.margin=margin(10,50,10,10)
  )

##### output table for total impacts pre and post vaccine ####

load("outputs/hospitalC_global_averted_pathogen.RData")

hospital_c <-    hospital_c[hospital_c$vaccine_id %in% keep_vac]
####!!! don't use for totals pre-vaccine as will have double counting for e.coli
### trying to account for this
### accounting for duplication of E. coli total amounts for ETEC & EXPEC
hospital_c[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         cost_med :=0]
hospital_c[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         cost_lo :=0]
hospital_c[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         cost_hi :=0]
hospital_c[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         days_med :=0]
hospital_c[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         days_lo :=0]
hospital_c[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         days_hi :=0]


#### Global productivity 

load("outputs/prod_additional4plots_sc2.RData")

### change vaccine ids for those we want to combine
all.prod.methods[vaccine_id=="Haemophilus influenzae type B_0.93_0.9_5 years_All_6, 10, 14 weeks"|
                   vaccine_id=="Haemophilus influenzae type B_0.69_0.9_5 years_All_6, 10, 14 weeks" ,
                 vaccine_id := "Haemophilus influenzae type B_both_0.9_5 years_All_6, 10, 14 weeks" ]

all.prod.methods[vaccine_id=="Streptococcus pneumoniae - Improved_0.7_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6 weeks & elderly age group"|
                   vaccine_id=="Streptococcus pneumoniae - Improved_0.5_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6 weeks & elderly age group",
                 vaccine_id := "Streptococcus pneumoniae - Improved_both_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6 weeks & elderly age group"]

global_prod_deaths <- all.prod.methods[, lapply(.SD, sum, na.rm=TRUE),
                                  by = c("Pathogen","vaccine_id"),
                                  .SDcols=c("sc1_averted",
                                            "sc1_total" ,
                                            "sc2_averted" ,
                                            "sc2_total" )]

all_results_vac <- merge(global_prod_deaths, hospital_c,
                         by="vaccine_id")

write.csv(all_results_vac, file="outputs/all_results_vac.csv")

### regional pathogen ranking
pathrank <- all.prod.methods[, lapply(.SD, sum, na.rm=TRUE),
                         by=c("vaccine_id",".id"),
                         .SDcols=c("sc1_total")]
pathrank <- split(pathrank,by=".id")
af <- pathrank$AFRO
am <- pathrank$AMRO
em <- pathrank$EMRO
eu <- pathrank$EURO
sea <- pathrank$SEARO
wp <- pathrank$WPRO