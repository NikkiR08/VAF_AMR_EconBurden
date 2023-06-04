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

## getting the different scenarios down
load("data_inputs/epi_inputs_baseline.RData")

baseline <- baseline %>%
  unite("vaccine_id",vaccine:target_population) %>%
  as.data.table()
baseline_vac <- unique(baseline$vaccine_id)

########### !!! need to keep to just these for those with multiple- change if want different vaccine profiles
# "Acinetobacter baumannii - all_0.7_0.7_5 years_All_6 weeks & elderly age group"  
# "Haemophilus influenzae type B_0.93_0.9_5 years_All_6, 10, 14 weeks" 
# "Klebsiella pneumoniae - all_0.7_0.7_5 years_All_6 weeks & elderly age group"
# "Mycobacterium tuberculosis - Improved_0.8_0.7_10 years_All_0 weeks + boost every 10 years"   
# "Streptococcus pneumoniae - Improved_0.7_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6 weeks"
#### !!! I noticed ones with lower e.g. coverage had higher case values .. so switched to keeping these..
# "Haemophilus influenzae type B_0.69_0.9_5 years_All_6, 10, 14 weeks"
# "Streptococcus pneumoniae - Improved_0.5_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6 weeks"


##### remove these ones 
rem_vac <- c("Acinetobacter baumannii - BSI_0.7_0.7_5 years_BSI_6 weeks & elderly age group",
             "Haemophilus influenzae type B_0.93_0.9_5 years_All_6, 10, 14 weeks",
             "Klebsiella pneumoniae - BSI_0.7_0.7_6 months_BSI_0 weeks (maternal)",
             "Mycobacterium tuberculosis - M72_0.5_0.7_10 years_All_10 years + boost every 10 years",
             "Streptococcus pneumoniae_0.58_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6, 10, 14 weeks",  
             "Streptococcus pneumoniae_0.27_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6, 10, 14 weeks",
             "Streptococcus pneumoniae - Improved_0.7_0.9_5 years_BSI, CNS infections, Cardiac infections, LRI_6 weeks",
             "ExPEC - BSI_0.7_0.7_5 years_BSI_6 weeks & elderly age group" ,                                            
             "ExPEC - UTI_0.7_0.7_5 years_UTI_6 weeks & elderly age group" )

rm(baseline)

cleaning.dt <- function(hospital_region_averted){
  global_hospital <- hospital_region_averted[hospital_region_averted$vaccine_id %in% baseline_vac,]
  global_hospital <- global_hospital[global_hospital$vaccine_id %!in% rem_vac]
  
  #### testing to see which ones have multiple vaccines + differing base values for DRIs
  ### this is to avoid double counting totals/total averted by pathogen etc.
  #  testing <- global_hospital[ ,c("Pathogen","class","Infectious.syndrome","Median_total_costing")]
  #  testing.2 <- unique(testing)
  #  testing.3 <- testing.2 %>% group_by(Pathogen, class, Infectious.syndrome) %>%
  #       filter(n() > 1)
  # duplicated(testing)
  
  ### this highlighted E. coli, EXPEC and ETEC with different targets
  ## but ETEC only GI 
  global_hospital[ , flag := 0]
  global_hospital[(vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months" & 
                     Infectious.syndrome!="Diarrhoea")|(
                       vaccine_id=="E. coli - non-diarrhogenic_0.7_0.7_5 years_All except Diarrhoea_6 weeks & elderly age group"& 
                         Infectious.syndrome=="Diarrhoea"),
                  flag := 1]
  global_hospital_totals <- global_hospital[flag==0]
  
  
  return(global_hospital_totals)
}
###### ******global totals ********#####################################

#### HOSPITAL COSTS ##########################
load("outputs/hospitalC_global_averted.RData")

global_hospital_totals <- cleaning.dt(hospital_region_averted)
global_hospital_totals[class=="Fluoroquinolones or MDR in Salmonella",Antibiotic.class :="any"]
global_hospital_totals[Infectious.syndrome=="BSI", Infectious.syndrome := "Bloodstream infections"]
global_hospital_totals[Infectious.syndrome=="UTI", Infectious.syndrome := "Urinary tract infections"]
global_hospital_totals[Antibiotic.class=="Methicillin", Antibiotic.class := "Penicillin"]
global_hospital_totals[Infectious.syndrome=="TB", Infectious.syndrome := "Tuberculosis"]

 ## billions
(global_hospital_totals[, lapply(.SD, sum, na.rm=TRUE),
         .SDcols=c("Median_total_costing",
                   "Median_avert_costing")])*1/1e9


classrank <- (global_hospital_totals[, lapply(.SD, sum, na.rm=TRUE),
                      by="class",
                      .SDcols=c("Median_total_costing",
                                "Median_avert_costing")])

global_hospital_totals <- setorder(global_hospital_totals, Median_total_costing)
global_hospital_totals$Pathogen <- factor(global_hospital_totals$Pathogen , 
                                   levels = 
                                     unique(global_hospital_totals$Pathogen[order(global_hospital_totals$Median_total_costing)]))

ggplot(global_hospital_totals, aes(reorder(Pathogen,Median_total_costing), Median_total_costing,
                            fill=class)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = -90))+
  scale_y_continuous(label= scales::comma)+
  xlab("Pathogen")+
  ylab("Total Hospital Costs (USD)")+
  labs(fill="Antibiotic Class")

ggplot(global_hospital_totals, aes(reorder(Pathogen,Median_total_costing), Median_total_costing,
                            fill=Infectious.syndrome)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = -90))+
  scale_y_continuous(label= scales::comma)+
  xlab("Pathogen")+
  ylab("Total Hospital Costs (USD)")+
  labs(fill="Syndrome")

############## getting a global median and IQR
load("outputs/total_global_cost.RData")

global_hospital <- hospital_region_averted[hospital_region_averted$vaccine_id %in% baseline_vac,]
global_hospital <- global_hospital[global_hospital$vaccine_id %!in% rem_vac]
global_hospital[ , flag := 0]
global_hospital[(vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months" & 
                   Infectious.syndrome!="Diarrhoea")|(
                     vaccine_id=="E. coli - non-diarrhogenic_0.7_0.7_5 years_All except Diarrhoea_6 weeks & elderly age group"& 
                       Infectious.syndrome=="Diarrhoea"),
                flag := 1]
global_hospital_totals <- global_hospital[flag==0]

global_hospital_totals <- global_hospital_totals %>% 
  group_by(run) %>%
  summarise(cost = sum(hospital_cost, na.rm=TRUE),
            days = sum(hospital_days, na.rm=TRUE),
            cost_avert = sum(avert_hospital_cost,na.rm=TRUE),
            days_avert = sum(hospital_days,na.rm=TRUE)) %>%
  summarise(Median_avert_costing = median(cost_avert, na.rm=TRUE),
            LOWIQR_avert_costing = quantile(cost_avert,0.25, na.rm = TRUE),
            HIGIQR_avert_costing = quantile(cost_avert,0.75, na.rm = TRUE),
            Median_total_costing = median(cost, na.rm=TRUE),
            LOWIQR_total_costing = quantile(cost,0.25, na.rm = TRUE),
            HIGHIQR_total_costing = quantile(cost,0.75, na.rm = TRUE),
            Median_avert_days = median(days_avert, na.rm=TRUE),
            LOWIQR_avert_days = quantile(days_avert,0.25, na.rm = TRUE),
            HIGIQR_avert_days = quantile(days_avert,0.75, na.rm = TRUE),
            Median_total_days = median(days, na.rm=TRUE),
            LOWIQR_total_days = quantile(days,0.25, na.rm = TRUE),
            HIGHIQR_total_days = quantile(days,0.75, na.rm = TRUE))%>%
  as.data.table()

global_hospital_totals <- global_hospital_totals/1e9

print(global_hospital_totals)
#### Global productivity ##########################

prod_deaths <- read.csv("outputs/productivity_loss_deaths_all.csv")
prod_deaths <- as.data.table(prod_deaths)
### getting one vaccine/one DRI per group
prod_deaths <- prod_deaths[prod_deaths $vaccine_id %in% baseline_vac,]
prod_deaths  <- prod_deaths [prod_deaths $vaccine_id %!in% rem_vac]
prod_deaths[ , flag := 0]
prod_deaths[(vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months" & 
                   Infectious.syndrome!="Diarrhoea")|(
                     vaccine_id=="E. coli - non-diarrhogenic_0.7_0.7_5 years_All except Diarrhoea_6 weeks & elderly age group"& 
                       Infectious.syndrome=="Diarrhoea"),
                flag := 1]
prod_deaths <- prod_deaths[flag==0]


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

ggplot(global_prod_deaths, aes(reorder(Pathogen,HC_cost), HC_cost,
                               fill=Antibiotic.class)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = -90))+
  scale_y_continuous(label= scales::comma)+
  xlab("Pathogen")+
  ylab("Total Productivity Losses (USD)")+
  labs(fill='Antibiotic Class') 

ggplot(global_prod_deaths, aes(reorder(Pathogen,HC_cost), HC_cost,
                               fill=Infectious.syndrome)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = -90))+
  scale_y_continuous(label= scales::comma)+
  xlab("Pathogen")+
  ylab("Total Productivity Losses (USD)")+
  labs(fil='Syndrome')

############******* REGIONAL PLOTS******* ################

########## HOSPITAL ########################
load("outputs/hospitalC_region_averted.RData")

regional_hospital_totals <- cleaning.dt(hospital_region_averted)

## regional
totals.temp <- (regional_hospital_totals[, lapply(.SD, sum, na.rm=TRUE),
 by="WHO.Region",
 .SDcols=c("Median_total_costing",
           "Median_avert_costing")])

write.csv(totals.temp,file="outputs/regional_median_HC.csv")
rm(totals.temp)

####################### by pathogen - averted hospital costs

load("outputs/hospitalC_region_averted_pathogen.RData")

regional_hospital_totals <-  hospital_region_averted[hospital_region_averted$vaccine_id %in% baseline_vac,]
regional_hospital_totals <- regional_hospital_totals[regional_hospital_totals$vaccine_id %!in% rem_vac]
####!!! don't use for totals pre-vaccine as will have double counting for e.coli

save(regional_hospital_totals, file="outputs/regional_hospital_4additionalplots.RData")

total_averted <- regional_hospital_totals[, lapply(.SD, sum, na.rm=TRUE),
                                           by = c("Pathogen"),
                                           .SDcols=c("Median_avert_costing")]

total_averted_los <-  regional_hospital_totals[, lapply(.SD, sum, na.rm=TRUE),
                                              by = c("Pathogen"),
                                              .SDcols=c("Median_avert_days")]

total_averted <- merge(total_averted, total_averted_los, by="Pathogen")

ggplot(regional_hospital_totals, aes(x = Pathogen, y = Median_avert_costing,
                                              fill=WHO.Region)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = LOWIQR_avert_costing, ymax = HIGIQR_avert_costing),
                position = position_dodge(0.9), width = .2)+
  xlab("Resistance Exposure") +
  ylab("Averted Hospital Costs (2019 USD)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(label= scales::comma)+
  labs(fill="WHO Region")


ggplot(regional_hospital_totals, aes(x = Pathogen, y = Median_avert_days,
                                                fill=WHO.Region)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = LOWIQR_avert_days, ymax = HIGIQR_avert_days),
                position = position_dodge(0.9), width = .2)+
  xlab("Pathogen") +
  ylab("Averted Hospital Bed Days") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(label= scales::comma)+
  labs(fill="WHO Region")


#### regional productivity loss #######

prod_deaths <- read.csv("outputs/productivity_loss_deaths_all.csv")
prod_deaths <- as.data.table(prod_deaths)
### getting one vaccine/one DRI per group
prod_deaths <- prod_deaths[prod_deaths $vaccine_id %in% baseline_vac,]
prod_deaths  <- prod_deaths [prod_deaths $vaccine_id %!in% rem_vac]
prod_deaths[ , flag := 0]
prod_deaths[(vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months" & 
               Infectious.syndrome!="Diarrhoea")|(
                 vaccine_id=="E. coli - non-diarrhogenic_0.7_0.7_5 years_All except Diarrhoea_6 weeks & elderly age group"& 
                   Infectious.syndrome=="Diarrhoea"),
            flag := 1]
prod_deaths <- prod_deaths[flag==0]


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
output_hc <- output_hc[output_hc $vaccine_id %in% baseline_vac,]
output_hc  <- output_hc [output_hc $vaccine_id %!in% rem_vac]
output_hc[ , flag := 0]
output_hc[(vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months" & 
               Infectious.syndrome!="Diarrhoea")|(
                 vaccine_id=="E. coli - non-diarrhogenic_0.7_0.7_5 years_All except Diarrhoea_6 weeks & elderly age group"& 
                   Infectious.syndrome=="Diarrhoea"),
            flag := 1]
output_hc <- output_hc[flag==0]


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

global_hospital_totals <- cleaning.dt(hospital_region_averted)
global_hospital_totals[class=="Fluoroquinolones or MDR in Salmonella",Antibiotic.class :="any"]
global_hospital_totals[Infectious.syndrome=="BSI", Infectious.syndrome := "Bloodstream infections"]
global_hospital_totals[Infectious.syndrome=="UTI", Infectious.syndrome := "Urinary tract infections"]
global_hospital_totals[Antibiotic.class=="Methicillin", Antibiotic.class := "Penicillin"]
global_hospital_totals[Infectious.syndrome=="TB", Infectious.syndrome := "Tuberculosis"]


vaccine_hospital <- global_hospital_totals[, lapply(.SD, sum, na.rm=TRUE),
                                        by = c("vaccine_id"),
                                        .SDcols=c("Median_avert_costing")]

prod_deaths <- read.csv("outputs/productivity_loss_deaths_all.csv")
prod_deaths <- as.data.table(prod_deaths)
### getting one vaccine/one DRI per group
prod_deaths <- prod_deaths[prod_deaths $vaccine_id %in% baseline_vac,]
prod_deaths  <- prod_deaths [prod_deaths $vaccine_id %!in% rem_vac]
prod_deaths[ , flag := 0]
prod_deaths[(vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months" & 
               Infectious.syndrome!="Diarrhoea")|(
                 vaccine_id=="E. coli - non-diarrhogenic_0.7_0.7_5 years_All except Diarrhoea_6 weeks & elderly age group"& 
                   Infectious.syndrome=="Diarrhoea"),
            flag := 1]
prod_deaths <- prod_deaths[flag==0]
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

totalcostingtemp[ ,total.cost := Median_avert_costing+averted_HC]

write.csv(totalcostingtemp, file="outputs/totalcosting.csv")

#### regional comparing regional friction cost to human capital #########

fc.prod <- read.csv("outputs/vaccine_avertable_FC.csv")

fc.prod <- as.data.table(fc.prod)
### getting one vaccine/one DRI per group
fc.prod <- fc.prod[fc.prod $vaccine_id %in% baseline_vac,]
fc.prod  <- fc.prod [fc.prod $vaccine_id %!in% rem_vac]
fc.prod[ , flag := 0]
fc.prod[(vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months" & 
               Infectious.syndrome!="Diarrhoea")|(
                 vaccine_id=="E. coli - non-diarrhogenic_0.7_0.7_5 years_All except Diarrhoea_6 weeks & elderly age group"& 
                   Infectious.syndrome=="Diarrhoea"),
            flag := 1]
fc.prod <- fc.prod[flag==0]


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
### getting one vaccine/one DRI per group
sc2.prod <- sc2.prod[sc2.prod $vaccine_id %in% baseline_vac,]
sc2.prod  <- sc2.prod [sc2.prod $vaccine_id %!in% rem_vac]
sc2.prod[ , flag := 0]
sc2.prod[(vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months" & 
           Infectious.syndrome!="Diarrhoea")|(
             vaccine_id=="E. coli - non-diarrhogenic_0.7_0.7_5 years_All except Diarrhoea_6 weeks & elderly age group"& 
               Infectious.syndrome=="Diarrhoea"),
        flag := 1]
sc2.prod <- sc2.prod[flag==0]


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
sc1.prod <- prod_deaths
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

