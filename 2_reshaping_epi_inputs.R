### reshaping new epi data to format to input in model####



#### loading packages
library(tidyr)
library(data.table)
library(dplyr)

cases <- read.csv("data_inputs/case_deaths_impact_by_vaccine.csv")
cases <- as.data.table(cases)

cases[ , hflag := "YES"]
cases[VO==VC, hflag := "NO"]

baseline <- cases[ , -c("VO", "vaccine_avertable_cases_resistance_high" ,
                        "vaccine_avertable_cases_susceptible_high" ,
                        "vaccine_avertable_deaths_resistant_high"   ,
                        "vaccine_avertable_deaths_susceptible_high",
                        "impact_ratio_high")]

names(baseline)[names(baseline) == 'VC'] <- 'target_population'
colnames(baseline) <- gsub('_base','',colnames(baseline))

high_scenario <- cases[ , -c("VC", "vaccine_avertable_cases_resistance_base" ,
                             "vaccine_avertable_cases_susceptible_base" ,
                             "vaccine_avertable_deaths_resistant_base"  ,
                             "vaccine_avertable_deaths_susceptible_base",
                        "impact_ratio_base")]
high_scenario <- high_scenario[hflag=="YES"]

names(high_scenario)[names(high_scenario) == 'VO'] <- 'target_population'
colnames(high_scenario) <- gsub('_high','',colnames(high_scenario))

cases2 <- read.csv("data_inputs/case_deaths_impact_by_vaccine_with_current_coverage.csv")
cases2$VC==cases2$VO
### chaelin stated that VC = base and this only has baseline results so assuming there is no high coverage scenario
cases2 <- as.data.table(cases2)
current_scenario <- cases2[ , -c("VO")]

names(current_scenario)[names(current_scenario) == 'VC'] <- 'target_population'
colnames(current_scenario) <- gsub('_base','',colnames(current_scenario))

## remove hflag from previous
baseline <- baseline[ ,-c("hflag")]
high_scenario <- high_scenario[ , -c("hflag")]


save(baseline, file="data_inputs/epi_inputs_baseline.RData")
save(high_scenario, file="data_inputs/epi_inputs_high_scenario.RData")
save(current_scenario, file="data_inputs/epi_inputs_current_scenario.RData")


all_data <- rbind(baseline, high_scenario)
all_data <- rbind(all_data, current_scenario)

### seems to be duplicates
dups <- all_data[duplicated(all_data)]
unique(dups$vaccine_avertable_cases_resistance)
### seems to be where there is no vaccine impact & where baseline repeated (when no high scenario)

## remove duplicates
all_data <- unique(all_data)

save(all_data, file="data_inputs/epi_inputs_all.RData")

############### seeing which are missing from cost data
## data
load("data_inputs/epi_inputs_all.RData") ## cases
cases <- all_data
rm(all_data)
cost.results <- read.csv("data_inputs/lit_review/manuscript_results_cost.csv")

## some data manip - by hand to compare to costs
cases[Antibiotic.class=="Carbapenems", Antibiotic.class:="carbapenems"]    
cases[Antibiotic.class=="Third-generation cephalosporins", 
      Antibiotic.class:="3g cephalosporins"]
cases[Antibiotic.class=="Vancomycin", 
      Antibiotic.class:="glycopeptides"]
cases[Antibiotic.class=="Methicillin"|
        Antibiotic.class=="Penicillin" , 
      Antibiotic.class:="penicillins"]
cases[Antibiotic.class=="Multi-drug resistance excluding extensive drug resistance in TB" , 
      Antibiotic.class:="mdr"]

cases[Infectious.syndrome=="LRI and thorax infections"|
        Infectious.syndrome=="TB", Infectious.syndrome:="RTI"]
cases[Infectious.syndrome=="Bacterial skin infections", Infectious.syndrome:="SSTI"]
cases[Infectious.syndrome=="Intra-abdominal infections", Infectious.syndrome:="IAI"]
cases[Infectious.syndrome=="Bone and joint infections", Infectious.syndrome:="B-J"]

## in future iteration try match to bug dic version?
##  for now just formatting cases by hand
cases[Pathogen=="Acinetobacter baumannii"|
        Pathogen=="Escherichia coli"|
        Pathogen=="Klebsiella pneumoniae"|
        Pathogen=="Pseudomonas aeruginosa"|
        Pathogen=="Non-typhoidal Salmonella" |
        Pathogen=="Salmonella Typhi" |
        Pathogen=="Salmonella Paratyphi"|
        Pathogen== "Shigella spp"|
        Pathogen== "Haemophilus influenzae" , gram.stain:="gn"]

cases[Pathogen=="Streptococcus pneumoniae" |
        Pathogen=="Enterococcus faecium"|
        Pathogen=="Staphylococcus aureus"|
        Pathogen=="Group A Streptococcus", gram.stain:="gp"]

cases[Pathogen=="Mycobacterium tuberculosis", gram.stain :="tb"]


combo_cases <- unique(cases[,c('Infectious.syndrome',
                               'gram.stain','Antibiotic.class')])
combo_cases$case_data <- "yes"

combo_costs <-  unique(cost.results[,c('gram.stain',
                                       'class','syndrome')])
combo_costs$cost_data <- "yes"

combo_both <- merge(combo_cases, combo_costs, 
                    by.x=c("gram.stain","Infectious.syndrome",
                           "Antibiotic.class"),
                    by.y=c("gram.stain","syndrome","class"),
                    all=TRUE)

### data complete
combo_both_comp <- combo_both[case_data=="yes" & 
                                cost_data=="yes"]
combo_both_nocost <- combo_both[case_data=="yes" & 
                                  is.na(cost_data)]

#### same for antibiotic class & gram stain ################

combo_cases <- unique(cases[,c('gram.stain','Antibiotic.class')])
combo_cases$case_data <- "yes"

combo_costs <-  unique(cost.results[,c( 'gram.stain','class')])
combo_costs$cost_data <- "yes"

combo_both <- merge(combo_cases, combo_costs, 
                    by.x=c("gram.stain",
                           "Antibiotic.class"),
                    by.y=c("gram.stain","class"),
                    all=TRUE)

### data complete
combo_both_comp <- combo_both[case_data=="yes" & 
                                cost_data=="yes"]
combo_both_nocost <- combo_both[case_data=="yes" & 
                                  is.na(cost_data)]
