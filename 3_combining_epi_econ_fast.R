
########### updated combining code 13/08

### now removed age groups - sum at the start instead of the end

#### loading packages
library(tidyr)
library(data.table)
library(dplyr)
library(stringr)

## loading data
#### clearing and reloading due to memory allocation issues & set up quickly due to time limitations
#### if memory is not an issue worth taking loading out of the loop


load("outputs/DRI_UC_los_trunc.RData") ## cost sample
load("data_inputs/epi_inputs_all.RData") ## cases
cases <- all_data
rm(all_data)

### add up over age groups to speed up
cases <- cases %>%
  group_by(vaccine,Efficacy,Coverage,
  Duration,DP,target_population,WHO.Region,
  ISO3, Country,
  Infectious.syndrome, Origin,
  Pathogen, Antibiotic.class) %>%
  summarise(cases_resistant=sum(cases_resistant),
            deaths_resistant=sum(deaths_resistant) ,
            vaccine_avertable_cases_resistance=sum(vaccine_avertable_cases_resistance), 
          vaccine_avertable_deaths_resistant=sum(vaccine_avertable_deaths_resistant))

## randomly select 1000 los costs for those where there are over 1000 samples (due to them being in e.g. DRI direct estimates and also adjust)
hospital.prop <- read.csv("data_inputs/hospitalisation_proportion.csv") ## proportion hospitalised

load("data_inputs/lit_review/who_whoc_wb.RData") ## region dictionary

#### formatting datasets ####
colnames(cases)
## creating vaccine scenario ID
### !! note would need to change if col order changes
cases <- cases %>%
  unite("vaccine_id",vaccine:target_population) %>%
  as.data.table()

### splitting back out the group string variable
DRI.all[, c("iso3c","whoc.region" ,"syndrome"  ,
            "class", "gram.stain") := tstrsplit(group_id_c, "_", fixed=TRUE)]

unique(cases$Pathogen)
cases[Pathogen=="Acinetobacter baumannii"|
        Pathogen=="Escherichia coli"|
        Pathogen=="Klebsiella pneumoniae"|
        Pathogen=="Pseudomonas aeruginosa"|
        Pathogen=="Neisseria gonorrhoeae"|
        Pathogen=="Shigella spp."|  
        Pathogen=="Neisseria gonorrhoeae", gram.stain:="gn"]

cases[Pathogen=="Streptococcus pneumoniae" |
        Pathogen=="Enterococcus faecium"|
        Pathogen=="Staphylococcus aureus", gram.stain:="gp"]

cases[Pathogen=="Mycobacterium tuberculosis", gram.stain:="tb"]

### adding in expert opinion grams (to match to LOS estimates)
cases[Pathogen=="Non-typhoidal Salmonella", 
      gram.stain:="Non-typhoidal Salmonella"]
cases[Pathogen=="Salmonella Typhi", 
      gram.stain:="Salmonella Typhi"]
cases[Pathogen=="Salmonella Paratyphi", 
      gram.stain:="Salmonella Paratyphi"]
cases[Pathogen=="Haemophilus influenzae", 
      gram.stain:="Haemophilus influenzae"]
cases[Pathogen=="Group A Streptococcus", 
      gram.stain:="Group A Streptococcus"]

cases[is.na(gram.stain)] ##check - empty

unique(cases$Infectious.syndrome)

cases[, syndrome:=Infectious.syndrome]
cases[Infectious.syndrome=="LRI and thorax infections",syndrome:="RTI"]
cases[Infectious.syndrome=="TB", syndrome:="RTI_TB"] ## separating out due to large cost difference
cases[Infectious.syndrome=="Bacterial skin infections", syndrome:="SSTI"]
cases[Infectious.syndrome=="Intra-abdominal infections", syndrome:="IAI"]
cases[Infectious.syndrome=="Bone and joint infections", syndrome:="B-J"]
cases[Infectious.syndrome=="Diarrhoea", syndrome:="GI"]
cases[Infectious.syndrome=="Cardiac infections", syndrome:="CARDIAC"]
cases[Infectious.syndrome=="CNS infections", syndrome:="CNS"]
cases[Infectious.syndrome=="Typhoid, paratyphoid, and iNTS", syndrome := "INF"] ## to match LOS

cases[Infectious.syndrome=="TB", syndrome:="RTI_TB"] ## separating out due to large cost difference
DRI.all[gram.stain=="tb", syndrome :="RTI_TB"]

cases[Pathogen=="Non-typhoidal Salmonella"|
        Pathogen=="Salmonella Typhi"|
        Pathogen=="Salmonella Paratyphi"|
        Pathogen=="Haemophilus influenzae"|
        Pathogen=="Group A Streptococcus", syndrome := "INF"] ## to match to "other los" which was all salmonella infections

cases[Antibiotic.class=="Carbapenems", class:="carbapenems"]    
cases[Antibiotic.class=="Third-generation cephalosporins", 
      class:="3g cephalosporins"]
cases[Antibiotic.class=="Vancomycin", 
      class:="glycopeptides"]
cases[Antibiotic.class=="Methicillin"|
        Antibiotic.class=="Penicillin" , 
      class:="penicillins"]
cases[Antibiotic.class=="Multi-drug resistance excluding extensive drug resistance in TB" , 
      class:="mdr"]
cases[Antibiotic.class=="Fluoroquinolones", 
      class:="fluoroquinolones"]
cases[Antibiotic.class=="Multi-drug resistance in Salmonella Typhi and Paratyphi" , 
      class:="all"] ## to match LOS
cases[Antibiotic.class=="Fluoroquinolones" & gram.stain=="Non-typhoidal Salmonella" , 
      class:="all"]
cases[Antibiotic.class=="Fluoroquinolones" & gram.stain=="Salmonella Typhi" , 
      class:="all"]
cases[Antibiotic.class=="Fluoroquinolones" & gram.stain=="Salmonella Paratyphi" , 
      class:="all"]
cases[Antibiotic.class=="Macrolide", 
      class:="macrolides"]

## due to "other los"
cases[Pathogen=="Non-typhoidal Salmonella"|
        Pathogen=="Salmonella Typhi"|
        Pathogen=="Salmonella Paratyphi"|
        Pathogen=="Haemophilus influenzae"|
        Pathogen=="Group A Streptococcus", class := "all"]

cases[WHO.Region=="Western Pacific Region" ,WHO.Region := "WPRO"]
cases[WHO.Region=="South-East Asia Region" ,WHO.Region := "SEARO"]
cases[WHO.Region=="European Region" ,WHO.Region := "EURO"]
cases[WHO.Region=="Region of the Americas" ,WHO.Region := "PAHO"]
cases[WHO.Region=="African Region" ,WHO.Region := "AFRO"]
cases[WHO.Region=="Eastern Mediterranean Region" ,WHO.Region := "EMRO"]

t1 <- unique(cases$syndrome)
t2 <- unique(DRI.all$syndrome)
setdiff(t1,t2)

setnames(cases, "ISO3", "iso3c")

### keep only columns we want
cases_averted <- cases[,c( "vaccine_avertable_cases_resistance",
                          "cases_resistant",
                          "Antibiotic.class","Infectious.syndrome",
                          "gram.stain",                         
                          "syndrome" ,                           
                          "class","iso3c","Pathogen" ,"WHO.Region",
                          "vaccine_id","Origin")]   
rm(cases)
gc()

### accounting for hospitalized
hospital.prop <- hospital.prop[, c("Region","Syndrome",
                                   "med")]
setnames(hospital.prop,"Syndrome","syndrome")
setnames(hospital.prop,"Region","WHO.Region")
setnames(hospital.prop,"med","prop_hospitalised")

hospital.prop <- as.data.table(hospital.prop)

keycols <- c("syndrome","WHO.Region")
setkeyv(hospital.prop, keycols)
setkeyv(cases_averted, keycols)

cases_averted <- hospital.prop[cases_averted, on=keycols]

cases_averted[ , prop_hospitalised := as.numeric(prop_hospitalised)/100]
cases_averted[ , cases_averted_hospitalised := vaccine_avertable_cases_resistance*prop_hospitalised]
cases_averted[ , cases_hospitalised := cases_resistant*prop_hospitalised]

rm(hospital.prop)

### if want to remove NA values (those with no cases_averted or proportional of hospitalisation data)
# cases_averted <- cases_averted[(!is.na(cases_averted_hospitalised)&
#             cases_averted_hospitalised>0)]

cases_averted[is.na(cases_averted_hospitalised), 
              cases_averted_hospitalised := 0]

keycols <- c("iso3c","gram.stain",
             "class","syndrome")

#### subset cases_averted by WHO. region to run the code
load("outputs/sample_whoc.RData")
load("outputs/sample_whoc_region.RData")

vaccines <- unique(cases_averted$vaccine_id)
vaccines <- vaccines[ vaccines != '_NA_NA___']
nvac <- round(length(vaccines))
nvac

cases_averted_all <- cases_averted

for (i in 1:nvac){  
  
  rm(vaccine_output)
  rm(vaccine_output_dt)
  gc()
  
  vaccines1 <- vaccines[i]
  
  cases_averted <- cases_averted_all[vaccine_id %in% vaccines1]
  
  
  cases_list = split(cases_averted, by=c("vaccine_id")) ##?? this is bit pointless if sectioning by vaccines above also
  # cases_list = split(cases_averted, by=c("WHO.Region"))
  
  ### !!! now that its split by vaccine / pathogen could make more efficient by subsetting in function
  ## #earlier on - leave for now as running
  
  across_vaccines <- function(cases_averted){
    
    combo_chunks <- function(DRI.all, cases_averted,
                             sample.whoc, sample.whoc.region,
                             who_whoc_wb){
      ## not efficient or good practice but need quick fix for data size issues
      DRI.all <- as.data.table(DRI.all)
      setkeyv(DRI.all,keycols)
      setkeyv(cases_averted,keycols)
      cost_cases <- full_join(DRI.all,cases_averted, by =  keycols)
      rm(cases_averted)
      rm(DRI.all)
      gc()
      
      
      ## those without cases averted that we are using to fill e.g. GI values:
      test <- cost_cases[is.na(WHO.Region)]
      test <- merge(test, who_whoc_wb, by="iso3c")
      test[ , WHO.Region := who.region]
      test <- test[ , -c("who.region","whoc.region.y",                     
                         "wb.region","Income.group")]
      setnames(test, "whoc.region.x","whoc.region")
      temp <- cost_cases[!is.na(WHO.Region)]
      rm(cost_cases)
      cost_cases <- rbind(test,temp)
      rm(test)
      rm(temp)
      gc()
      
      ### split out those that are complete across both and those not
      no_cost_cases <- cost_cases[is.na(los.DRI)]
      cost_cases_c <- cost_cases[!is.na(los.DRI)]
      rm(cost_cases)
      gc()
      ###~~~ !!! check with the gastro nes & if cost_cases is the right one to use
      
      
      ### !!! addition March 2023
      ## remove weighting towards bugs with more countries/age groups impacted
      ### note to make more robust in next iteration could do population weighted regional average values (?)
      cost_cases_c_nonweight <- cost_cases_c %>% 
        group_by(whoc.region,class,gram.stain,
                 syndrome,run) %>% 
        filter(row_number() == 1)
      
      av.los.region.class.gram <- cost_cases_c_nonweight %>% group_by(whoc.region,class,gram.stain,
                                                                      run)%>%
        summarise(av_los_class_gram := mean(los.DRI))%>%
        as.data.table
      
      ### class different/any gram stain
      av.los.region.class <- cost_cases_c_nonweight %>% group_by(whoc.region,class,
                                                                 run)%>%
        summarise(av_los_class := mean(los.DRI))%>%
        as.data.table
      
      ## do another averages for syndrome and region to apply to 
      ## e.g. Gastro
      av.los.region.syndrome <- cost_cases_c_nonweight %>% group_by(whoc.region, 
                                                                    syndrome,
                                                                    run)%>%
        summarise(av_los_syndrome := mean(los.DRI))
      
      ## do a final by region 
      av.los.region <- cost_cases_c_nonweight %>% group_by(whoc.region,
                                                           run)%>%
        summarise(av_los_region := mean(los.DRI))
      
      ## rename runs for merging in later
      names(av.los.region.class.gram)[names(av.los.region.class.gram) == 'run'] <- 'run_class_gram'
      names(av.los.region.class)[names(av.los.region.class) == 'run'] <- 'run_class'
      names(av.los.region.syndrome)[names(av.los.region.syndrome) == 'run'] <- 'run_syndrome'
      names(av.los.region)[names(av.los.region) == 'run'] <- 'run_region'
      
      ## merge together with cost_cases
      ## + using the best available proportion
      ## note had to use slightly roundabout way due to size allocation limits
      
      ### merge in with who_wb_dictionary to get whoc.region labels
      no_cost_cases <- merge(no_cost_cases,who_whoc_wb,
                             by="iso3c", all.x=TRUE, all.y=FALSE,
                             allow.cartesian=TRUE)
      
      no_cost_cases[ , whoc.region := whoc.region.x]
      no_cost_cases[is.na(whoc.region), whoc.region := whoc.region.y]
      no_cost_cases<-no_cost_cases[ , -c("whoc.region.x","whoc.region.y")]
      
      test.iso <- no_cost_cases[is.na(whoc.region)]
      unique(test.iso$iso3c)
      ## "COM" and "SSD"
      no_cost_cases[iso3c=="SSD", whoc.region := "AFRO E"]
      no_cost_cases[iso3c=="COM", whoc.region := "AFRO D"]
      ## REF https://www.thelancet.com/cms/10.1016/S2214-109X(20)30231-X/attachment/65c63993-18d0-4fff-8d21-85defa16a3b2/mmc1.pdf
      ### !! although note previous definitions of WHOC used might need updating in AMR-UCR 
      
      no_cost_cases_class_gram <- merge(no_cost_cases, av.los.region.class.gram,
                                        by=c("whoc.region", "class","gram.stain"),
                                        all.x=TRUE, all.y=FALSE, allow.cartesian=TRUE)
      
      no_cost_cases_class_gram_filled <- no_cost_cases_class_gram[!is.na(av_los_class_gram)]
      
      no_cost_cases_class_gram_empty <- no_cost_cases_class_gram[is.na(av_los_class_gram)]
      
      no_cost_cases_class <- merge(no_cost_cases_class_gram_empty, av.los.region.class,by=c("whoc.region", "class"),
                                   all.x=TRUE, all.y=FALSE, allow.cartesian=TRUE)
      
      
      no_cost_cases_class_filled <- no_cost_cases_class[!is.na(av_los_class)]
      
      no_cost_cases_class_empty <- no_cost_cases_class[is.na(av_los_class)]
      
      
      no_cost_cases_syndrome <- merge(no_cost_cases_class_empty, av.los.region.syndrome,
                                      by=c("whoc.region", "syndrome"),
                                      all.x=TRUE,all.y=FALSE,allow.cartesian=TRUE)
      
      no_cost_cases_syndrome_filled <- no_cost_cases_syndrome[!is.na(av_los_syndrome)]
      
      no_cost_cases_syndrome_empty <- no_cost_cases_syndrome[is.na(av_los_syndrome)]
      
      no_cost_cases_region <- merge(no_cost_cases_syndrome_empty, av.los.region,
                                    by=c("whoc.region"),
                                    all.x=TRUE,all.y=FALSE,allow.cartesian=TRUE)
      
      all_costed <- list(cost_cases_c,
                         no_cost_cases_class_gram_filled,
                         no_cost_cases_class_filled,
                         no_cost_cases_syndrome_filled,
                         no_cost_cases_region)
      
      rm(cost_cases_c,
         no_cost_cases_region,
         no_cost_cases_syndrome_filled,
         no_cost_cases_class_filled)
      rm(no_cost_cases_class_gram_filled)
      
      
      all_costed <- rbindlist(all_costed, fill=TRUE, use.names = TRUE)
      all_costed <- as.data.table(all_costed)
      
      all_costed[is.na(los.DRI) , los.DRI := av_los_class_gram]
      all_costed[is.na(run) , run := run_class_gram]
      all_costed[is.na(los.DRI) , los.DRI := av_los_class]
      all_costed[is.na(run) , run := run_class]
      all_costed[is.na(los.DRI) , los.DRI := av_los_syndrome]
      all_costed[is.na(run) , run := run_syndrome]
      all_costed[is.na(los.DRI) , los.DRI:= av_los_region]
      all_costed[is.na(run) , run := run_region]
      ### tidy up columns
      all_costed <- all_costed[ ,c("los.DRI" , "iso3c" ,"whoc.region" ,"syndrome",                          
                                   "class","gram.stain","WHO.Region" ,  "run", "cases_resistant",
                                   "vaccine_avertable_cases_resistance","Antibiotic.class","Infectious.syndrome" ,              
                                   "Pathogen","prop_hospitalised", "cases_averted_hospitalised","cases_hospitalised",
                                   "vaccine_id","Origin")]
      
      ### check
      test2 <- all_costed[is.na(whoc.region)] ## should be 0 obs
      
      
      
      ## merge together with
      DRI.whoc <- merge(all_costed, sample.whoc, by=c("iso3c",
                                                      "run"),
                        all.x=TRUE,
                        allow.cartesian=TRUE)
      rm(all_costed)
      
      ### merge into those without into regional sample
      DRI.whoc.n <- DRI.whoc[is.na(mean_i)]
      DRI.whoc.y <- DRI.whoc[!is.na(mean_i)]
      unique(DRI.whoc.n$iso3c)
      
      ## "PRK" "SOM" "SSD" "ZWE"
      
      rm(DRI.whoc)
      
      setnames(sample.whoc.region,"region","whoc.region",skip_absent=TRUE)
      
      DRI.whoc.n <- DRI.whoc.n[,-c("mean_i","ID")]
      DRI.whoc.n <- merge(DRI.whoc.n, sample.whoc.region, by=c("whoc.region",
                                                               "run"),
                          all.x=TRUE,
                          all.y=FALSE)
      ### !!! next iteration could try to have more complex matching
      ### across runs / datasets / age groups
      
      DRI.whoc.all <- rbind(DRI.whoc.y,DRI.whoc.n)
      
      rm(DRI.whoc.y)
      rm(DRI.whoc.n)
      
      ## previously saved "outputs/DRI_whoc_all.RData" here
      
      DRI.whoc.all[ , los.cost := los.DRI*mean_i]
      
      ### multiply cases and unit costs ("los.DRI" are the unit costs for LOS, los.cost for cost)
      DRI.whoc.all[ , avertable_cost_cases := los.cost*cases_averted_hospitalised]
      DRI.whoc.all[ , avertable_days_cases := los.DRI*cases_averted_hospitalised]
      
      DRI.whoc.all[ , cost_cases := los.cost*cases_hospitalised]
      DRI.whoc.all[ , days_cases := los.DRI*cases_hospitalised]
      
      DRI.whoc.all <- DRI.whoc.all[,c("run","syndrome" ,"Infectious.syndrome","Antibiotic.class", 
                                      "Pathogen", "class","WHO.Region", 
                                      "cases_averted_hospitalised", "cases_hospitalised",
                                      "los.cost" , "los.DRI",
                                      "avertable_cost_cases","avertable_days_cases", 
                                      "cost_cases", "days_cases","vaccine_id","Origin",
                                      "iso3c")] 
      
      
      #### change class/syndrome back for e.g. salmonella
      DRI.whoc.all[Pathogen=="Non-typhoidal Salmonella"|
                     Pathogen=="Salmonella Typhi"|
                     Pathogen=="Salmonella Paratyphi", class := "Fluoroquinolones or MDR in Salmonella"]
      
      DRI.whoc.all[Pathogen=="Non-typhoidal Salmonella"|
                     Pathogen=="Salmonella Typhi"|
                     Pathogen=="Salmonella Paratyphi", syndrome := "Salmonella infections"]
      
      DRI.whoc.all[Pathogen=="Group A Streptococcus", class := "macrolides"]
      DRI.whoc.all[Pathogen=="Group A Streptococcus", syndrome := "Group A Streptococcus Infections"]
      
      DRI.whoc.all[Pathogen=="Haemophilus influenzae",class := "3g cephalosporins"]
      DRI.whoc.all[Pathogen=="Haemophilus influenzae",syndrome := "Hib infections"]
      
      ## adding over age groups as the samples are huge are otherwise (60+ million per vaccine scenario)
      results <- DRI.whoc.all %>%
        group_by(iso3c, WHO.Region, Pathogen, class, Infectious.syndrome, vaccine_id, run) %>%
        summarise(cases_averted_hospitalised = sum(cases_averted_hospitalised, na.rm=TRUE),
                  cases_hospitalised = sum(cases_hospitalised, na.rm=TRUE),
                  los.cost = median(los.cost, na.rm=TRUE),
                  los.DRI = median(los.DRI,na.rm=TRUE),
                  avertable_cost_cases=sum(avertable_cost_cases,na.rm=TRUE),
                  avertable_days_cases=sum(avertable_days_cases,na.rm=TRUE),
                  cost_cases=sum(cost_cases, na.rm=TRUE),
                  days_cases=sum(days_cases,na.rm=TRUE))
      
      return(results)
      
    }
    
    sdf = split(DRI.all, by=c("run"))
    midway_output <- lapply(sdf, combo_chunks,
                            cases_averted=cases_averted,
                            sample.whoc=sample.whoc,
                            sample.whoc.region=sample.whoc.region,
                            who_whoc_wb=who_whoc_wb)
    rm(sdf)
    gc()
    
    result <- rbindlist(midway_output)
    
    
    return(result)
  }
  
  vaccine_output <- lapply(cases_list,across_vaccines)
  vaccine_output_dt <- rbindlist(vaccine_output)
  
  temp <- vaccine_output_dt[1,]
  temp[, c("vaccine_target_disease","efficacy" ,"coverage"  ,
           "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]
  
  temp <- substr(temp$vaccine_target_disease, start = 1, stop = 4)
  save(vaccine_output_dt, 
       file=paste0("C:/Users/nichola.naylor/Documents/WHO_2023/VAF_AMR_EconBurden/outputs/fulloutput_chunks/",temp,i,".RData"))
  
}

