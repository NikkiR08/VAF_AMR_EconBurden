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
                               "class","gram.stain","WHO.Region" , "Age.group", "run", "cases_resistant",
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
                                  "Pathogen", "class","WHO.Region", "Age.group",
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