
######################## LABOUR PRODUCTIVITY #################################
################## requried libraries
require(readxl)
require(tidyverse)
require(data.table)
require(janitor)
require(reshape2)

# ### LOADING IN DATA AND PACKAGES FROM DATA MANIP when testing
##################################**** BY INFECTIOUS SYNDROME *****################
################# required case & death data

load("data_inputs/epi_inputs_all.RData") ## cases
cases <- all_data
rm(all_data)
cases <- cases %>%
  unite("vaccine_id",vaccine:target_population) %>%
  as.data.table()
## create low/numerical definers for groups
cases[, c("low","high") := tstrsplit(Age.group, "to")]

cases[Age.group=="Early Neonatal"|
        Age.group== "Late Neonatal"|
        Age.group=="Post Neonatal", low := 0]
cases[Age.group=="Early Neonatal"|
        Age.group== "Late Neonatal"|
        Age.group=="Post Neonatal", high := 0]
cases[Age.group=="95 plus" , low := 95] 
cases[Age.group=="95 plus" , high := 100] ## capping at 100

cases[ , low := as.numeric(low)]
cases[ , high := as.numeric(high)]
cases[ , midpoint := ceiling((low+high)/2)] ## for merging in later

cases[WHO.Region=="European Region", who.region := "EURO"]            
cases[WHO.Region=="Western Pacific Region", who.region := "WPRO"]       
cases[WHO.Region== "Region of the Americas", who.region := "PAHO"]             
cases[WHO.Region=="South-East Asia Region", who.region := "SEARO"]     
cases[WHO.Region=="Eastern Mediterranean Region", who.region := "EMRO"]
cases[WHO.Region=="African Region", who.region := "AFRO"]


### wages
wage_region <- as.data.table(read.csv("data_inputs/regional_labour.csv"))
wage_template <-  as.data.table(read.csv("data_inputs/wage_template.csv"))

wage_region_LY <- as.data.table(wage_region) ## need to fully copy otherwise overwrites wage_region
wage_region_LY <- wage_region_LY[,"average_scenario1" := 1 ]
wage_region_LY <- wage_region_LY[, "average_scenario2":=1]

## convert from months to years
wage_region[ , average_scenario1 := average_scenario1*12]
wage_region[ , average_scenario2 := average_scenario2*12]

### regional life tables
LT <- read.csv("data_inputs/life_tables_region.csv")
LT <- as.data.table(LT)
LT <- LT[Indicator=="nqx - probability of dying between ages x and x+n" &
           Period=="2019"]
##Probability of dying between the beginning of the age group x and the beginning of the next age group x+n, n being the interval of the age group.
## https://www.who.int/data/gho/indicator-metadata-registry/imr-details/30

## keep columns we want
LT <- LT[ ,c("Location", 
             "FactValueNumeric","Dim1", "Dim2")]

LT[, c("low","high") := tstrsplit(Dim2, "-")]
LT[ , low := extract_numeric(low)]
LT[ , high := extract_numeric(high)]
LT[Dim2=="85+ years", high := 100] ## capping at 100

LT[Dim2=="<1 year", low := 0]
LT[Dim2=="<1 year", high := 0]

## renaming WHO regions
LT[Location=="Europe", who.region := "EURO"]            
LT[Location=="Western Pacific", who.region := "WPRO"]       
LT[Location== "Americas", who.region := "PAHO"]             
LT[Location=="South-East Asia", who.region := "SEARO"]     
LT[Location=="Eastern Mediterranean", who.region := "EMRO"]
LT[Location=="Africa", who.region := "AFRO"]

LT <- LT[ , -c("Location")]

#### adapting the life table to have yearly transitions
LT[ , t.int := 1/(high-low)]
LT[t.int==Inf, t.int := 1]
LT[ , rate.temp := (-log(1-FactValueNumeric))/1]
LT[ , p.final := 1-exp(-rate.temp*t.int)]


LT <- dcast(LT,Dim2+low+high+Dim1~who.region, value.var = "p.final")
LT <- as.data.table(LT)
### getting a row per year group
LT[ , freq := high-low+1]
LT <- LT[rep(seq(.N), freq), !"freq"]
LT[, id := seq_len(.N), by = c("Dim1","Dim2")]
LT[ , id := id-1]
LT[ , Age := low+id]

### Cap at 84, as other wise the probability of dying 85+ is 1 (and we're not costing beyond 65 anyways)
LT <- LT[Age<85]

# # #### doing for each region

hc.calculator <- function(region,LT,cases,wage_region,
                          r=0){
  
  # # ### testing 
  # region <- "WPRO"
  # LT <- LT_test2
  # cases <- cases_test
  # wage_region <- wage_region
  # r <- 0.03
  
  
  ### !!! in update should pre-specify all the data sets that feed into this function
  region <- region
  country <- region
  tempvector <- c("Dim1","Age",country)
  tempLT <- LT[, ..tempvector]
  
  lt.male_input <- tempLT[Dim1=="Male"]
  ## getting to match format to run through previous code
  lt.male_input <- lt.male_input[ ,-c("Dim1")]
  
  lt.female_input <- tempLT[Dim1=="Female"]
  lt.female_input <- lt.female_input[ ,-c("Dim1")]
  
  ## getting wage in the format we need 
  wagetemp <- wage_region[who.region==country]
  dt.wage <- wage_template
  dt.wage[ , wage := (wagetemp$average_scenario1)*wage_adjuster] 

  
  r <- r
  
  myvector <- c("Age",country)
  
  l_x_est <- function(dt, countr){
    ## dt = data table with q(x) vaues
    ## country = selected country
    
    myvector <- c("Age",countr)
    
    y <- dt[, ..myvector]
    colnames(y) <- c("x","q_x")
    
    
    ## order based on x
    y <- y[order(x)]
    
    y[ , d_x := -log(1-y$q_x)]
    
    y[ 1, l_x := 100000] 
    
    for (i in 2:nrow(y)){
      y[i, l_x := y$l_x[[i-1]] *
          exp((-y$d_x[[i-1]]))]
    }
    return(y)
  }
  
  q.male <- l_x_est(lt.male_input, country)##
  q.female <- l_x_est(lt.female_input, country)
  
  q.person <- merge(q.male, q.female, by="x")
  colnames(q.person) <- c("x","q_male","d_male","l_male",
                          "q_female","d_female","l_female")
  q.person[ , p.f := l_female/(l_female+l_male)]
  q.person[ , l_person := (p.f*l_female)+
              ((1-p.f)*l_male)]
  
  for (i in 1:(nrow(q.person)-1)){
    q.person[i, bigl_x := (q.person$l_person[[i]]+ q.person$l_person[[i+1]])/2]
  }
  
  q.person[nrow(q.person), bigl_x := (q.person$l_person[[nrow(q.person)]])/2]
  
  ## remove NA rows
  
  for (i in 1:nrow(q.person)){
    q.person[i, t_x := sum(q.person$bigl_x[i:nrow(q.person)])]
  }
  
  q.person[ , LE_x := t_x/l_person]
  
  
  dt.wage <- dt.wage[,  c("low","high","wage")]
  colnames(dt.wage) <- c("low","high","wage_age")
  
  prod <- q.person[dt.wage, on = .(x >= low, x <= high), nomatch = 0,
                   .(x.x, l_person, bigl_x, t_x, LE_x,wage_age)]
  
  prod[ , z_x := bigl_x*wage_age] 
  
  for (i in 1:nrow(prod)){
    prod[i , t_adj := sum(prod$z_x[i:nrow(prod)])]
  }
  
  prod[ , prod_x := t_adj/l_person]
  
  prod_hc.calc <- prod[ , c("x.x","z_x")]
  
  temp.q <- list()
  for (i in 1:nrow(prod_hc.calc)){
    temp.q[[i]] <- prod_hc.calc[i:nrow(prod_hc.calc),]
  }
  
  temp.q <- bind_rows(temp.q, .id = "column_label")
  temp.q %>% setDT() ## creating a copy as otherwise there is a warning
  ## message (still runs but just for "clean" code), so this stops attempts of .internal.selfref detected
  temp.q_copy <- copy(temp.q)
  temp.q_copy[ , column_label := as.numeric(column_label)-1]
  temp.q_copy[ , b_x := z_x/((1+r))^(x.x-(column_label))] ## column label is age at death, x.x is year
  total.b <- temp.q_copy[,.(bigb_x=sum(b_x)), by=column_label]
  
  colnames(total.b) <- c("x.x","bigb_x")
  ### !! need to check this, but should be by age groups, so replace x.x labelling for now
  total.b$x.x <- unique(temp.q$x.x)
  
  prod <- merge(prod, total.b, by="x.x")
  
  prod[ , dprod_WLYL := bigb_x/l_person]
  
  ######### calculating productivity losses over the life time 
  cases_temp <- cases[who.region==country]
  
  hc.prod <- merge(prod, cases_temp, by.x="x.x", by.y="midpoint", all=FALSE)
  ### if time check this is merging appropriately
  hc.prod[ , averted_HC := vaccine_avertable_deaths_resistant*dprod_WLYL]
  hc.prod[ , HC_cost := deaths_resistant*dprod_WLYL]
  ###!!! if want to look at age distributional effects on health outcomes next 
  ### might want to save this file and add to read in for outputs
  
  return(hc.prod)
}


## by group 
total_wage_noage_group <- function(hc.prod) {
 x <-  hc.prod[, lapply(.SD, sum, na.rm=TRUE),
                              by = c("WHO.Region","Pathogen",
                                     "vaccine_id" ,
                                     # "Antibiotic.class",
                                     "Infectious.syndrome"),
                              .SDcols=c("averted_HC",
                                        "HC_cost")]
  ## nb if want by country can use -by = c("ISO3","Infectious.syndrome",
  ## "Pathogen",
  ## "Antibiotic.class"), but not it will be based on Regional Life Tables 
  return(x)
}

output_lst <- list()
unique(cases$who.region)
output_lst$WPRO <- total_wage_noage_group(hc.calculator(region="WPRO",
                                                        LT=LT,
                                                        cases=cases,
                                                        wage_region=wage_region,
                                                        r=0.03))
output_lst$SEARO <- total_wage_noage_group(hc.calculator(region="SEARO",
                                                         LT=LT,
                                                         cases=cases,
                                                         wage_region=wage_region,
                                                         r=0.03))
output_lst$EURO <- total_wage_noage_group(hc.calculator(region="EURO",
                                                        LT=LT,
                                                        cases=cases,
                                                        wage_region=wage_region,
                                                        r=0.03))
output_lst$PAHO <- total_wage_noage_group(hc.calculator(region="PAHO",
                                                        LT=LT,
                                                        cases=cases,
                                                        wage_region=wage_region,
                                                        r=0.03))
output_lst$AFRO <- total_wage_noage_group(hc.calculator(region="AFRO",
                                                        LT=LT,
                                                        cases=cases,
                                                        wage_region=wage_region,
                                                        r=0.03))
output_lst$EMRO <-total_wage_noage_group(hc.calculator(region="EMRO",
                                                       LT=LT,
                                                       cases=cases,
                                                       wage_region=wage_region,
                                                       r=0.03))

output_hc <- rbindlist(output_lst, idcol=TRUE)

deaths_all <- output_hc


############## ****** BY PATHOGEN ******########
total_wage_noage_pathogen <- function(hc.prod) {
  x <-    hc.prod[, lapply(.SD, sum, na.rm=TRUE),
                                      by = c("WHO.Region","Pathogen",
                                             "vaccine_id" #,
                                             # "Antibiotic.class",
                                             # "Infectious.syndrome"
                                      ),
                                      .SDcols=c("averted_HC",
                                                "HC_cost")]
  ## nb if want by country can use -by = c("ISO3","Infectious.syndrome",
  ## "Pathogen",
  ## "Antibiotic.class"), but not it will be based on Regional Life Tables 
  return(x)
}

### !!! for update - make more efficient & update labelling to match productivity
output_lst <- list()
unique(cases$who.region)
output_lst$WPRO <- total_wage_noage_pathogen(hc.calculator(region="WPRO",
                                                           LT=LT,
                                                           cases=cases,
                                                           wage_region=wage_region,
                                                           r=0.03))
output_lst$SEARO <- total_wage_noage_pathogen(hc.calculator(region="SEARO",
                                                            LT=LT,
                                                            cases=cases,
                                                            wage_region=wage_region,
                                                            r=0.03))
output_lst$EURO <- total_wage_noage_pathogen(hc.calculator(region="EURO",
                                                           LT=LT,
                                                           cases=cases,
                                                           wage_region=wage_region,
                                                           r=0.03))
output_lst$PAHO <- total_wage_noage_pathogen(hc.calculator(region="PAHO",
                                                           LT=LT,
                                                           cases=cases,
                                                           wage_region=wage_region,
                                                           r=0.03))
output_lst$AFRO <- total_wage_noage_pathogen(hc.calculator(region="AFRO",
                                                           LT=LT,
                                                           cases=cases,
                                                           wage_region=wage_region,
                                                           r=0.03))
output_lst$EMRO <-total_wage_noage_pathogen(hc.calculator(region="EMRO",
                                                          LT=LT,
                                                          cases=cases,
                                                          wage_region=wage_region,
                                                          r=0.03))
output_hc <- rbindlist(output_lst, idcol=TRUE)

deaths_path <- copy(output_hc)

deaths_path$Infectious.syndrome <- "Total"

deaths_all <- as.data.table(deaths_all)

deaths_all$WHO.Region <- deaths_all$.id
deaths_all <- deaths_all[ , -c(".id")]

deaths_path <- as.data.table(deaths_path)
deaths_path$WHO.Region <- deaths_path$.id
deaths_path <- deaths_path[ , -c(".id")]

death <- rbind(deaths_all, deaths_path)

death.av <- death[ ,-c("HC_cost")]
death.all <- death[ ,- c("averted_HC")]

death.all <- dcast(death.all, Pathogen + Infectious.syndrome +
                     vaccine_id ~ WHO.Region, value.var = c("HC_cost"))

death.av <- dcast(death.av, Pathogen + Infectious.syndrome +
                    vaccine_id ~ WHO.Region, value.var = c("averted_HC"))

deaths.prod <- merge(death.all, death.av, by=c("Pathogen",
                                               "Infectious.syndrome",
                                               "vaccine_id")) %>% as.data.table()

deaths.prod[, c("vaccine-target disease","efficacy" ,"coverage"  ,
                "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including those with no mached vaccine
deaths.prod <- deaths.prod[vaccine_id!="_NA_NA___"]

## relabelling columns 
setnames(deaths.prod, old = c( "AFRO.x","PAHO.x","EMRO.x","EURO.x" ,"SEARO.x", "WPRO.x" ,
                               "AFRO.y", "PAHO.y","EMRO.y" ,"EURO.y","SEARO.y" ,"WPRO.y"   ), 
         new = c("total_AFRO","total_AMRO","total_EMRO",	"total_EURO",
                 "total_SEARO",	"total_WPRO",	
                 "averted_AFRO","averted_AMRO",
                 "averted_EMRO","averted_EURO",	"averted_SEARO",	"averted_WPRO"))

write.csv(deaths.prod, file="outputs/END_productivity_loss_deaths.csv")


############## ****** BY PATHOGEN_GLOBAL ******########

### !! have to rerun the above code first
deaths.prod.global <- data.table::melt(deaths.prod, id.vars = c("Pathogen" ,
                                                    "Infectious.syndrome" ,
                       "vaccine_id",
                       "vaccine-target disease" ,
                         "efficacy"         ,
                         "coverage"         ,
                         "duration"         ,
                            "target disease" ,       
                        "target population"), 
                       measure.vars = list(c('total_AFRO' ,
                                             'total_EMRO',
                                             'total_EURO' ,
                                             'total_AMRO',
                                             'total_SEARO',
                                             'total_WPRO'   ),
                                           c('averted_AFRO' ,    
                                            'averted_EMRO',
                                            'averted_EURO' ,
                                            'averted_AMRO',
                                            'averted_SEARO' ,
                                            'averted_WPRO'  )),
                                    value.name = c('total', 'averted'))

deaths.prod.global.syndrome <- deaths.prod.global[, lapply(.SD, sum, na.rm=TRUE),
                                       by = c("Pathogen",
                                              "vaccine_id" ,
                                              # "Antibiotic.class",
                                              "Infectious.syndrome"
                                       ),
                                       .SDcols=c("averted",
                                                 "total")]
## remove duplicates
deaths.prod.global.syndrome <- distinct(deaths.prod.global.syndrome)

write.csv(deaths.prod.global.syndrome, file="outputs/END_productivity_loss_deaths_global.csv")

############## ****** ALL Levels ******########
total_wage_noage_all <- function(hc.prod) {
  x <-    hc.prod[, lapply(.SD, sum, na.rm=TRUE),
                  by = c("WHO.Region","Pathogen",
                         "vaccine_id",
                          "Antibiotic.class",
                          "Infectious.syndrome"
                  ),
                  .SDcols=c("averted_HC",
                            "HC_cost")]
  ## nb if want by country can use -by = c("ISO3","Infectious.syndrome",
  ## "Pathogen",
  ## "Antibiotic.class"), but not it will be based on Regional Life Tables 
  return(x)
}

### !!! for update - make more efficient & update labelling to match productivity
output_lst <- list()
unique(cases$who.region)
output_lst$WPRO <- total_wage_noage_all(hc.calculator(region="WPRO",
                                                      LT=LT,
                                                      cases=cases,
                                                      wage_region=wage_region,
                                                      r=0.03))
output_lst$SEARO <- total_wage_noage_all(hc.calculator(region="SEARO",
                                                       LT=LT,
                                                       cases=cases,
                                                       wage_region=wage_region,
                                                       r=0.03))
output_lst$EURO <- total_wage_noage_all(hc.calculator(region="EURO",
                                                      LT=LT,
                                                      cases=cases,
                                                      wage_region=wage_region,
                                                      r=0.03))
output_lst$PAHO <- total_wage_noage_all(hc.calculator(region="PAHO",
                                                      LT=LT,
                                                      cases=cases,
                                                      wage_region=wage_region,
                                                      r=0.03))
output_lst$AFRO <- total_wage_noage_all(hc.calculator(region="AFRO",
                                                      LT=LT,
                                                      cases=cases,
                                                      wage_region=wage_region,
                                                      r=0.03))
output_lst$EMRO <-total_wage_noage_all(hc.calculator(region="EMRO",
                                                     LT=LT,
                                                     cases=cases,
                                                     wage_region=wage_region,
                                                     r=0.03))
output_hc <- rbindlist(output_lst, idcol=TRUE)

###!!! not including those with no matched vaccine
output_hc <- output_hc[vaccine_id!="_NA_NA___"]

output_hc[`.id`=="PAHO", `.id` :="AMRO"]

write.csv(output_hc, file="outputs/productivity_loss_deaths_all.csv")

######### ******SCENARIO 2 IN HC *****###################

hc.calculator.sc2 <- function(region,LT,cases,wage_region,
                          r=0){
  
  # # ### testing 
  # region <- "WPRO"
  # LT <- LT_test2
  # cases <- cases_test
  # wage_region <- wage_region
  # r <- 0.03
  
  
  ### !!! in update should pre-specify all the data sets that feed into this function
  region <- region
  country <- region
  tempvector <- c("Dim1","Age",country)
  tempLT <- LT[, ..tempvector]
  
  lt.male_input <- tempLT[Dim1=="Male"]
  ## getting to match format to run through previous code
  lt.male_input <- lt.male_input[ ,-c("Dim1")]
  
  lt.female_input <- tempLT[Dim1=="Female"]
  lt.female_input <- lt.female_input[ ,-c("Dim1")]
  
  ## getting wage in the format we need 
  wagetemp <- wage_region[who.region==country]
  dt.wage <- wage_template
  dt.wage[ , wage := (wagetemp$average_scenario2)*wage_adjuster] 
  
  
  r <- r
  
  myvector <- c("Age",country)
  
  l_x_est <- function(dt, countr){
    ## dt = data table with q(x) vaues
    ## country = selected country
    
    myvector <- c("Age",countr)
    
    y <- dt[, ..myvector]
    colnames(y) <- c("x","q_x")
    
    
    ## order based on x
    y <- y[order(x)]
    
    y[ , d_x := -log(1-y$q_x)]
    
    y[ 1, l_x := 100000] 
    
    for (i in 2:nrow(y)){
      y[i, l_x := y$l_x[[i-1]] *
          exp((-y$d_x[[i-1]]))]
    }
    return(y)
  }
  
  q.male <- l_x_est(lt.male_input, country)##
  q.female <- l_x_est(lt.female_input, country)
  
  q.person <- merge(q.male, q.female, by="x")
  colnames(q.person) <- c("x","q_male","d_male","l_male",
                          "q_female","d_female","l_female")
  q.person[ , p.f := l_female/(l_female+l_male)]
  q.person[ , l_person := (p.f*l_female)+
              ((1-p.f)*l_male)]
  
  for (i in 1:(nrow(q.person)-1)){
    q.person[i, bigl_x := (q.person$l_person[[i]]+ q.person$l_person[[i+1]])/2]
  }
  
  q.person[nrow(q.person), bigl_x := (q.person$l_person[[nrow(q.person)]])/2]
  
  ## remove NA rows
  
  for (i in 1:nrow(q.person)){
    q.person[i, t_x := sum(q.person$bigl_x[i:nrow(q.person)])]
  }
  
  q.person[ , LE_x := t_x/l_person]
  
  
  dt.wage <- dt.wage[,  c("low","high","wage")]
  colnames(dt.wage) <- c("low","high","wage_age")
  
  prod <- q.person[dt.wage, on = .(x >= low, x <= high), nomatch = 0,
                   .(x.x, l_person, bigl_x, t_x, LE_x,wage_age)]
  
  prod[ , z_x := bigl_x*wage_age] 
  
  for (i in 1:nrow(prod)){
    prod[i , t_adj := sum(prod$z_x[i:nrow(prod)])]
  }
  
  prod[ , prod_x := t_adj/l_person]
  
  prod_hc.calc <- prod[ , c("x.x","z_x")]
  
  temp.q <- list()
  for (i in 1:nrow(prod_hc.calc)){
    temp.q[[i]] <- prod_hc.calc[i:nrow(prod_hc.calc),]
  }
  
  temp.q <- bind_rows(temp.q, .id = "column_label")
  temp.q %>% setDT() ## creating a copy as otherwise there is a warning
  ## message (still runs but just for "clean" code), so this stops attempts of .internal.selfref detected
  temp.q_copy <- copy(temp.q)
  temp.q_copy[ , column_label := as.numeric(column_label)-1]
  temp.q_copy[ , b_x := z_x/((1+r))^(x.x-(column_label))] ## column label is age at death, x.x is year
  total.b <- temp.q_copy[,.(bigb_x=sum(b_x)), by=column_label]
  
  colnames(total.b) <- c("x.x","bigb_x")
  ### !! need to check this, but should be by age groups, so replace x.x labelling for now
  total.b$x.x <- unique(temp.q$x.x)
  
  prod <- merge(prod, total.b, by="x.x")
  
  prod[ , dprod_WLYL := bigb_x/l_person]
  
  ######### calculating productivity losses over the life time 
  cases_temp <- cases[who.region==country]
  
  hc.prod <- merge(prod, cases_temp, by.x="x.x", by.y="midpoint", all=FALSE)
  ### if time check this is merging appropriately
  hc.prod[ , averted_HC := vaccine_avertable_deaths_resistant*dprod_WLYL]
  hc.prod[ , HC_cost := deaths_resistant*dprod_WLYL]
  ###!!! if want to look at age distributional effects on health outcomes next 
  ### might want to save this file and add to read in for outputs
  
  return(hc.prod)
}


output_lst <- list()
unique(cases$who.region)
output_lst$WPRO <- total_wage_noage_group(hc.calculator.sc2(region="WPRO",
                                                            LT=LT,
                                                            cases=cases,
                                                            wage_region=wage_region,
                                                            r=0.03))
output_lst$SEARO <- total_wage_noage_group(hc.calculator.sc2(region="SEARO",
                                                             LT=LT,
                                                             cases=cases,
                                                             wage_region=wage_region,
                                                             r=0.03))
output_lst$EURO <- total_wage_noage_group(hc.calculator.sc2(region="EURO",
                                                            LT=LT,
                                                            cases=cases,
                                                            wage_region=wage_region,
                                                            r=0.03))
output_lst$PAHO <- total_wage_noage_group(hc.calculator.sc2(region="PAHO",
                                                            LT=LT,
                                                            cases=cases,
                                                            wage_region=wage_region,
                                                            r=0.03))
output_lst$AFRO <- total_wage_noage_group(hc.calculator.sc2(region="AFRO",
                                                            LT=LT,
                                                            cases=cases,
                                                            wage_region=wage_region,
                                                            r=0.03))
output_lst$EMRO <-total_wage_noage_group(hc.calculator.sc2(region="EMRO",
                                                           LT=LT,
                                                           cases=cases,
                                                           wage_region=wage_region,
                                                           r=0.03))

output_hc <- rbindlist(output_lst, idcol=TRUE)

deaths_all <- output_hc


############## ****** BY PATHOGEN ******########
total_wage_noage_pathogen <- function(hc.prod) {
  x <-    hc.prod[, lapply(.SD, sum, na.rm=TRUE),
                  by = c("WHO.Region","Pathogen",
                         "vaccine_id" #,
                         # "Antibiotic.class",
                         # "Infectious.syndrome"
                  ),
                  .SDcols=c("averted_HC",
                            "HC_cost")]
  ## nb if want by country can use -by = c("ISO3","Infectious.syndrome",
  ## "Pathogen",
  ## "Antibiotic.class"), but not it will be based on Regional Life Tables 
  return(x)
}

### !!! for update - make more efficient & update labelling to match productivity
output_lst <- list()
unique(cases$who.region)
output_lst$WPRO <- total_wage_noage_pathogen(hc.calculator.sc2(region="WPRO",
                                                               LT=LT,
                                                               cases=cases,
                                                               wage_region=wage_region,
                                                               r=0.03))
output_lst$SEARO <- total_wage_noage_pathogen(hc.calculator.sc2(region="SEARO",
                                                                LT=LT,
                                                                cases=cases,
                                                                wage_region=wage_region,
                                                                r=0.03))
output_lst$EURO <- total_wage_noage_pathogen(hc.calculator.sc2(region="EURO",
                                                               LT=LT,
                                                               cases=cases,
                                                               wage_region=wage_region,
                                                               r=0.03))
output_lst$PAHO <- total_wage_noage_pathogen(hc.calculator.sc2(region="PAHO",
                                                               LT=LT,
                                                               cases=cases,
                                                               wage_region=wage_region,
                                                               r=0.03))
output_lst$AFRO <- total_wage_noage_pathogen(hc.calculator.sc2(region="AFRO",
                                                               LT=LT,
                                                               cases=cases,
                                                               wage_region=wage_region,
                                                               r=0.03))
output_lst$EMRO <-total_wage_noage_pathogen(hc.calculator.sc2(region="EMRO",
                                                              LT=LT,
                                                              cases=cases,
                                                              wage_region=wage_region,
                                                              r=0.03))
output_hc <- rbindlist(output_lst, idcol=TRUE)

deaths_path <- output_hc

deaths_path$Infectious.syndrome <- "Total"

deaths_all <- as.data.table(deaths_all)

deaths_all$WHO.Region <- deaths_all$.id
deaths_all <- deaths_all[ , -c(".id")]

deaths_path <- as.data.table(deaths_path)
deaths_path$WHO.Region <- deaths_path$.id
deaths_path <- deaths_path[ , -c(".id")]

death <- rbind(deaths_all, deaths_path)

death.av <- death[ ,-c("HC_cost")]
death.all <- death[ ,- c("averted_HC")]

death.all <- dcast(death.all, Pathogen + Infectious.syndrome +
                     vaccine_id ~ WHO.Region, value.var = c("HC_cost"))

death.av <- dcast(death.av, Pathogen + Infectious.syndrome +
                    vaccine_id ~ WHO.Region, value.var = c("averted_HC"))

deaths.prod <- merge(death.all, death.av, by=c("Pathogen",
                                               "Infectious.syndrome",
                                               "vaccine_id")) %>% as.data.table()

deaths.prod[, c("vaccine-target disease","efficacy" ,"coverage"  ,
                "duration", "target disease","target population") := tstrsplit(vaccine_id, "_", fixed=TRUE)]

###!!! not including early and late neonatal (those are the NA rows, but could add in together and match to vaccines if wanted):
deaths.prod <- deaths.prod[vaccine_id!="_NA_NA___"]

## relabelling columns 
setnames(deaths.prod, old = c( "AFRO.x","PAHO.x","EMRO.x","EURO.x" ,"SEARO.x", "WPRO.x" ,
                               "AFRO.y", "PAHO.y","EMRO.y" ,"EURO.y","SEARO.y" ,"WPRO.y"   ), 
         new = c("mean_total_AFRO","mean_total_AMRO","mean_total_EMRO",	"mean_total_EURO",
                 "mean_total_SEARO",	"mean_total_WPRO",	
                 "mean_averted_AFRO","mean_averted_AMRO",
                 "mean_averted_EMRO","mean_averted_EURO",	"mean_averted_SEARO",	"mean_averted_WPRO"))

###!! it says mean but its the total for each region (no averaging really used in combination, average wage and average case thought)

write.csv(deaths.prod, file="outputs/productivity_loss_deaths_sc2.csv")


total_wage_noage_all <- function(hc.prod) {
  x <-    hc.prod[, lapply(.SD, sum, na.rm=TRUE),
                  by = c("WHO.Region","Pathogen",
                         "vaccine_id",
                         "Antibiotic.class",
                         "Infectious.syndrome"
                  ),
                  .SDcols=c("averted_HC",
                            "HC_cost")]
  ## nb if want by country can use -by = c("ISO3","Infectious.syndrome",
  ## "Pathogen",
  ## "Antibiotic.class"), but not it will be based on Regional Life Tables 
  return(x)
}

### !!! for update - make more efficient & update labelling to match productivity
output_lst <- list()
unique(cases$who.region)
output_lst$WPRO <- total_wage_noage_all(hc.calculator.sc2(region="WPRO",
                                                          LT=LT,
                                                          cases=cases,
                                                          wage_region=wage_region,
                                                          r=0.03))
output_lst$SEARO <- total_wage_noage_all(hc.calculator.sc2(region="SEARO",
                                                           LT=LT,
                                                           cases=cases,
                                                           wage_region=wage_region,
                                                           r=0.03))
output_lst$EURO <- total_wage_noage_all(hc.calculator.sc2(region="EURO",
                                                          LT=LT,
                                                          cases=cases,
                                                          wage_region=wage_region,
                                                          r=0.03))
output_lst$PAHO <- total_wage_noage_all(hc.calculator.sc2(region="PAHO",
                                                          LT=LT,
                                                          cases=cases,
                                                          wage_region=wage_region,
                                                          r=0.03))
output_lst$AFRO <- total_wage_noage_all(hc.calculator.sc2(region="AFRO",
                                                          LT=LT,
                                                          cases=cases,
                                                          wage_region=wage_region,
                                                          r=0.03))
output_lst$EMRO <-total_wage_noage_all(hc.calculator.sc2(region="EMRO",
                                                         LT=LT,
                                                         cases=cases,
                                                         wage_region=wage_region,
                                                         r=0.03))
output_hc <- rbindlist(output_lst, idcol=TRUE)

###!!! not including rows with no associated vaccines
output_hc <- output_hc[vaccine_id!="_NA_NA___"]

output_hc[`.id`=="PAHO", `.id` :="AMRO"]

write.csv(output_hc, file="outputs/productivity_loss_deaths_all_sc2.csv")

################***** Working Life Years Lost ******#####

wlyl_summary <- function(hc.prod) {
  x <-    hc.prod[, lapply(.SD, sum, na.rm=TRUE),
                  by = c("WHO.Region","Pathogen",
                         "vaccine_id" ,
                         "Antibiotic.class",
                          "Infectious.syndrome" ),
                  .SDcols=c("averted_HC",
                            "HC_cost")]
  return(x)
}

### !!! for update - make more efficient & update labelling to match productivity
output_lst <- list()
unique(cases$who.region)
output_lst$WPRO <- wlyl_summary(hc.calculator(region="WPRO",
                                              LT=LT,
                                              cases=cases,
                                              wage_region=wage_region_LY,
                                              r=0))
output_lst$SEARO <- wlyl_summary(hc.calculator(region="SEARO",
                                                    LT=LT,
                                                    cases=cases,
                                                    wage_region=wage_region_LY,
                                                    r=0))
output_lst$EURO <- wlyl_summary(hc.calculator(region="EURO",
                                                   LT=LT,
                                                   cases=cases,
                                                   wage_region=wage_region_LY,
                                                   r=0))
output_lst$PAHO <- wlyl_summary(hc.calculator(region="PAHO",
                                                   LT=LT,
                                                   cases=cases,
                                                   wage_region=wage_region_LY,
                                                   r=0))
output_lst$AFRO <- wlyl_summary(hc.calculator(region="AFRO",
                                                   LT=LT,
                                                   cases=cases,
                                                   wage_region=wage_region_LY,
                                                   r=0))
output_lst$EMRO <-wlyl_summary(hc.calculator(region="EMRO",
                                                  LT=LT,
                                                  cases=cases,
                                                  wage_region=wage_region_LY,
                                                  r=0))
output_hc <- rbindlist(output_lst, idcol=TRUE)

output_hc[`.id`=="PAHO", `.id` :="AMRO"]
save(output_hc,file="outputs/working_life_years_lost.Rdata" )

######################*****FRICTION COST METHOD**********############
#################
load("data_inputs/epi_inputs_all.RData") ## cases
wage_region <- as.data.table(read.csv("data_inputs/regional_labour.csv"))
wage_template <-  as.data.table(read.csv("data_inputs/wage_template.csv"))

FC <- all_data
rm(all_data)
FC <- FC %>%
  unite("vaccine_id",vaccine:target_population) %>%
  as.data.table()
## create low/numerical definers for groups
FC[, c("low","high") := tstrsplit(Age.group, "to")]

FC[Age.group=="Early Neonatal"|
     Age.group== "Late Neonatal"|
     Age.group=="Post Neonatal", low := 0]
FC[Age.group=="Early Neonatal"|
     Age.group== "Late Neonatal"|
     Age.group=="Post Neonatal", high := 0]
FC[Age.group=="95 plus" , low := 95] 
FC[Age.group=="95 plus" , high := 100] ## capping at 100

FC[ , low := as.numeric(low)]
FC[ , high := as.numeric(high)]
FC[ , midpoint := ceiling((low+high)/2)] ## for merging in later

FC[WHO.Region=="European Region", who.region := "EURO"]            
FC[WHO.Region=="Western Pacific Region", who.region := "WPRO"]       
FC[WHO.Region== "Region of the Americas", who.region := "PAHO"]             
FC[WHO.Region=="South-East Asia Region", who.region := "SEARO"]     
FC[WHO.Region=="Eastern Mediterranean Region", who.region := "EMRO"]
FC[WHO.Region=="African Region", who.region := "AFRO"]
FC[, c("low","high") := tstrsplit(Age.group, "to")]
FC[Age.group=="Early Neonatal"|
     Age.group== "Late Neonatal"|
     Age.group=="Post Neonatal", low := 0]
FC[Age.group=="Early Neonatal"|
     Age.group== "Late Neonatal"|
     Age.group=="Post Neonatal", high := 0]
FC[Age.group=="95 plus" , low := 95]
FC[Age.group=="95 plus" , high := 100] ## capping at 100

FC[ , low := as.numeric(low)]
FC[ , high := as.numeric(high)]
FC[ , midpoint := ceiling((low+high)/2)] ## for merging in later

dt.wage <- merge(FC,wage_region, by="who.region")

dt.wage <- dt.wage[!is.na(Age.group)]

wage_template <- wage_template[,  c("low","high","wage_adjuster")]

FC.wage <- dt.wage[wage_template, on = .(midpoint >= low, midpoint <= high), nomatch = 0]

FC.wage[, wage := (((average_scenario1)*6*wage_adjuster))] ## 6 months friction cost
FC.wage[ , wage_loss_total := deaths_resistant*wage]
FC.wage[ , wage_loss_averted := vaccine_avertable_deaths_resistant*wage]

## sum prod_hc by group
total_wage_FC <- FC.wage[, lapply(.SD, sum, na.rm=TRUE),
                                by = c("who.region",
                                       "Pathogen" ,
                                       "vaccine_id",
                                       "Antibiotic.class",
                                       "Infectious.syndrome"
                                ),
                                .SDcols=c("wage_loss_total",
                                          "wage_loss_averted")]

total_wage_FC[who.region=="PAHO", who.region := "AMRO"]

total_wage_FC <- total_wage_FC[vaccine_id!="_NA_NA___"]
write.csv(total_wage_FC, file="outputs/vaccine_avertable_FC.csv")


