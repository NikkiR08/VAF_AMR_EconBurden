
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

########### adapting for test ###################

cases_test <- cases[1:2,]
LT_test1 <- as.data.table(LT)
LT_test1[, WPRO:=1]

LT_test2 <- as.data.table(LT)
LT_test2[, WPRO:=0]


################***** Working Life Years Lost ******#####


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
  
  ### Excess Life Years Lost & Lost Wages from death #######
  
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
  
  ######### calculating productivity losses over the life time #######
  cases_temp <- cases[who.region==country]
  
  hc.prod <- merge(prod, cases_temp, by.x="x.x", by.y="midpoint", all=FALSE)
  ### if time check this is merging appropriately
  hc.prod[ , averted_HC := vaccine_avertable_deaths_resistant*dprod_WLYL]
  hc.prod[ , HC_cost := deaths_resistant*dprod_WLYL]
  ###!!! if want to look at age distributional effects on health outcomes next 
  ### might want to save this file and add to read in for outputs
  
  return(hc.prod)
}


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

output_lst <- list()
unique(cases$who.region)

### testing when dead - no years
output_lst$WPRO <- wlyl_summary(hc.calculator(region="WPRO",LT=LT_test1,
                                                   cases=cases_test, wage_region=wage_region_LY,
                                                   r=0))
output_hc1 <- rbindlist(output_lst, idcol=TRUE)

## should be zero - it is

## testing all the years - 50 years WY
output_lst$WPRO <- wlyl_summary(hc.calculator(region="WPRO",LT=LT_test2,cases=cases_test,
                                                   wage_region=wage_region_LY,
                                                   r=0))
output_hc2 <- rbindlist(output_lst, idcol=TRUE)

a <- sum(cases_test$deaths_resistant)
50*a
## matches

## testing wage function
output_lst$WPRO <- wlyl_summary(hc.calculator(region="WPRO",LT=LT_test2,
                                              cases=cases_test, wage_region=wage_region,
                                              r=0))
output_hc3 <- rbindlist(output_lst, idcol=TRUE)

a*50*8508
## matches

## testing discounting function
output_lst$WPRO <- wlyl_summary(hc.calculator(region="WPRO",LT=LT_test2,cases=cases_test,
                                              wage_region=wage_region,
                                              r=0.03))
output_hc4 <- rbindlist(output_lst, idcol=TRUE)

a <- sum(cases_test$deaths_resistant)
x <- c(rep(0,14),rep(8508,50))
dr <-  1/(1+0.03)^(1:length(x))
x <- x*dr
sum(x)*a
### matches
