##### combining the estimates for AMR DRI and S ########

library(tidyverse)
library(data.table)
library(truncnorm) ## for truncated sampling
library(truncdist)

### SAMPLING LOS #####

n.samples <- 1000

upperlimsamp.amrs <- 500 ##  max ranges from DET ~500 days
upperlimsamp.dri <-7500 ## max difference in "outputs/los_all_amrdris.RData" is 15x
lowerlimsamp <- 0

#### take a while to run so loading in for now
set.seed(NULL) ## can't set seed with data table method as samples 1 value each time
## but much quicker than loop

load("data_inputs/lit_review/los_output_cc_DRI.RData")

### there's some duplication, remove 
los.output.cc <- los.output.cc[!duplicated(los.output.cc), ]

los.cc.DRI <- as.data.table(los.output.cc)
los.cc.DRI[ , ID := c(1:nrow(los.cc.DRI))]
rm(los.output.cc)

### !!! if negative value (due to 1 paper in this case, similar reasoning to susceptible)
### prior opinion that anyone with an infection should not cost less than anyone without (All other things being equal)
### use global average
test <- los.cc.DRI[TE.final<0] ## all BSI carbapenem - !!! would need to alter if used on different data
temp <- los.cc.DRI[syndrome=="BSI" &
                     class=="carbapenems"&
                     level=="global"]
temp <- temp[1,]

los.cc.DRI[TE.final<0 & syndrome=="BSI" &
              class=="carbapenems", TE.final:=temp$TE.final]
los.cc.DRI[TE.final<0 & syndrome=="BSI" &
              class=="carbapenems", TE.final:=temp$TE.final]

los.cc.DRI[TE.final<0 & syndrome=="BSI" &
             class=="carbapenems", TE.final:=temp$TE.final]
los.cc.DRI[TE.final<0 & syndrome=="BSI" &
             class=="carbapenems", TE.final:=temp$TE.final]

los.cc.thin.DRI <- los.cc.DRI[ , c("iso3c.x","whoc.region", "syndrome","class","gram.stain","TE.final","seTE.final","ID","no.studies","level")]
list.los.cc.DRI <- rep(list(los.cc.thin.DRI),n.samples)
sample.DRI <- rbindlist(list.los.cc.DRI, idcol=TRUE)

setnames(sample.DRI,".id","run")
sample.DRI[ , TE.final.rep := TE.final] # makes it easier to check
sample.DRI[ , TE.final.rep := rtruncnorm(1, a=lowerlimsamp, b=upperlimsamp.dri, mean=TE.final, sd=seTE.final)]
sample.DRI[ , TE.final := TE.final.rep] 
sample.DRI <- sample.DRI[ , -c("seTE.final","TE.final.rep")]
sample.DRI[ , DRI.flag := 1]
save(sample.DRI, file="outputs/sample_DRI_trunc.RData")

## and when TE<0 using global average as TE can be < minimum limit ??
#
load("data_inputs/lit_review/los_output_cc.RData")
### there's some duplication, remove 
los.output.cc <- los.output.cc[!duplicated(los.output.cc), ]

los.cc.AMR <- as.data.table(los.output.cc)
los.cc.AMR[ , ID := c(1:nrow(los.cc.AMR))]
rm(los.output.cc)

test <- los.cc.AMR[TE.final<0]
test[ , temp := paste0(syndrome,class,gram.stain)]
unique(test$temp)
# [1] "BSIcarbapenemsgn"       "COL/INFpenicillinsgp"
# [3] "RTI3g cephalosporinsgn" "RTIpenicillinsgp
#### !!! could make into a function but for now copying:
temp <- los.cc.AMR[syndrome=="BSI" &
                     class=="carbapenems"&
                     level=="global"]
temp <- temp[1,]
###1
los.cc.AMR[TE.final<0 & syndrome=="BSI" &
             class=="carbapenems", TE.final:=temp$TE.final]
los.cc.AMR[TE.final<0 & syndrome=="BSI" &
             class=="carbapenems", seTE.final:=temp$seTE.final]
los.cc.AMR[TE.final<0 & syndrome=="BSI" &
             class=="carbapenems", level:=temp$level]
los.cc.AMR[TE.final<0 & syndrome=="BSI" &
             class=="carbapenems", no.studies:=temp$no.studies]

###2
temp <- los.cc.AMR[syndrome=="COL/INF" &
                     class=="penicillins"&
                     level=="global"]
temp <- temp[1,]

los.cc.AMR[TE.final<0 &syndrome=="COL/INF" &
             class=="penicillins", TE.final:=temp$TE.final]
los.cc.AMR[TE.final<0 &syndrome=="COL/INF" &
             class=="penicillins", seTE.final:=temp$seTE.final]
los.cc.AMR[TE.final<0 &syndrome=="COL/INF" &
             class=="penicillins", level:=temp$level]
los.cc.AMR[TE.final<0 &syndrome=="COL/INF" &
             class=="penicillins", no.studies:=temp$no.studies]
###3
temp <- los.cc.AMR[syndrome=="RTI" &
                     class=="3g cephalosporins"&
                     level=="global"]
temp <- temp[1,]

los.cc.AMR[TE.final<0 & syndrome=="RTI" &
             class=="3g cephalosporins", TE.final:=temp$TE.final]
los.cc.AMR[TE.final<0 & syndrome=="RTI" &
             class=="3g cephalosporins", seTE.final:=temp$seTE.final]
los.cc.AMR[TE.final<0 & syndrome=="RTI" &
             class=="3g cephalosporins", level:=temp$level]
los.cc.AMR[TE.final<0 & syndrome=="RTI" &
             class=="3g cephalosporins", no.studies:=temp$no.studies]
###4
temp <- los.cc.AMR[syndrome=="RTI" &
                     class=="penicillins"&
                     level=="global"]
temp <- temp[1,]

los.cc.AMR[TE.final<0 & syndrome=="RTI" &
             class=="penicillins", TE.final:=temp$TE.final]
los.cc.AMR[TE.final<0 & syndrome=="RTI" &
             class=="penicillins", seTE.final:=temp$seTE.final]
los.cc.AMR[TE.final<0 & syndrome=="RTI" &
             class=="penicillins", level:=temp$level]
los.cc.AMR[TE.final<0 & syndrome=="RTI" &
             class=="penicillins", no.studies:=temp$no.studies]

los.cc.thin.AMR <- los.cc.AMR[ , c("iso3c.x","whoc.region", "syndrome","class","gram.stain",
                                   "TE.final","seTE.final","ID","no.studies","level")]
list.los.cc.AMR <- rep(list(los.cc.thin.AMR),n.samples)
sample.AMR <- rbindlist(list.los.cc.AMR, idcol=TRUE)

setnames(sample.AMR,".id","run")
sample.AMR[ , TE.final.rep := TE.final] # makes it easier to check
sample.AMR[ , TE.final.rep := rtruncnorm(1, a=lowerlimsamp, b=upperlimsamp.amrs, mean=TE.final, sd=seTE.final)]
sample.AMR[ , TE.final := TE.final.rep] 
sample.AMR <- sample.AMR[ , -c("seTE.final","TE.final.rep")]

save(sample.AMR, file="outputs/sample_AMR_trunc.RData")


### building similar samples for LOS from other sources
load("data_inputs/los_other.RData")
cases <- read.csv("data_inputs/case_deaths_impact_by_vaccine.csv")
cases <- as.data.table(cases)

combos <- unique(cases[,c('Infectious.syndrome',
                          'Pathogen','Antibiotic.class')])

## keep salmonella ones
salmonella <- combos[grepl('Salmonella', combos$Pathogen), ]

##### creating frame of data to input ###
load("data_inputs/lit_review/los_output_cc.RData")
### there's some duplication, remove 
los.output.cc <- los.output.cc[!duplicated(los.output.cc), ]

los.cc.AMR <- as.data.table(los.output.cc)
los.cc.AMR[ , ID := c(1:nrow(los.cc.AMR))]
rm(los.output.cc)

los.cc.thin.AMR <- los.cc.AMR[ , c("iso3c.x","whoc.region", "syndrome","class","gram.stain",
                                   "TE.final","seTE.final","ID","no.studies","level")]
list.los.cc.AMR <- rep(list(los.cc.thin.AMR),n.samples)

## create a country list to populate
### salmonella related los:
countries <- los.cc.AMR[!duplicated(los.cc.AMR$iso3c.x),]

countries <- rep(list(countries),3)
countries <- rbindlist(countries)

countries[ , syndrome:="INF"]
countries[ , class := "all"]
countries[,level:="other_source"]
countries[,no.studies:=NA]
countries[,ID:= c((max(sample.DRI$ID)+1):(max(sample.DRI$ID)+nrow(countries)))]

### there is a more efficient way of doing this but for now...
###!!! note will have to adapt this if have more "other" DRI LOS estimates
n3 <- nrow(countries)/3

countries[1:n3, gram.stain := "Salmonella Typhi"] ## not technically gram stain but need to identify
st <- los.other[microbe=="Salmonella Typhi"]
countries[1:n3, TE.final := st$lnm] ## note need to remember this isn't TE as estimated in AMR-UCR but shape/scale params
countries[1:n3, seTE.final := st$lnv]

countries[((n3+1):(n3+n3)), gram.stain := "Salmonella Paratyphi"] ## not technically gram stain but need to identify
st <- los.other[microbe=="Salmonella Paratyphi"]
countries[((n3+1):(n3+n3)), TE.final := st$lnm] ## note need to remember this isn't TE as estimated in AMR-UCR but shape/scale params
countries[((n3+1):(n3+n3)), seTE.final := st$lnv]

countries[((n3+n3+1):(n3*3)), gram.stain := "Non-typhoidal Salmonella"] ## not technically gram stain but need to identify
st <- los.other[microbe=="Non-typhoidal Salmonella"]
countries[((n3+n3+1):(n3*3)), TE.final := st$lnm] ## note need to remember this isn't TE as estimated in AMR-UCR but shape/scale params
countries[((n3+n3+1):(n3*3)), seTE.final := st$lnv]

countries <- countries[ , -c("Income.group" ,"wb.region" )]

los.cc.DRI.extra <- rep(list(countries),n.samples)
sample.DRI.other <- rbindlist(los.cc.DRI.extra, idcol=TRUE)

setnames(sample.DRI.other,".id","run")
sample.DRI.other[ , TE.final.rep := TE.final] # makes it easier to check
### lognormal distribution for salmonella DRI:
sample.DRI.other[ , TE.final.rep := exp(rtruncnorm(1, a=lowerlimsamp, b=upperlimsamp.dri, mean=TE.final, sd=seTE.final))]
sample.DRI.other[ , TE.final := TE.final.rep] 
sample.DRI.other <- sample.DRI.other[ , -c("seTE.final","TE.final.rep")]
save(sample.DRI.other, file="outputs/sample_dri_other_trunc.RData")

### do the same for the other length of stay (non salmonella)####
#### remember this time its AMR LOS

## clostridium difficile
countries <- los.cc.AMR[!duplicated(los.cc.AMR$iso3c.x),]
countries[ , syndrome:="GI"]
countries[ , class := "all"]
countries[,level:="other_source"]
countries[,no.studies:=NA]
countries[,ID:= c((max(sample.AMR$ID)+1):(max(sample.AMR$ID)+nrow(countries)))]
countries[, gram.stain := "Clostridium difficle"] ## not technically gram stain but need to identify
st <- los.other[microbe=="Clostridium difficle"]
countries[, TE.final := st$alpha.value] ## note need to remember this isn't TE as estimated in AMR-UCR but shape/scale params
countries[, seTE.final := st$beta.value]


## Cholera
countries2 <- los.cc.AMR[!duplicated(los.cc.AMR$iso3c.x),]
countries2[ , syndrome:="GI"]
countries2[ , class := "all"]
countries2[,level:="other_source"]
countries2[,no.studies:=NA]
countries2[, gram.stain := "Cholera"] ## not technically gram stain but need to identify
st <- los.other[microbe=="Cholera"]
countries2[, TE.final := st$alpha.value] ## note need to remember this isn't TE as estimated in AMR-UCR but shape/scale params
countries2[, seTE.final := st$beta.value]

## H.pylori
countries3 <- los.cc.AMR[!duplicated(los.cc.AMR$iso3c.x),]
countries3[ , syndrome:="GI"]
countries3[ , class := "all"]
countries3[,level:="other_source"]
countries3[,no.studies:=NA]
countries3[, gram.stain := "H.pylori"] ## not technically gram stain but need to identify
st <- los.other[microbe=="H.pylori"]
countries3[, TE.final := st$alpha.value] ## note need to remember this isn't TE as estimated in AMR-UCR but shape/scale params
countries3[, seTE.final := st$beta.value]

## Haemophilus influenzae
countries4 <- los.cc.AMR[!duplicated(los.cc.AMR$iso3c.x),]
countries4[ , syndrome:="INF"]
countries4[ , class := "all"]
countries4[,level:="other_source"]
countries4[,no.studies:=NA]
countries4[, gram.stain := "Haemophilus influenzae"] ## not technically gram stain but need to identify
st <- los.other[microbe=="Haemophilus influenzae"]
countries4[, TE.final := st$alpha.value] ## note need to remember this isn't TE as estimated in AMR-UCR but shape/scale params
countries4[, seTE.final := st$beta.value]

## Group A Streptococcus
countries5 <- los.cc.AMR[!duplicated(los.cc.AMR$iso3c.x),]
countries5[ , syndrome:="INF"]
countries5[ , class := "all"]
countries5[,level:="other_source"]
countries5[,no.studies:=NA]
countries5[, gram.stain := "Group A Streptococcus"] ## not technically gram stain but need to identify
st <- los.other[microbe=="Group A Streptococcus"]
countries5[, TE.final := st$alpha.value] ## note need to remember this isn't TE as estimated in AMR-UCR but shape/scale params
countries5[, seTE.final := st$beta.value]

countries.l <- list(countries, countries2,
                    countries3, countries4,
                    countries5)
countries.amr <- rbindlist(countries.l)
rm(countries.l)
gc()

countries.amr <- countries.amr[ , -c("Income.group" ,"wb.region" )]
countries.amr[,ID:= c((max(sample.AMR$ID)+1):(max(sample.AMR$ID)+nrow(countries.amr)))]


los.cc.AMR.extra <- rep(list(countries.amr),n.samples)
sample.AMR.other <- rbindlist(los.cc.AMR.extra, idcol=TRUE)

setnames(sample.AMR.other,".id","run")
sample.AMR.other[ , TE.final.rep := TE.final] # makes it easier to check
# sample.AMR.other[ , TE.final.rep := rtrunc(n = 1, spec = "gamma", shape = TE.final, 
#                                            scale = seTE.final, 
#                                            a = lowerlimsamp, b=upperlimsamp.amrs)]
### for some reason data table version not working any more so using loop

for (i in 1:nrow(sample.AMR.other)){
  shape <- sample.AMR.other[i,TE.final]
  scale <- sample.AMR.other[i,seTE.final]
  x <- rtrunc(n = 1, spec = "gamma", shape = shape, 
              scale = scale, 
              a = lowerlimsamp, b=upperlimsamp.amrs)
  sample.AMR.other[ i, TE.final.rep := x]
}


sample.AMR.other[ , TE.final := TE.final.rep] 
sample.AMR.other <- sample.AMR.other[ , -c("seTE.final","TE.final.rep")]

save(sample.AMR.other, file="outputs/sample_amr_other_trunc.RData")

##### COMBINING SAMPLES 

### remove EUSA from "other" sample data tables
### see setdiff(unique(sample.AMR.other$iso3c.x),unique(sample.AMR$iso3c.x) )

sample.DRI.other <- sample.DRI.other[iso3c.x !="EUSA"]
sample.AMR.other <- sample.AMR.other[iso3c.x !="EUSA"]
setdiff(unique(sample.AMR.other$iso3c.x),unique(sample.AMR$iso3c.x) )

sample.DRI <- rbind(sample.DRI, sample.DRI.other,fill=TRUE)
rm(sample.DRI.other)

sample.AMR <- rbind(sample.AMR, sample.AMR.other, fill=TRUE)
rm(sample.AMR.other)

## susceptible:
load("outputs/los_SE_TE.RData")
los.cc.S <- as.data.table(los.output.cc.S)

### !!! if negative value use global average
# temp <- los.cc.S[TE.final<=0]
# temp[ , combo := paste0(syndrome,class,gram.stain)]
# unique(temp$combo)
# ##"RTIcarbapenemsgn" - no global value to replace so remove

## !!! would need to alter here with different meta-analysis results
los.cc.S <- los.cc.S[TE.final>0]
los.cc.S[ , ID := c(1:nrow(los.cc.S))]

los.cc.thin.S <- los.cc.S[ , c("iso3c.x","whoc.region", "syndrome","class","gram.stain","TE.final",
                               "seTE.final","ID","no.studies","level")]
list.los.cc.S <- rep(list(los.cc.thin.S),n.samples)
sample.S <- rbindlist(list.los.cc.S, idcol=TRUE)

setnames(sample.S,".id","run") 

sample.S[ , TE.final.rep := TE.final] # makes it easier to check
sample.S[ , TE.final.rep := rtruncnorm(1, a=lowerlimsamp, b=upperlimsamp.amrs, 
                                         mean=TE.final, sd=seTE.final)]
sample.S[ , TE.final := TE.final.rep] 
sample.S <- sample.S[ , -c("seTE.final","TE.final.rep")]

save(sample.S, file="outputs/sample_S_trunc.RData")

### data wrangling to be able to merge 
#### in next iteration could remove the unite 
### as end up splitting out again to make more efficient


los.S <- sample.S %>%
  unite("group_id_c",iso3c.x:gram.stain) %>%
  group_by(group_id_c) %>% 
  as.data.table()

names(los.S)[names(los.S) == 'TE.final'] <- 'los.S'


los.AMR <- sample.AMR %>%
  unite("group_id_c",iso3c.x:gram.stain) %>%
  group_by(group_id_c) %>% 
  as.data.table()

names(los.AMR)[names(los.AMR) == 'TE.final'] <- 'los.AMR'

los.DRI <- sample.DRI %>%
  unite("group_id_c",iso3c.x:gram.stain) %>%
  group_by(group_id_c) %>% 
  as.data.table()

names(los.DRI)[names(los.DRI) == 'TE.final'] <- 'los.DRI'

AMR.S.temp <- merge(los.AMR, los.S,
                    by=c("group_id_c","run"), all.x=TRUE)

## separate out those where there is an S and those without
AMR.S <- AMR.S.temp[!is.na(los.S)]
AMR.noS <- AMR.S.temp[is.na(los.S)]

## calculate DRI
AMR.S[ ,los.DRI := los.S+los.AMR]
AMR.S[ , AMR.S.flag := 1]

## combine with DRI estimates from AMR-UCR
AMR.S.DRI <- AMR.S[ , c("group_id_c","run", "los.DRI")]
los.DRI <- los.DRI[ , c("group_id_c","run", "los.DRI","DRI.flag")]
DRI.all <- rbind(AMR.S.DRI,los.DRI, fill=TRUE)

## calculate the proportional difference between AMR & DRI 
## to use for those where we just have AMR costs 
### note don't use DRI values as DRI might be > AMR in sample
### could have e.g. ordering of values and linking or more complicated
## sampling combinations in future iterations !!!
AMR.S[ , prop.AMR2DRI := los.DRI/los.AMR]
AMR.S[, c("iso3c.x","whoc.region" ,"syndrome"  ,
          "class", "gram.stain") := tstrsplit(group_id_c, "_", fixed=TRUE)]

### !! future iterations might want to look at also
### proportional difference where have direct AMR and DRI,
### then split by syndrome
### but keeping with by class for now with the AMR & S data set
### as we want to maximise the use of data from AMR costs we have
### and apply across syndromes (that don't have other data)

prop.amr2dri <- AMR.S %>% group_by(whoc.region,class)%>%
  summarise(av_prop := median(prop.AMR2DRI, na.rm=TRUE))%>%
  as.data.table

## do another averages for by just region to apply to 
## e.g. glycopeptide where don't have one in prop.amr2dri
prop.amr2dri.general <- prop.amr2dri %>% group_by(whoc.region)%>%
  summarise(av_prop_gen := median(av_prop))

## merge into those with just AMR
AMR.noS[, c("iso3c.x","whoc.region" ,"syndrome"  ,
            "class", "gram.stain") := tstrsplit(group_id_c, "_", fixed=TRUE)]

## merge with prop 2 amr data
AMR.noS.p <- merge(AMR.noS, prop.amr2dri, by=c("whoc.region",
                                               "class"),all.x=TRUE)
AMR.noS.p <- merge(AMR.noS.p, prop.amr2dri.general, by=c("whoc.region"),all.x=TRUE)

## using the best available proportion
AMR.noS.p[ , prop := av_prop]
AMR.noS.p[is.na(prop) , prop := av_prop_gen]

## apply the proportional difference
AMR.noS.p[ , los.DRI := (los.AMR*prop)]

## combine with the DRI.all data
DRI.temp <- AMR.noS.p[ ,c("group_id_c"  ,"run" ,
                          "los.DRI")]

DRI.all <- rbind(DRI.all, DRI.temp, fill=TRUE)

# ### still have duplicates for those who are in multiple ways of calculating  DRI for that exposure group
x<- DRI.all[ , -c("los.DRI")]
x <- x[ ,-c("DRI.flag")]
x <- x[,list(Count=.N),names(x)]
unique(x$Count)
### there were more than 1s this time
y <- x[Count>1]


### some have large run --> really slow runs and also means different variables sampled different number of times

## merge in count variable
DRI.all.clean <- merge(DRI.all, x,by=c("group_id_c","run"))

## keep those with 1 run per group or >1 count from DRI sample (i.e. prioritise DRI extracted data if available)
DRI.all.clean <- DRI.all.clean[Count==1 |(Count>1 & DRI.flag==1)]

## create a new run id for each group id so as to not double count runs as cases/costs
DRI.all.clean <- DRI.all.clean[, run := sequence(.N), by = c("group_id_c")]

## remove unnecessary columns and replace old DRI.all to save
DRI.all.clean <- DRI.all.clean[ , -c("DRI.flag" ,  "Count"  )]
DRI.all <- DRI.all.clean

x<- DRI.all[ , -c("los.DRI")]
x <- x[,list(Count=.N),names(x)]
unique(x$Count)
y <- x[Count>1]
## should be 0 obs in y now 

save(DRI.all, file="outputs/DRI_UC_los_trunc.RData")


