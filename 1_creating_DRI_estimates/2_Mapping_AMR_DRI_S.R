########### MAPPING susceptible DATA 
############## PACKAGES #################

library(stringr)
library(dplyr)
library(data.table)
library(countrycode)
library(wbstats)

############ READ IN FILES ##############

load("outputs/los_all_amrdris.Rdata")  ## literature review DET
lit <- los.all

lit$row_id <- c(1:nrow(lit))

### download World bank classifications from WB stats
load("data_inputs/lit_review/dic_bug.RData") ## bacteria dictionary
load("data_inputs/lit_review/dic_drug.RData") ## antibiotic dictionary
load("data_inputs/lit_review/who_whoc_wb.RData") ## region dictionary

#######**** MAPPING BACTERIA ****####
# combine them 
bug <- merge(lit, dic_bug, by.x="bacteria.code", by.y="bacteria.code")

## change formatting to all lower-case
bug <- bug  %>% as.data.frame %>%  ## have to convert back to df for the next bit to work without error
  mutate_at(vars(c("exposed.R")), ~ str_to_lower(.))

#######**** MAPPING ANTIBIOTICS ****####

## make all lower case
dic_drug <- dic_drug %>% 
  mutate_at(vars(c("Drug.protein","Linked.antibiotic")), ~ str_to_lower(.))

## group ones that already are in our class and ones that need to be grouped
## take the ones we want from the drug dictionary
classy <- unique(dic_drug$Linked.antibiotic)
classy <- paste(classy,collapse="|")

table.input <- as.data.table(bug)
table.input[ , chf:= 0L]
table.input[grepl(classy,table.input$exposed.R), chf := 1]

### split into those by class and those not
class.y <- subset(table.input, chf==1)
class.n <- subset(table.input, chf==0)

## match the ones currently not at a class level
class.nm <- merge(class.n, dic_drug, by.x="exposed.R", by.y="Drug.protein",all=FALSE)

## rename columns to match (adding an extra column in class.y so they match in terms of width)
class.y$class <- class.y$exposed.R
colnames(class.nm)[colnames(class.nm)=="Linked.antibiotic"] <- "class"

## combine the newly matched dataset with the one which was already defined by class
bug_class <- union(class.y, class.nm)
## get just exposures of interest
bug_class <- bug_class[(gram.stain=="gp" & (class=="penicillins"|class=="glycopeptides"))|
                         (gram.stain=="gn" & (class=="3g cephalosporins"|class=="carbapenems"))|
                         gram.stain=="tb"] 

#########**** COMBINING ALL OF THE ABOVE ****#####
## convert input country to iso3c code using country code package
bug_class$iso3c <-countrycode(bug_class$country, origin="country.name", destination="iso3c") 

bug_class_region <- merge(bug_class, who_whoc_wb, by.x="iso3c", by.y="iso3c",all.x=TRUE)
is.na(bug_class_region$wb.region)

EuSA <- bug_class_region[country=="Europe"]
EuSA[ , wb.region := "Europe & Central Asia"]
EuSA <- merge(EuSA, who_whoc_wb, by="wb.region", allow.cartesian = TRUE)
EuSA <- EuSA %>% 
  select(!ends_with(".x")) %>% ## removing unneeded columns
  group_by(retrieval, bacteria.code, exposed.R, TE_S, who.region.y,whoc.region.y,Income.group.y) %>%
  ## might want to adapt this in future to be more specific 
  filter(row_number() == 1)%>% ## take just 1 per study + who.region/region combination
  as.data.table()
EuSA[ , iso3c.y := "EUSA"] ## put country code to NA so don't mistake evidence maps later
colnames(EuSA)[colnames(EuSA) == 'who.region.y'] <- 'who.region' ## match column names to data.table with main results
colnames(EuSA)[colnames(EuSA) == 'whoc.region.y'] <- 'whoc.region'
colnames(EuSA)[colnames(EuSA) == 'Income.group.y'] <- 'Income.group'
colnames(EuSA)[colnames(EuSA) == 'iso3c.y'] <- 'iso3c'

## remove "Low income" group as only 1/51 so might skew low income group results if all Europe results mapped to low income countries
EuSA <- EuSA[Income.group!="Low income"]
EuSA <- EuSA[Income.group!="Lower middle income"]
## set whoc regions to NA as not specifically targetting an individual WHOC region (e.g. EURO B) but rather the whole of Europe
EuSA <- unique(EuSA)

save(EuSA, file="outputs/EuSA_S.RData")

bug_class_region <- bug_class_region[!is.na(iso3c)]
bug_class_region <- rbind(bug_class_region, EuSA)

## clean up to only keep key columns
los_all_mapped <- bug_class_region[ , c("row_id","iso3c", "bacteria.code",
                                          "syndrome",
                                          "TE_AMR","TE_DRI","TE_S",
                                          "seTE_AMR","seTE_DRI","seTE_S",
                                          "gram.stain",
                                          "class","who.region","whoc.region","wb.region",
                                          "Income.group")]


# #save data
save(los_all_mapped, file="outputs/los_all_mapped.RData")
