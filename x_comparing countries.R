######### comparing countries

who_region_updated <- read.csv("data_all/who_region_updated.csv")

new.who <- who_region_updated$DimensionMemberCode

load("data_all/who_whoc_wb.RData")

old.who <- who_whoc_wb$iso3c

setdiff(new.who,old.who)
who_dic_diff <-setdiff(new.who,old.who)

x <- subset(who_whoc_wb, is.na(who_whoc_wb$whoc.region))
x

### cases 
load("data_inputs/epi_inputs_all.RData")

all_data <- unique(all_data$ISO3)

y <- setdiff(new.who,all_data)


## economic outputs
load("outputs/fulloutput_chunks/46.RData")

vaccine_output_dt <- unique(vaccine_output_dt$iso3c)

setdiff(new.who, vaccine_output_dt)

## unit cos