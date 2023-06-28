#### SAMPLE WHO CHOICE COSTS 
##### combining the estimates for AMR DRI and S ########

library(tidyverse)
library(data.table)

### functions
### taken from https://devinincerti.com/2018/02/10/psa.html#gamma-and-lognormal-distributions
## credit to Devin Incerti, 2018 for the lnorm_mom function
lnorm_mom <- function(mean, sd){
  if (mean > 0){
    sigma2 <- log((sd^2 + mean^2)/mean^2)
    mu <- log(mean) - 1/2 * sigma2
  } else{
    stop("Mean must be positive")
  }
  return(list(mu = mu, sigma2 = sigma2))
}

load("outputs/DRI_UC_los.RData")
n.samples <- 1000

##############******** SAMPLING WHO CHOICE ***********##########

load("data_inputs/lit_review/whoc_cc_2019USD.RData")
whoc.cc <- as.data.table(whoc.cc.2019)
as.numeric.factor <- function(x) {as.numeric(as.character(x))}
## get the pre-grouped WHOC cost estimates by country
whoc.cc <- whoc.cc[!is.na(iso3c)]
whoc.cc[ , ID := .GRP, by =.(iso3c)] ## create numeric ID variable

whoc.cc$mean_i <- as.numeric.factor(whoc.cc$mean_i) ##!!! make sure to use inflated costs "_i" 
whoc.cc$SD_i <- as.numeric.factor(whoc.cc$SD_i)

set.seed(190)
whoc.cc.thin <- whoc.cc[ , c("iso3c","mean_i","SD_i","ID")]
whoc.cc.full <- rep(list(whoc.cc.thin),n.samples)

for (i in 1:max(unique(whoc.cc$ID))){
  
  ## split into group wanted
  dt.temp <- whoc.cc[ID == i]
  
  lnorm.pars <- lnorm_mom(dt.temp$mean_i, dt.temp$SD_i)
  lnorm.sample <- rlnorm(n.samples, meanlog = lnorm.pars$mu, sdlog = sqrt(lnorm.pars$sigma2))
  
  for (j in 1:n.samples){
    whoc.cc.full[[j]][ID==i]$mean_i <- lnorm.sample[[j]]
    
  }
  
}
sample.whoc <- rbindlist(whoc.cc.full, use.names=TRUE, fill=TRUE, idcol="run")
sample.whoc <- sample.whoc[ , -c("SD_i")]

save(sample.whoc, file="outputs/sample_whoc.RData")

#### COMBINE TO GET A SAMPLE OF COSTS
DRI.all[, c("iso3c.x","whoc.region" ,"syndrome"  ,
            "class", "gram.stain") := tstrsplit(group_id_c, "_", fixed=TRUE)]


DRI.whoc <- merge(DRI.all,sample.whoc, by.x=c("iso3c.x",
                                              "run"),
                  by.y=c("iso3c","run"), all.x=TRUE,
                  all.y=FALSE)

test <- DRI.whoc[is.na(mean_i)]
unique(test$iso3c.x)
unique(test$whoc.region)
## "PRK" "SOM" "ZWE"
## use regional averages for these

whoc.cc.region <- as.data.table(whoc.cc.2019)
whoc.cc.region <- whoc.cc.region[(region=="SEARO D"|
                                    region== "EMRO D"|
                                    region=="AFRO E") &
                                   is.na(iso3c)] ###!!! could add more regions here
whoc.cc.region[ , ID := .GRP, by =.(region)] ## create numeric ID variable

whoc.cc.region$mean_i <- as.numeric.factor(whoc.cc.region$mean_i) ##!!! make sure to use inflated costs "_i" 
whoc.cc.region$SD_i <- as.numeric.factor(whoc.cc.region$SD_i)

set.seed(190)
whoc.cc.region.thin <- whoc.cc.region[ , c("region","mean_i","SD_i","ID")]
whoc.cc.region.full <- rep(list(whoc.cc.region.thin),n.samples)

for (i in 1:max(unique(whoc.cc.region$ID))){
  
  ## split into group wanted
  dt.temp <- whoc.cc.region[ID == i]
  
  lnorm.pars <- lnorm_mom(dt.temp$mean_i, dt.temp$SD_i)
  lnorm.sample <- rlnorm(n.samples, meanlog = lnorm.pars$mu, sdlog = sqrt(lnorm.pars$sigma2))
  
  for (j in 1:n.samples){
    whoc.cc.region.full[[j]][ID==i]$mean_i <- lnorm.sample[[j]]
    
  }
  
}
sample.whoc.region <- rbindlist(whoc.cc.region.full, use.names=TRUE, fill=TRUE, idcol="run")
sample.whoc.region <- sample.whoc.region[ , -c("SD_i")]

save(sample.whoc.region, file="outputs/sample_whoc_region.RData")


