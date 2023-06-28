TE_creator_S <- function(los.est){
  ### note this is different to TE_creator as removed/added
  ### some of the mapping columns at the end of the function
  los.est[ , n_exposed := as.numeric.factor(n_exposed)]
  los.est[ , n_nonexposed := as.numeric.factor(n_nonexposed)]
  los.est[ , avexp := as.numeric.factor(avexp)]
  los.est[ , avnon := as.numeric.factor(avnon)]
  los.est[ , lowexp := as.numeric.factor(lowexp)]
  los.est[ , highexp := as.numeric.factor(highexp)]
  los.est[ , otherexp := as.numeric.factor(otherexp)]
  los.est[ , lownon := as.numeric.factor(lownon)]
  los.est[ , highnon := as.numeric.factor(highnon)]
  los.est[ , othernon := as.numeric.factor(othernon)]
  los.est[ , measure := as.character(measure)]
  los.est[ , se.measure := as.character(se.measure)]
  
  ##*** had to assume symmetry to get quartiles 1 and 3 from median and IQR
  los.est[se.measure=="IQR+", lowexp:=avexp-(otherexp/2)]
  los.est[se.measure=="IQR+", highexp:=(otherexp/2)+avexp]
  los.est[se.measure=="IQR+", lownon:=avnon-(othernon/2)]
  los.est[se.measure=="IQR+", highnon:=(othernon/2)+avnon]
  los.est[se.measure=="IQR+", se.measure:="IQR"]
  
  ### adapting notation to fit meta r package namings etc.
  los.est[ , n.e := n_exposed]
  los.est[ , n.c := n_nonexposed]
  los.est[ , n.t := n.e + n.c]
  los.est[, pval := p]
  
  ### adding new variables needed from Wan
  ## for Excess (using n.t):
  los.est <- merge(los.est, Table1, by.x ="n.t", by.y="n", all.x=TRUE, all.y=FALSE)
  ## for exposed (using n.e):
  e <- Table1
  colnames(e) <- c("n","table1_exp")
  los.est <- merge(los.est, e, by.x="n.e",by.y="n",all.x=TRUE,all.y=FALSE)
  ## for nonexposed (using n.c):
  c <- Table1
  colnames(c) <- c("n","table1_non")
  los.est <- merge(los.est, c, by.x="n.c",by.y="n",all.x=TRUE,all.y=FALSE)
  rm(e)
  rm(c)
  
  ## create Q
  los.est[ , Qcase := round((n.t-1)/4)]
  los.est[ , Qcase_exp := round((n.e-1)/4)]
  los.est[ , Qcase_non := round((n.c-1)/4)]
  los.est <- merge(los.est, Table2, by.x ="Qcase", by.y="n", all.x=TRUE, all.y=FALSE)
  ## for exposed (using n.e):
  e <- Table2
  colnames(e) <- c("n","table2_exp")
  los.est <- merge(los.est, e, by.x="Qcase_exp",by.y="n",all.x=TRUE,all.y=FALSE)
  ## for nonexposed (using n.c):
  c <- Table2
  colnames(c) <- c("n","table2_non")
  los.est <- merge(los.est, c, by.x="Qcase_non",by.y="n",all.x=TRUE,all.y=FALSE)
  rm(e)
  rm(c)
  
  ##### EXCESS ALREADY GIVEN #####
  
  excess <- los.est[measure=="excess"|measure=="excess_median"]
  excess[, TE := avexp] ## ASSUMING SYMMETRICAL DISTRIBUTIONS - WILL BE REPLACED WITH MORE/BETTER INFO IF THERE IS SOME AVAILABLE 
  ## THROUGH BELOW CALCULATIONS
  excess[se.measure=="se", seTE := otherexp]
  excess[se.measure=="pvalue", seTE := TE/abs(qnorm(pval/2))]
  excess[se.measure=="95ci", seTE := (highexp-lowexp) / 3.92]
  excess[se.measure=="sd", seTE := otherexp/sqrt(n.t)] ## if SD given
  
  ## median + IRQ + range ## no rows as of yet
  excess[se.measure=="IQR.range", LB := ((minexp+lowexp+avexp+highexp)/4)+(((4*maxexp)-minexp-lowexp-avexp-highexp)/(4*n.t))]
  excess[se.measure=="IQR.range", UB := ((lowexp+avexp+highexp+maxexp)/4)+(((4*minexp)-lowexp-avexp-highexp-maxexp)/(4*n.t))]
  excess[se.measure=="IQR.range", TE := (LB+UB)/2]
  excess[se.measure=="IQR.range" & n.t > 50, sdTE := ((maxexp-minexp)/
                                                        (4*qnorm((n.t-0.375)/
                                                                   (n.t+0.25))))+
           ((highexp-lowexp)/
              (4*qnorm(((0.75*(n.t))-0.125)/
                         (n.t+0.25))))]
  excess[se.measure=="IQR.range" & n.t <= 50, sdTE:= 0.5*(((maxexp-minexp)/table1)+
                                                            ((highexp-lowexp)/table2))]
  
  ## median + IQR
  excess[se.measure=="IQR", TE := (lowexp+avexp+highexp)/3]
  excess[se.measure=="IQR" & n.t <=50, 
         sdTE := (highexp-lowexp)/table2]
  excess[se.measure=="IQR" & n.t >50, 
         sdTE := (highexp-lowexp)/2*(qnorm(((0.75*n.t)-0.125)/
                                             (n.t+0.25)))]
  # ## median + range
  excess[measure=="excess_median" & se.measure=="range", TE := ((lowexp+(2*avexp)+highexp)/4)+((lowexp-(2*avexp)+highexp)/(4*n.t))]
  excess[measure=="excess" & se.measure=="range", TE := avexp]
  
  # ## mean + range (SE calculation same for mean or median + range)
  excess[se.measure=="range" & n.t <= 50,
         sdTE := (highexp - lowexp)/table1]
  excess[se.measure=="range" & n.t > 50,
         sdTE := (highexp-lowexp)/2*(qnorm((n.t-0.375)/
                                             (n.t+0.25)))]
  
  ########### ESTIMATING TE DIFFERENCE FROM EXPOSED V NONEXPOSED ######
  ### Convert all to mean and SD using Wan et al (2014) formulae
  diff2calc <- los.est[measure!="excess"& measure!="excess_median"]
  
  ## mean and SE values given
  diff2calc[measure=="mean", mean_exp := avexp]
  diff2calc[se.measure=="se", se_exp := otherexp]
  diff2calc[measure=="mean", mean_non := avnon]
  diff2calc[se.measure=="se", se_non := othernon]
  
  ## confidence intervals for both - assumes normal distribution
  diff2calc[se.measure=="95ci", mean_exp := avexp]
  diff2calc[se.measure=="95ci", se_exp := (highexp-lowexp) / 3.92]
  diff2calc[se.measure=="95ci", mean_non := avnon]
  diff2calc[se.measure=="95ci", se_non := (highnon-lownon) / 3.92]
  
  ## SD given
  diff2calc[se.measure=="sd", mean_exp := avexp]
  diff2calc[se.measure=="sd", sd_exp := otherexp]
  diff2calc[se.measure=="sd", mean_non := avnon]
  diff2calc[se.measure=="sd", sd_non := othernon]
  
  ##############CASE DATA #####
  ## median + IRQ + range
  diff2calc[se.measure=="IQR.range", LB := ((minexp+lowexp+avexp+highexp)/4)+(((4*maxexp)-minexp-lowexp-avexp-highexp)/(4*n.e))]
  diff2calc[se.measure=="IQR.range", UB := ((lowexp+avexp+highexp+maxexp)/4)+(((4*minexp)-lowexp-avexp-highexp-maxexp)/(4*n.e))]
  diff2calc[se.measure=="IQR.range", mean_exp := (LB+UB)/2]
  diff2calc[se.measure=="IQR.range" & n.e > 50, sd_exp := ((maxexp-minexp)/
                                                             (4*qnorm((n.e-0.375)/
                                                                        (n.e+0.25))))+
              ((highexp-lowexp)/
                 (4*qnorm((0.75(n.e)-0.125)/
                            (n.e+0.25))))]
  diff2calc[se.measure=="IQR.range" & n.e <= 50, 
            sd_exp:= 0.5*(((maxexp-minexp)/
                             table1_exp)+((highexp-lowexp)/
                                            table2_exp))]
  ## median + IQR
  diff2calc[se.measure=="IQR", mean_exp := (lowexp+avexp+highexp)/3]
  diff2calc[se.measure=="IQR" & n.e <=50, 
            sd_exp := (highexp-lowexp)/table2_exp]
  diff2calc[se.measure=="IQR" & n.e >50, 
            sd_exp := (highexp-lowexp)/2*(qnorm(((0.75*n.e)-0.125)/
                                                  (n.e+0.25)))]
  ## median + range
  diff2calc[measure=="median" & se.measure=="range", 
            mean_exp := ((lowexp+(2*avexp)+highexp)/4)+((lowexp-(2*avexp)+highexp)/(4*n.e))]
  diff2calc[se.measure=="range" & n.e <= 50, 
            sd_exp := (highexp - lowexp)/table1_exp]
  diff2calc[se.measure=="range" & n.e > 50, 
            sd_exp := (highexp-lowexp)/2*(qnorm((n.e-0.375)/
                                                  (n.e+0.25)))]
  
  ###################### CONTROL DATA ########
  ## median + IRQ + range
  diff2calc[se.measure=="IQR.range", LB := ((minnon+lownon+avnon+highnon)/4)+(((4*maxnon)-minnon-lownon-avnon-highnon)/(4*n.c))]
  diff2calc[se.measure=="IQR.range", UB := ((lownon+avnon+highnon+maxnon)/4)+(((4*minnon)-lownon-avnon-highnon-maxnon)/(4*n.c))]
  diff2calc[se.measure=="IQR.range", mean_non := (LB+UB)/2]
  diff2calc[se.measure=="IQR.range" & n.c > 50, sd_non := ((maxnon-minnon)/
                                                             (4*qnorm((n.c-0.375)/
                                                                        (n.c+0.25))))+
              ((highnon-lownon)/
                 (4*qnorm((0.75(n.c)-0.125)/
                            (n.c+0.25))))]
  diff2calc[se.measure=="IQR.range" & n.c <= 50, 
            sd_non:= 0.5*(((maxnon-minnon)/
                             table1_non)+((highnon-lownon)/
                                            table2_non))]
  ## median + IQR
  diff2calc[se.measure=="IQR", mean_non := (lownon+avnon+highnon)/3]
  diff2calc[se.measure=="IQR" & n.c <=50, 
            sd_non := (highnon-lownon)/table2_non]
  diff2calc[se.measure=="IQR" & n.c >50, 
            sd_non := (highnon-lownon)/2*(qnorm(((0.75*n.c)-0.125)/
                                                  (n.c+0.25)))]
  ## median + range
  diff2calc[measure=="median" & se.measure=="range",
            mean_non := ((lownon+(2*avnon)+highnon)/4)+
              ((lownon-(2*avnon)+highnon)/(4*n.c))]
  diff2calc[se.measure=="range" & n.c <= 50, 
            sd_non := (highnon - lownon)/table1_non]
  diff2calc[se.measure=="range" & n.c > 50, 
            sd_non := (highexp-lowexp)/2*(qnorm((n.c-0.375)/
                                                  (n.c+0.25)))]
  
  diff2calc[measure=="mean", mean_exp := avexp]
  diff2calc[measure=="mean", mean_non := avnon]
  # with lack of other data..assuming symmetrical & same outcome measure (mean difference)
  diff2calc[measure=="median" & se.measure=="pvalue", mean_exp := avexp]
  diff2calc[measure=="median" & se.measure=="pvalue", mean_non := avnon]
  
  ##### convert SD to SE ######
  excess[is.na(seTE) & !is.na(sdTE), seTE := sdTE/sqrt(n.t)]
  
  ## getting errors from se and sd measure as character 
  diff2calc[  , sd_exp := as.numeric(sd_exp)]
  diff2calc[  , se_exp := as.numeric(se_exp)]
  
  diff2calc[is.na(se_exp) & !is.na(sd_exp), se_exp := sd_exp/sqrt(n.e)]
  diff2calc[is.na(se_non) & !is.na(sd_non), se_non := sd_non/sqrt(n.c)]
  
  ###### calculate mean of difference & SE of difference #######
  diff2calc[ , TE := mean_exp - mean_non]
  diff2calc[ , seTE := sqrt(((se_exp)^2)+((se_non)^2))]
  diff2calc[(measure=="median"|measure=="mean") & se.measure=="pvalue", seTE := TE/abs(qnorm(pval/2))]
  
  #### create data combining the different groups
  
  ## clean up the columsn for each
  excess <- excess[ , c("retrieval","study_ref","los","bacteria.code","exposed.R",
                        "syndrome","country",
                        "n.e","n.c","n.t","TE","seTE",  "unique_id_4both")]
  diff2calc <- diff2calc[ , c("retrieval","study_ref","los","bacteria.code","exposed.R",
                              "syndrome","country",
                              "n.e","n.c","n.t","TE","seTE", "unique_id_4both")]
  ## bind
  los.TE <- rbindlist(list(excess,diff2calc))
  
  return(los.TE)
}

meta.grouping.S <- function(x){
  
  # x <- los.TE ## use when testing function changes (REMEMBER TO NOT USE IN MAIN FUNCTION)
  
  # x is either "los.TE" or "costing.TE.adj" from the previous scripts
  # output is a data.table with estimates for the groupings which we have
  # atleast global estimates for
  
  l.est <- as.data.table(x)
  ## remove NA values - !!! check which ones get dropped
  l.est <- l.est[!is.na(TE)] ## none dropped in last running 
  l.est <- l.est[!is.na(seTE)] ## none dropped in last running
  l.est <- l.est[!seTE==Inf] ## 1 dropped for LOS - Spindel et al 2005 for los.TE, 0 for costing.TE
  l.est <- l.est[!seTE==0] ## 1 dropped for LOS - Ericson et al 2015 , 0 for costing.TE
  l.est[ , seTE := abs(seTE)]
  l.est[ , Income.group := as.character(Income.group)]
  # l.est <- l.est[!is.na(Income.group)]
  # l.est <- l.est[!is.na(whoc.region)]
  
  ###### creating all options #####
  ### creating the data.table that then fill with all combinations
  region <- c(unique(who_whoc_wb$iso3c), "EuSA")
  n.region <- length(region)
  ## number of syndromes is currently just for any we have estimates for
  synd <- unique(l.est$syndrome)
  n.synd <- length(synd)
  synd.tb <- c("RTI")
  n.synd.tb <- length(synd.tb)
  
  ### define classes of interest per gram stain/type
  class.gp <- c("glycopeptides","penicillins")              
  class.gn <- c("3g cephalosporins","carbapenems")    
  class.tb <- c("mdr","xdr")
  
  ##GP
  output.gp <- data.table(class=rep(class.gp, (n.synd*n.region)))
  output.gp[ , syndrome := rep(synd, each=((length(class.gp))), len=nrow(output.gp))]
  output.gp[ , region := rep(region, each=n.synd*((length(class.gp))) , len=nrow(output.gp))]
  output.gp[ , gram.stain := "gp"]
  ##GN
  output.gn <- data.table(class=rep(class.gn, (n.synd*n.region)))
  output.gn[ , syndrome := rep(synd, each=((length(class.gn))), len=nrow(output.gn))]
  output.gn[ , region := rep(region, each=n.synd*((length(class.gn))) , len=nrow(output.gn))]
  output.gn[ , gram.stain := "gn"]
  ##TB
  output.tb <- data.table(class=rep(class.tb, (n.synd.tb*n.region)))
  output.tb[ , syndrome := rep(synd.tb, each=((length(class.tb))), len=nrow(output.tb))]
  output.tb[ , region := rep(region, each=n.synd.tb*((length(class.tb))) , len=nrow(output.tb))]
  output.tb[ , gram.stain := "tb"]
  
  ## combine into one dataset
  l = list(output.gp,output.gn, output.tb)
  dt.all <- rbindlist(l, use.names=TRUE)
  rm(l)
  colnames(dt.all)[colnames(dt.all) == 'region'] <- 'iso3c'
  dt.all <- merge(dt.all, who_whoc_wb, by="iso3c")
  dt.all <- merge(dt.all, l.est, by=c("iso3c","syndrome", "class", "gram.stain"), all.x = TRUE, all.y=TRUE)
  dt.all <- dt.all[ ,-c("who.region.y" ,
                        "whoc.region.y","wb.region.y",
                        "Income.group.y")]
  colnames(dt.all)[colnames(dt.all) == 'who.region.x'] <- 'who.region'
  colnames(dt.all)[colnames(dt.all) == 'whoc.region.x'] <- 'whoc.region'
  colnames(dt.all)[colnames(dt.all) == 'wb.region.x'] <- 'wb.region'
  colnames(dt.all)[colnames(dt.all) == 'Income.group.x'] <- 'Income.group'
  
  ## relabelling those not in WHOC list
  dt.all[iso3c=="TWN", wb.region:="East Asia & Pacific"]
  dt.all[iso3c=="TWN", Income.group:="High income"]
  dt.all[iso3c=="HKG", wb.region:="East Asia & Pacific"]
  dt.all[iso3c=="HKG", Income.group:="High income"]
  
  ## adding EuSA region
  # get unique combinations from EuSA.RData
  EuSA <- EuSA %>% group_by(who.region,wb.region,Income.group) %>%
    filter(row_number() == 1)%>% ## take just 1 per region combination
    select("iso3c","who.region","whoc.region","Income.group","wb.region") %>%
    as.data.table()
  
  ## merge into dt.all 
  # get the EuSA ones:
  temp.EuSA <- dt.all[iso3c=="EUSA"]
  temp.EuSA <- merge(temp.EuSA, EuSA, by="iso3c", allow.cartesian = TRUE)
  ## allow.cartesian as want for all in temp.EuSA to be matched with each group
  
  temp.EuSA <- temp.EuSA %>% 
    select(!ends_with(".x")) %>% ## removing unneeded columns
    group_by(row_id, syndrome, class, gram.stain, who.region.y,Income.group.y) %>%
    filter(row_number() == 1)%>% ## take just 1 per study & bug/drug/syndrome + who.region/region combination
    as.data.table()
  
  colnames(temp.EuSA)[colnames(temp.EuSA) == 'who.region.y'] <- 'who.region' ## match column names to data.table with main results
  colnames(temp.EuSA)[colnames(temp.EuSA) == 'whoc.region.y'] <- 'whoc.region'
  colnames(temp.EuSA)[colnames(temp.EuSA) == 'wb.region.y'] <- 'wb.region'
  colnames(temp.EuSA)[colnames(temp.EuSA) == 'Income.group.y'] <- 'Income.group'
  
  dt.all <- dt.all[iso3c!="EUSA"]
  dt.all <- rbind(dt.all, temp.EuSA)
  
  ### !!! note because of this - for who region (not used), WB region 
  ## and global groupings, you need to remove duplicate rows from EUSA 
  ## manip before counting otherwise will double count/use studies
  ## not needed for income groups as that's the duplication
  
  dt.all[ , group_whoc := .GRP, by =.(whoc.region, syndrome, class, gram.stain)]
  dt.all[ , group_income := .GRP, by =.(Income.group,syndrome, class, gram.stain)]
  dt.all[ , group_wbregion := .GRP, by =.(wb.region,syndrome, class, gram.stain)]
  dt.all[ , group_global := .GRP, by =.(syndrome, class, gram.stain)]
  
  
  #### grouping information by WHOC regions ######
  
  n.studies <- dt.all %>%
    filter(!is.na(whoc.region)) %>% ## remove NA values otherwise grouped into own group
    as.data.table()
  ## have to break pipe to use data.table unique
  n.studies <- unique(n.studies,by=c("group_whoc","row_id"))  ## removing duplicate studies from region mapping
  n.studies <- n.studies %>% 
    group_by(group_whoc) %>% 
    count(!is.na(TE)) 
  
  n.studies <- as.data.table(n.studies)
  colnames(n.studies)[colnames(n.studies) == '!is.na(TE)'] <- 'any'
  n.studies[any== FALSE, n := 0]
  
  ## if n=1 just use that study
  n.studies.1 <- subset(n.studies, n==1)
  n.1 <- merge(n.studies.1, dt.all, by="group_whoc")
  n.1 <- n.1[ , c("whoc.region","syndrome", "class", "gram.stain","TE","seTE", "group_whoc","group_income","group_global","n")]
  n.1 <- n.1[!is.na(TE)]
  
  ## form final list through merging n.1 and n.2
  # get unique group rows from dt all
  setkey(dt.all, group_whoc)
  dt.output <- dt.all[J(unique(group_whoc)), mult = "first"]
  dt.output[ , TE := NA]
  dt.output[ , seTE := NA]
  
  n.1 <- n.1[ , c("group_whoc", "TE","seTE","n")]
  dt.output <- merge(dt.output, n.1, by="group_whoc", all.x=TRUE, all.y=FALSE)
  dt.output[ , TE.x := TE.y]
  dt.output[ , seTE.x := seTE.y]
  dt.output <- dt.output[ , -c("TE.y","seTE.y")]
  
  ## if n>1 then meta-analysis
  n.studies.2 <- subset(n.studies, n>1)
  t <- as.numeric(nrow(n.studies.2))
  if (t>0){
    n.2  <- merge(n.studies.2, dt.all, c("group_whoc"))
    n.2  <- n.2[!is.na(TE)]
    n.2[ , group_ID := .GRP, by =.(whoc.region, syndrome, class, gram.stain)]
    n.2 <- unique(n.2, by=c("group_whoc","row_id"))
    
    # output for preallocation
    output.meta <- data.table(TE = rep(0,max(unique(n.2$group_ID))),
                              seT = rep(0,max(unique(n.2$group_ID))),
                              lowerT = rep(0,max(unique(n.2$group_ID))),
                              upperT = rep(0,max(unique(n.2$group_ID))),
                              group_ID= rep(0,max(unique(n.2$group_ID))),
                              group_whoc=rep(0,max(unique(n.2$group_ID))))
    
    for (i in 1:max(unique(n.2$group_ID))){
      
      ## split into group wanted
      dt.temp <- n.2[group_ID == i]
      
      #### metagen() function when have SE value
      m.dl <- metagen(TE,
                      seTE,
                      data=dt.temp,
                      comb.fixed = FALSE,
                      comb.random = TRUE,
                      hakn = FALSE,
                      prediction=TRUE,
                      sm="SMD")
      
      ## extracting out the variables
      output.meta[i,TE := m.dl$TE.random]
      output.meta[i,seT := m.dl$seTE.random]
      output.meta[i,lowerT := m.dl$lower.random]
      output.meta[i,upperT := m.dl$upper.random] 
      output.meta[i, group_ID := i]
      output.meta[i, group_whoc := min(dt.temp$group_whoc)]
      output.meta[i, n := m.dl$k]
      
    }
    
    output.meta <- output.meta[ ,c("group_whoc","TE","seT","n")]
    dt.output <- merge(dt.output, output.meta, by="group_whoc", all.x=TRUE, all.y=FALSE)
    dt.output[is.na(TE.x), TE.x := TE]
    dt.output[is.na(seTE.x), seTE.x := seT]
    dt.output[is.na(n.x), n.x:= n.y]
    dt.output <- dt.output[ , -c("TE","seT","n.y")]
    
  }
  
  ## see which have no estimates
  dt.na.whoc <- dt.output[is.na(dt.output$TE.x)]
  
  ######## grouping by WB Income Classification ########
  n.studies <- dt.all %>%
    filter(!is.na(Income.group)) %>% ## remove NA values otherwise grouped into own group
    as.data.table()
  ## have to break pipe to use data.table unique
  n.studies <- unique(n.studies,by=c("group_income","row_id"))  ## removing duplicate studies from region mapping
  n.studies <- n.studies %>% 
    group_by(group_income) %>% 
    count(!is.na(TE)) 
  
  n.studies <- as.data.table(n.studies)
  colnames(n.studies)[colnames(n.studies) == '!is.na(TE)'] <- 'any'
  n.studies[any== FALSE, n := 0]
  
  ## if n=1 just use that study
  n.studies.1 <- subset(n.studies, n==1)
  n.1 <- merge(n.studies.1, dt.all, by="group_income")
  n.1 <- n.1[!is.na(TE)]
  # get unique group rows from dt all
  setkey(dt.all, group_income)
  dt.output.income <- dt.all[J(unique(group_income)), mult = "first"]
  dt.output.income[ , TE := NA]
  dt.output.income[ , seTE := NA]
  
  n.1 <- n.1[ , c("group_income", "TE","seTE","n")]
  dt.output.income <- merge(dt.output.income, n.1, by="group_income", all.x=TRUE, all.y=FALSE)
  dt.output.income[ , TE.x := TE.y]
  dt.output.income[ , seTE.x := seTE.y]
  dt.output.income <- dt.output.income[ , -c("TE.y","seTE.y")]
  
  ## if n>1 then meta-analysis
  n.studies.2 <- subset(n.studies, n>1)
  t <- as.numeric(nrow(n.studies.2))
  if (t>0){
    n.2  <- merge(n.studies.2, dt.all, by="group_income")
    n.2 <- n.2[!is.na(TE)]
    n.2[ , group_ID := .GRP, by =.(Income.group, syndrome, class, gram.stain)]
    n.2 <- unique(n.2, by=c("group_income","row_id"))
    
    # output for preallocation
    output.meta <- data.table(TE = rep(0,max(unique(n.2$group_ID))),
                              seT = rep(0,max(unique(n.2$group_ID))),
                              lowerT = rep(0,max(unique(n.2$group_ID))),
                              upperT = rep(0,max(unique(n.2$group_ID))),
                              group_ID= rep(0,max(unique(n.2$group_ID))),
                              group_income =rep(0,max(unique(n.2$group_ID))))
    
    for (i in 1:max(unique(n.2$group_ID))){
      
      ## split into group wanted
      dt.temp <- n.2[group_ID == i]
      
      #### metagen() function when have SE value
      m.dl <- metagen(TE,
                      seTE,
                      data=dt.temp,
                      comb.fixed = FALSE,
                      comb.random = TRUE,
                      hakn = FALSE,
                      prediction=TRUE,
                      sm="SMD")
      
      ## extracting out the variables
      output.meta[i,TE := m.dl$TE.random]
      output.meta[i,seT := m.dl$seTE.random]
      output.meta[i,lowerT := m.dl$lower.random]
      output.meta[i,upperT := m.dl$upper.random] 
      output.meta[i, group_ID := i]
      output.meta[i, group_income := min(dt.temp$group_income)]
      output.meta[i, n := m.dl$k]
      
    }
    
    output.meta <- output.meta[ ,c("group_income","TE","seT","n")]
    dt.output.income <- merge(dt.output.income, output.meta, by="group_income", all.x=TRUE, all.y=FALSE)
    dt.output.income[is.na(TE.x), TE.x := TE]
    dt.output.income[is.na(seTE.x), seTE.x := seT]
    dt.output.income[is.na(n.x), n.x:= n.y]
    dt.output.income <- dt.output.income[ , -c("TE","seT","n.y")]
    
  }
  
  dt.na.income <- dt.output.income[is.na(dt.output.income$TE.x)]
  
  ###### grouping by WB regional estimates ###################
  
  n.studies <- dt.all %>%
    filter(!is.na(wb.region)) %>% ## remove NA values otherwise grouped into own group
    as.data.table()
  ## have to break pipe to use data.table unique
  n.studies <- unique(n.studies,by=c("group_wbregion","row_id"))  ## removing duplicate studies from region mapping
  n.studies <- n.studies %>% 
    group_by(group_wbregion) %>% 
    count(!is.na(TE)) 
  
  n.studies <- as.data.table(n.studies)
  colnames(n.studies)[colnames(n.studies) == '!is.na(TE)'] <- 'any'
  n.studies[any== FALSE, n := 0]
  
  ## if n=1 just use that study
  n.studies.1 <- subset(n.studies, n==1)
  n.1 <- merge(n.studies.1, dt.all, by="group_wbregion")
  n.1 <- n.1[!is.na(TE)]
  # get unique group rows from dt all
  setkey(dt.all, group_wbregion)
  dt.output.wbregion <- dt.all[J(unique(group_wbregion)), mult = "first"]
  dt.output.wbregion[ , TE := NA]
  dt.output.wbregion[ , seTE := NA]
  
  n.1 <- n.1[ , c("group_wbregion", "TE","seTE","n")]
  dt.output.wbregion <- merge(dt.output.wbregion, n.1, by="group_wbregion", all.x=TRUE, all.y=FALSE)
  dt.output.wbregion[ , TE.x := TE.y]
  dt.output.wbregion[ , seTE.x := seTE.y]
  dt.output.wbregion <- dt.output.wbregion[ , -c("TE.y","seTE.y")]
  
  ## if n>1 then meta-analysis
  n.studies.2 <- subset(n.studies, n>1)
  t <- as.numeric(nrow(n.studies.2))
  if (t>0){
    n.2  <- merge(n.studies.2, dt.all, by="group_wbregion")
    n.2 <- n.2[!is.na(TE)]
    n.2[ , group_ID := .GRP, by =.(wb.region, syndrome, class, gram.stain)]
    n.2 <- unique(n.2, by=c("group_wbregion","row_id"))
    
    # output for preallocation
    output.meta <- data.table(TE = rep(0,max(unique(n.2$group_ID))),
                              seT = rep(0,max(unique(n.2$group_ID))),
                              lowerT = rep(0,max(unique(n.2$group_ID))),
                              upperT = rep(0,max(unique(n.2$group_ID))),
                              group_ID= rep(0,max(unique(n.2$group_ID))),
                              group_wbregion =rep(0,max(unique(n.2$group_ID))))
    
    for (i in 1:max(unique(n.2$group_ID))){
      
      ## split into group wanted
      dt.temp <- n.2[group_ID == i]
      
      #### metagen() function when have SE value
      m.dl <- metagen(TE,
                      seTE,
                      data=dt.temp,
                      comb.fixed = FALSE,
                      comb.random = TRUE,
                      hakn = FALSE,
                      prediction=TRUE,
                      sm="SMD")
      
      ## extracting out the variables
      output.meta[i,TE := m.dl$TE.random]
      output.meta[i,seT := m.dl$seTE.random]
      output.meta[i,lowerT := m.dl$lower.random]
      output.meta[i,upperT := m.dl$upper.random] 
      output.meta[i, group_ID := i]
      output.meta[i, group_wbregion := min(dt.temp$group_wbregion)]
      output.meta[i, n := m.dl$k]
      
    }
    
    output.meta <- output.meta[ ,c("group_wbregion","TE","seT","n")]
    dt.output.wbregion <- merge(dt.output.wbregion, output.meta, by="group_wbregion", all.x=TRUE, all.y=FALSE)
    dt.output.wbregion[is.na(TE.x), TE.x := TE]
    dt.output.wbregion[is.na(seTE.x), seTE.x := seT]
    dt.output.wbregion[is.na(n.x), n.x:= n.y]
    dt.output.wbregion <- dt.output.wbregion[ , -c("TE","seT","n.y")]
    
  }
  
  dt.na.wbregion <- dt.output.wbregion[is.na(dt.output.wbregion$TE.x)]
  
  ###### grouping by global estimates ###################
  
  ## have to break pipe to use data.table unique
  n.studies <- unique(dt.all,by=c("row_id"))  ## removing duplicate studies from region mapping
  n.studies <- n.studies %>% 
    group_by(group_global) %>% 
    count(!is.na(TE)) 
  
  
  n.studies <- as.data.table(n.studies)
  colnames(n.studies)[colnames(n.studies) == '!is.na(TE)'] <- 'any'
  n.studies[any== FALSE, n := 0]
  
  ## if n=1 just use that study
  n.studies.1 <- subset(n.studies, n==1)
  n.1 <- merge(n.studies.1, dt.all, by="group_global")
  n.1 <- n.1[ , c("syndrome", "class", "gram.stain","TE","seTE", "group_whoc","group_income","group_global","n")]
  n.1 <- n.1[!is.na(TE)]
  
  ## form final list through merging n.1 and n.2
  # get unique group rows from dt all
  setkey(dt.all, group_global)
  dt.output.global <- dt.all[J(unique(group_global)), mult = "first"]
  dt.output.global[ , TE := NA]
  dt.output.global[ , seTE := NA]
  
  n.1 <- n.1[ , c("group_global", "TE","seTE","n")]
  dt.output.global <- merge(dt.output.global, n.1, by="group_global", all.x=TRUE, all.y=FALSE)
  dt.output.global[ , TE.x := TE.y]
  dt.output.global[ , seTE.x := seTE.y]
  dt.output.global <- dt.output.global[ , -c("TE.y","seTE.y")]
  
  ## if n>1 then meta-analysis
  n.studies.2 <- subset(n.studies, n>1)
  t <- as.numeric(nrow(n.studies.2))
  if (t>0){
    n.2  <- merge(n.studies.2, dt.all, by="group_global")
    n.2 <- n.2[!is.na(TE)]
    n.2[ , group_ID := .GRP, by =.(syndrome, class, gram.stain)]
    n.2 <- unique(n.2, by=c("row_id"))
    
    # output for preallocation
    output.meta <- data.table(TE = rep(0,max(unique(n.2$group_ID))),
                              seT = rep(0,max(unique(n.2$group_ID))),
                              lowerT = rep(0,max(unique(n.2$group_ID))),
                              upperT = rep(0,max(unique(n.2$group_ID))),
                              group_ID= rep(0,max(unique(n.2$group_ID))),
                              group_global =rep(0,max(unique(n.2$group_ID))))
    
    for (i in 1:max(unique(n.2$group_ID))){
      
      ## split into group wanted
      dt.temp <- n.2[group_ID == i]
      
      #### metagen() function when have SE value
      m.dl <- metagen(TE,
                      seTE,
                      data=dt.temp,
                      comb.fixed = FALSE,
                      comb.random = TRUE,
                      hakn = FALSE,
                      prediction=TRUE,
                      sm="SMD")
      
      ## extracting out the variables
      output.meta[i,TE := m.dl$TE.random]
      output.meta[i,seT := m.dl$seTE.random]
      output.meta[i,lowerT := m.dl$lower.random]
      output.meta[i,upperT := m.dl$upper.random] 
      output.meta[i, group_ID := i]
      output.meta[i, group_global := min(dt.temp$group_global)]
      output.meta[i, n := m.dl$k]
      
    }
    
    output.meta <- output.meta[ ,c("group_global","TE","seT","n")]
    dt.output.global <- merge(dt.output.global, output.meta, by="group_global", all.x=TRUE, all.y=FALSE)
    dt.output.global[is.na(TE.x), TE.x := TE]
    dt.output.global[is.na(seTE.x), seTE.x := seT]
    dt.output.global[is.na(n.x), n.x:= n.y]
    dt.output.global <- dt.output.global[ , -c("TE","seT","n.y")]
    
  }
  
  dt.na.global <- dt.output.global[is.na(dt.output.global$TE.x)]
  dt.na.global <- dt.na.global[ , c("syndrome","class","gram.stain")]
  # write.csv(dt.na.global, "cost_per_case/outputs/missing_global_combinations.csv")
  
  ### combining altogether
  dt.output <- dt.output[ ,c("iso3c","whoc.region","syndrome","class","gram.stain",
                             "TE.x","seTE.x","group_whoc","n.x")]
  setnames(dt.output, "TE.x", "TE.whoc")
  setnames(dt.output, "seTE.x", "seTE.whoc")
  setnames(dt.output, "n.x", "n.whoc")
  
  dt.output.income <- dt.output.income[ ,c("Income.group","syndrome","class","gram.stain",
                                           "TE.x","seTE.x","group_income","n.x")]
  setnames(dt.output.income, "TE.x", "TE.income")
  setnames(dt.output.income, "seTE.x", "seTE.income")
  setnames(dt.output.income, "n.x", "n.income")
  
  dt.output.wbregion <- dt.output.wbregion[ ,c("wb.region","syndrome","class","gram.stain",
                                               "TE.x","seTE.x","group_wbregion","n.x")]
  setnames(dt.output.wbregion, "TE.x", "TE.wbregion")
  setnames(dt.output.wbregion, "seTE.x", "seTE.wbregion")
  setnames(dt.output.wbregion, "n.x", "n.wbregion")
  
  
  dt.output.global <- dt.output.global[ ,c("syndrome","class","gram.stain",
                                           "TE.x","seTE.x","group_global","n.x")]
  setnames(dt.output.global, "TE.x", "TE.global")
  setnames(dt.output.global, "seTE.x", "seTE.global")
  setnames(dt.output.global, "n.x", "n.global")
  
  ## merge dt.all with outputs
  # first remove the dt.all categories we don't want to map down to
  # i.e. non-WHO-classified countries
  dt.all.merge <- dt.all[!is.na(whoc.region)]
  
  dt.output.country <- merge(dt.all.merge, dt.output, by=c("whoc.region",
                                                           "syndrome",
                                                           "class",
                                                           "gram.stain"))
  ### note that income.group.x and income.group.y are different
  # because the one row chosen for the WHOC regional data (e.g. AGO) might be different
  # to the one being merged onto to (e.g. AFG) but this will get removed anyway
  # so ignore the ".y" columns for now
  
  dt.output.all <- merge(dt.output.country, dt.output.income, by=c("Income.group",
                                                                   "syndrome",
                                                                   "class",
                                                                   "gram.stain"))
  
  dt.output.all2 <- merge(dt.output.all, dt.output.wbregion, by=c("wb.region",
                                                                  "syndrome",
                                                                  "class",
                                                                  "gram.stain"))
  
  dt.output.all3 <- merge(dt.output.all2, dt.output.global, by=c("syndrome",
                                                                 "class",
                                                                 "gram.stain"), 
                          allow.cartesian = TRUE)
  ## if whoc available
  dt.output.all3[ , TE.final := TE.whoc]
  dt.output.all3[TE.final==TE.whoc, level := "whoc"]
  dt.output.all3[TE.final==TE.whoc, no.studies := n.whoc]
  
  ## if income available
  dt.output.all3[is.na(TE.final), TE.final := TE.income ]
  ## have to add in extra is.na() in case all income are in one whoc region
  dt.output.all3[TE.final==TE.income & is.na(TE.whoc), level := "income"]
  dt.output.all3[TE.final==TE.income, no.studies := n.income ]
  
  ## if wb region available
  dt.output.all3[is.na(TE.final), TE.final := TE.wbregion]
  dt.output.all3[TE.final==TE.wbregion & is.na(TE.whoc)
                 & is.na(TE.income), level := "wbregion"]
  dt.output.all3[TE.final==TE.wbregion, no.studies := n.wbregion ]
  
  ## if global available
  dt.output.all3[is.na(TE.final), TE.final := TE.global ]
  dt.output.all3[TE.final==TE.global & is.na(TE.whoc)
                 & is.na(TE.income) & is.na(TE.wbregion) , level := "global"]
  dt.output.all3[TE.final==TE.global, no.studies := n.global ]
  
  ## setting the standard error accordingly
  dt.output.all3[ , seTE.final := seTE.whoc] 
  dt.output.all3[is.na(seTE.final), seTE.final := seTE.income ]
  dt.output.all3[is.na(seTE.final), seTE.final := seTE.wbregion ]
  dt.output.all3[is.na(seTE.final), seTE.final := seTE.global ]
  
  
  ## just ones we have global estimates for
  ## otherwise can use same syndrome + gram.stain - but will already
  # have those estimates in the output file & would mean a lot more code
  los.output.cc <- dt.output.all3[!is.na(TE.final)]
  los.output.cc <- los.output.cc[ ,c("syndrome","class" , "gram.stain",  "iso3c.x",
                                     "whoc.region"   , "Income.group"    ,  "wb.region"    , "TE.final"     ,  
                                     "seTE.final" ,   "level"    ,      "no.studies")]
  
  return(los.output.cc)
}