
#### code for plotting final outputs by Rhys Kingston 2023
### adapted for this repo


if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "tidyverse"
  , "readr"
  , "here"
  , "janitor"
  , "scales"
  , "rgdal"
  , "sf"
  , "countrycode"
  , "maps"
  , "rgeos"
  , "devtools"
  , "treemapify"
  , "ggbeeswarm"
  , "viridis"
  , "plotly"
)

########## loading data
load("outputs/regional_hospital_4additionalplots.RData")

### accounting for duplication of E. coli total amounts for ETEC & EXPEC
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         Median_total_costing :=0]
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         LOWIQR_total_costing :=0]
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         HIGHIQR_total_costing :=0]
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         Median_total_days :=0]
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         LOWIQR_total_days :=0]
regional_hospital_totals[vaccine_id=="ETEC_0.6_0.7_5 years_Diarrhoea_6 months",
                         HIGHIQR_total_days :=0]

load("outputs/prod_4additionalplots.RData")

regional_prod_deaths <- prod_deaths[, lapply(.SD, sum, na.rm=TRUE),
                            by = c(".id","Pathogen","vaccine_id"),
                            .SDcols=c("HC_cost","averted_HC")]
regional_prod_deaths[.id=="PAHO", .id:="AMRO"]
### merge together
all.dt <- merge(regional_prod_deaths, regional_hospital_totals, by.x=c(".id","Pathogen","vaccine_id"),
                by.y=c("WHO.Region","Pathogen","vaccine_id"))
### !! note that the total costing set to 0 for ETEC is just because all e.coli is already costed (included diarrheoa)
## in the EXPEC - so applicable to both but removed to remove double counting

## sum across vaccines for pathogens
all.dt <- all.dt[, lapply(.SD, sum, na.rm=TRUE),
            by = c("Pathogen",
                   ".id"),
            .SDcols=c(    "HC_cost"   ,           
                        "averted_HC"   ,         
                        "Median_avert_costing"  ,"LOWIQR_avert_costing",
                        "HIGIQR_avert_costing" ,
                        "Median_total_costing",  "LOWIQR_total_costing" ,
                        "HIGHIQR_total_costing", "Median_avert_days"    ,
                         "LOWIQR_avert_days"  ,   "HIGIQR_avert_days" ,
                        "Median_total_days"  ,   "LOWIQR_total_days"   , 
                         "HIGHIQR_total_days"  )]

all.dt.temp <- all.dt[,.(median_cost_sum_G=sum(Median_total_costing)),
                 by=c("Pathogen")]

all.dt <- merge(all.dt, all.dt.temp, by=c("Pathogen"),
                allow.cartesian = TRUE)

all.dt.temp <- all.dt[,.(median_prod_cost_sum_G=sum(HC_cost)),
                      by=c("Pathogen")]

all.dt <- merge(all.dt, all.dt.temp, by=c("Pathogen"),
                allow.cartesian = TRUE)

all.dt[ , global_total := median_prod_cost_sum_G+median_cost_sum_G]

##### Line plot for hospital costs
options(scipen=10000) ## turn off scientific notation

######## bed days averted ##################
all.dt %>%
  ggplot() +
  geom_pointrange(
    mapping = aes(
      y = fct_reorder(Pathogen, median_cost_sum_G)
      , x = Median_avert_days
      , xmin = LOWIQR_avert_days
      , xmax = HIGIQR_avert_days
      , colour = .id
    )
    , position = position_dodge(width = 0.5)
    , size = 0.7
    , lwd = 0.7
  ) +
  scale_colour_viridis(discrete = TRUE) +
  scale_x_log10(
    labels = comma
    , position = "top"
  ) +
  labs(
    title = "Median Hospital Days averted due to vaccinations, by WHO region"
    , subtitle = "Capacity in Days. Line ranges represent the IQR.\n"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank()
    , panel.grid.major.y = element_blank()
    , axis.line.y = element_line(
      colour = "grey"
      , linewidth = 1
    )
    , axis.text.y = element_text(
      face = "bold.italic"
      , family = "sans"
      , size = 14
      , colour = "black"
    )
    , axis.text.x.top = element_text(
      family = "sans"
      , size = 12
      , margin = margin(
        b = 10
      )
    )
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    , legend.position = c(0.14, 0.98)
    , legend.title = element_blank()
    , plot.title = element_text(
      face = "bold"
      , family = "sans"
      , size = 20
      , colour = "black"
    )
    , plot.subtitle = element_text(
      face = "bold"
      , family = "sans"
      , size = 16
      , colour = "grey50"
    )
  ) +
  guides(
    alpha = "none"
    , colour = guide_legend(
      override.aes = list(
        size = 2
      )
      , nrow = 1)
  )

########### hospital cost averted ####################
all.dt %>%
  ggplot() +
  geom_pointrange(
    mapping = aes(
      y = fct_reorder(Pathogen, median_cost_sum_G)
      , x = Median_avert_costing
      , xmin = LOWIQR_avert_costing
      , xmax = HIGIQR_avert_costing
      , colour = .id
    )
    , position = position_dodge(width = 0.5)
    , size = 0.7
    , lwd = 0.7
  ) +
  scale_colour_viridis(discrete = TRUE) +
  scale_x_log10(
    labels = dollar
    , position = "top"
  ) +
  labs(
    title = "Median Hospital Costs averted due to vaccinations, by WHO region"
    , subtitle = "Cost in 2019 USD. Line ranges represent the IQR.\n"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank()
    , panel.grid.major.y = element_blank()
    , axis.line.y = element_line(
      colour = "grey"
      , linewidth = 1
    )
    , axis.text.y = element_text(
      face = "bold.italic"
      , family = "sans"
      , size = 14
      , colour = "black"
    )
    , axis.text.x.top = element_text(
      family = "sans"
      , size = 12
      , margin = margin(
        b = 10
      )
    )
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    , legend.position = c(0.20, 0.98)
    , legend.title = element_blank()
    , plot.title = element_text(
      face = "bold"
      , family = "sans"
      , size = 18
      , colour = "black"
    )
    , plot.subtitle = element_text(
      face = "bold"
      , family = "sans"
      , size = 16
      , colour = "grey50"
    )
  ) +
  guides(
    alpha = "none"
    , colour = guide_legend(
      override.aes = list(
        size = 2
      )
      , nrow = 1)
  )

########### total costs ####################

total.cost.plot <- function (all.dt){
  all.dt %>%
  ggplot(
    mapping = aes(
      x = Median_total_costing
      , size = HC_cost
      , y = fct_reorder(Pathogen,global_total)
      , colour = .id
      , alpha = .8
    )
  ) +
  scale_colour_viridis(
    discrete = TRUE
  ) +
  scale_y_discrete(
    expand = c(
      0
      , 1
    )
  ) +
  geom_beeswarm(
    priority = "ascending"
    , cex = 2
  ) +
  scale_size_continuous(
    range = c(
      2
      , 30
    )
  ) +
  scale_x_log10(
    labels = dollar
    , position = "top"
  ) +
  labs(
    title = "Total Hospital Costs caused by DRIs, by WHO region"
    , subtitle = "Log scaled costs in 2019 USD. Circles are sized by productivity loss due to DRI deaths, and colour indicates WHO region.\n"
  ) +
  theme_minimal() +
  theme(
     axis.line.y = element_line(
      colour = "grey"
      , linewidth = 1
    )
    , axis.text.y = element_text(
      face = "bold.italic"
      , family = "sans"
      , size = 14
      , colour = "black"
    )
    , axis.text.x.top = element_text(
      family = "sans"
      , size = 12
      , margin = margin(
        b = 10
      )
    )
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    , legend.position = c(0.165, 0.98)
    , legend.title = element_blank()
    , plot.title = element_text(
      face = "bold"
      , family = "sans"
      , size = 20
      , colour = "black"
    )
    , plot.subtitle = element_text(
      face = "bold"
      , family = "sans"
      , size = 16
      , colour = "grey50"
    )
  ) +
  guides(
    alpha = "none"
    , size = "none"
    , colour = guide_legend(
      override.aes = list(
        size = 8
      )
      , nrow = 1)
  )
}


total.cost.plot(all.dt)


#### unit cost plotting code #####

#### running using global unit cost from combining_cost_cases_NEW
load("outputs/hospitalC_global_los.RData")
UNITcost_averted_global <- hospital_region_averted
UNITcost_averted_global[ ,Exposure_Group := paste(class,Pathogen)]

UNITcost_averted_global <- UNITcost_averted_global %>% complete(Exposure_Group, 
                                                                nesting(Infectious.syndrome))


#### remove TB and discuss in paper, as huge high IQR & having trouble with the log scaling

ggplot(UNITcost_averted_global, aes(x=interaction(Exposure_Group), y=Median_unitcost, 
                                    fill=Infectious.syndrome)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=LOWIQR_unitcost, ymax=HIGIQR_unitcost),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Resistance & Bacterial Exposure") +
  ylab("Hospital Cost per Case (2019 USD)") +
  ggtitle("Global Averages") +
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_cartesian(ylim=c(NA, 30000), expand = FALSE)+
options(scipen=999) 



ggplot(UNITcost_averted_global, aes(x=interaction(Exposure_Group), y=Median_unitcost, 
                                    fill=Infectious.syndrome)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=LOWIQR_unitcost, ymax=HIGIQR_unitcost),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Resistance & Bacterial Exposure") +
  ylab("Hospital Cost per Case (2019 USD)") +
  ggtitle("Global Averages") +
  scale_fill_viridis_d()+
  scale_y_continuous(trans="log10")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))