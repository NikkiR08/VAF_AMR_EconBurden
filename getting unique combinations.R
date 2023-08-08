library(dplyr)
library(data.table)

load("outputs/hospitalC_region_averted.RData")
 xx <- hospital_c[,c("Pathogen","Infectious.syndrome")]
 xy <- hospital_c[ , c("Pathogen","class")]
 xx <- xx %>% distinct()
 xy <- xy %>% distinct()
 xxxy <- merge(xx,xy,by="Pathogen", allow.cartesian = TRUE)
xxxy <- dcast(xxxy, Pathogen + Infectious.syndrome ~ class, value.var="Infectious.syndrome") 
xxxy <- as.data.table(xxxy)
xxxy <- xxxy[ , -c("Infectious.syndrome")]

newdf <- xxxy %>% group_by(Pathogen) %>% summarise_all(toString)

### too much blank space
newdf <- xx %>% group_by(Pathogen) %>% summarise_all(toString)
newdf2 <- xy %>% group_by(Pathogen) %>% summarise_all(toString)
df <- merge(newdf,newdf2,by="Pathogen", allow.cartesian = TRUE)

write.csv(df, file="outputs/unique_combinations.csv")
