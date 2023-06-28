#### writing final data tables to a format that will be uploaded 

library(openxlsx)

path1 <- "C:/Users/nichola.naylor/Documents/WHO_2023/VAF_AMR_EconBurden/outputs/"

myfiles = list.files(path=path1,
                     pattern="*.csv", full.names = TRUE)


files_to_upload <- grep("END", myfiles, value = TRUE) 

for (i in 1:length(files_to_upload)){
x <- read.csv(files_to_upload[i])

### extracting the filename from the original pathname
Split <- strsplit(files_to_upload[i], "/")
filename <- Split[[1]][length(Split[[1]])]
filename <- substring(filename,1, nchar(filename)-4)

filename <- paste0(path1, filename,".xls")

write.xlsx(x, filename)
}