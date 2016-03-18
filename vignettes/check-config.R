## ---- eval=TRUE, echo=FALSE, message=FALSE-------------------------------
#### load required packages
library(googlesheets)
library(readr)
library(resistanceGame)
library(dplyr)
library(ggplot2)

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

#reading location config file from googledocs
gd <- gs_url("https://docs.google.com/spreadsheets/d/1w6vZvxqX3FgtkmM8mUSvB67reekKz148LoQY2OT012M/")
locations <- gs_read(gd)

#cut first 2 rows which have junk in
locations <- as.data.frame( locations[-c(1:2),] )
#cut columns after 8 which contain non useful stuff
locations <- locations[,c(1:8)] 
#cut rows with NULL & NA in Vector ID column
locations <- locations[ -which(is.na(locations["Vector IDs"]) | locations["Vector IDs"]=='NULL'), ] 

#rename emergences column
colnames(locations)[colnames(locations)=="Vector\nEmergences"] <- "emergences"


plot_config_gdocs(locations)

#emergences are here (but I may want to keep in the locations df)
#emergences <- as.data.frame(locations[,"Vector\nEmergences"])
#but there are up to 2 emergence patterns per cell
#so will need to add to/modify expand_season() function

#emergences <- emergences[-which(is.na(emergences) | emergences=='0')]

# emers <- data.frame()
# 
# #not sure this bit below quite copes with emergences being a vector
# for (i in nrow(locations))
# {
#   tmp <- locations$emergences[i]
#   
#   #remove comma from middle and split in two
#   tmp2 <- unlist(strsplit(tmp, split = ","))
#   #remove brackets
#   tmp3 <- gsub('\\(','',tmp2)
#   tmp4 <- gsub('\\)','',tmp3)
#   
#   #for each vector at this location
#   for (j in length(tmp4))
#   {
#     #expanding the string
#     tmp5 <- expand_season(tmp4[j], return_tstep = 'months')
#     
#     #cat(tmp2,"\n")
#     emers <- rbind(emers,tmp5)
#   }    
#   
# }






