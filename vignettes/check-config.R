## ---- eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE----------------
#### load required packages
library(googlesheets)
library(readr)
library(resistanceGame)
library(dplyr)
library(ggplot2)

## ---- eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE, results='hide'----
#  #eval et to false to not query gdocs
#  
#  url_locations <- "https://docs.google.com/spreadsheets/d/1w6vZvxqX3FgtkmM8mUSvB67reekKz148LoQY2OT012M/"
#  url_resistances <- "https://docs.google.com/spreadsheets/d/1f9omxuMru95xBkmR0hLhrM88tvnxvJjsEXIiDjJOgL4"
#  url_interventions <- "https://docs.google.com/spreadsheets/d/1SVyZZTR6tmDHRr0hLAyn3UYS6n0x9vuu1YEKJ4M8MIs"
#  url_vectors <- "https://docs.google.com/spreadsheets/d/10urCBTuW-_E7i1alJOfqXF3r2jglrAQGBsvOGo7k1sA"
#  
#  #reading location config file from googledocs
#  gd <- gs_url(url_locations)
#  locations <- gs_read(gd)
#  
#  gd <- gs_url(url_resistances)
#  resistances <- gs_read(gd)
#  
#  gd <- gs_url(url_interventions)
#  interventions <- gs_read(gd)
#  
#  gd <- gs_url(url_vectors)
#  vectors <- gs_read(gd)
#  
#  
#  #cut first 2 rows which have junk in
#  locations <- as.data.frame( locations[-c(1:2),] )
#  #cut columns after 8 which contain non useful stuff
#  locations <- locations[,c(1:8)]
#  #cut rows with NULL & NA in Vector ID column
#  locations <- locations[ -which(is.na(locations["Vector IDs"]) | locations["Vector IDs"]=='NULL'), ]
#  
#  #rename columns to more useful
#  colnames(locations)[colnames(locations)=="Location ID"] <- "loc_id"
#  colnames(locations)[colnames(locations)=="Vector\nEmergences"] <- "emergences"
#  colnames(locations)[colnames(locations)=="Vector IDs"] <- "vec_ids"
#  colnames(locations)[colnames(locations)=="Vector Resistance\nIDs (*)"] <- "vec_res_ids"
#  colnames(locations)[colnames(locations)=="Vector\nPopulations (*)"] <- "vec_abund"
#  colnames(locations)[colnames(locations)=="Vector Resistance\nFrequencies (*)"] <- "vec_res_freqs"
#  colnames(locations)[colnames(locations)=="Vector Resistance\nIntensities (*)"] <- "vec_res_ints"
#  
#  #save example edited data to the package
#  devtools::use_data(locations)
#  devtools::use_data(resistances)
#  devtools::use_data(interventions)
#  devtools::use_data(vectors)
#  

## ---- eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE, results='hide', fig.width=7, fig.height=7----
#locations should be got from saved ver. in package
#data(locations)

plot_config_gdocs_emergences(locations)


## ---- eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE, results='hide', fig.width=7, fig.height=7----

#TODO work out how to resolve repetition of code in plot_config_gdocs_emergences()

  #for each location
  for (i in 1:nrow(locations))
  {
    tmp <- locations$emergences[i]

    #remove comma from middle and split in two
    tmp2 <- unlist(strsplit(tmp, split = ","))
    #remove brackets
    tmp3 <- gsub('\\(','',tmp2)
    tmp4 <- gsub('\\)','',tmp3)
    
    #this might not work if just one vector
    vec_ids <- unlist(strsplit(locations$`vec_ids`[i], split = ","))
    
    #for each vector at this location
    for (j in 1:length(tmp4))
    {
      #expanding the emergence string
      tmp5 <- expand_season(tmp4[j], return_tstep = 'weeks')
      
      #add plot title
      
      #run sim & plot
      plot_sim(run_sim(num_tsteps=144,
                       emergence=tmp5))
    }  
    
  }



