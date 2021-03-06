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
#  #cut columns after 9 which contain non useful stuff
#  locations <- locations[,c(1:9)]
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
#  colnames(vectors)[colnames(vectors)=="Vector ID"] <- "vec_id"
#  colnames(vectors)[colnames(vectors)=="Name"] <- "name"
#  colnames(vectors)[colnames(vectors)=="Survival Rate"] <- "survival"
#  colnames(vectors)[colnames(vectors)=="Infection Rate"] <- "infection"
#  
#  #save example edited data to the package
#  devtools::use_data(locations, overwrite=TRUE)
#  devtools::use_data(resistances, overwrite=TRUE)
#  devtools::use_data(interventions, overwrite=TRUE)
#  devtools::use_data(vectors, overwrite=TRUE)

## ---- eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE, results='hide'----
#  
#  #create a composite dataframe of vectors at locations for later use
#  vecbyloc <- vecbyloc_from_gdocs()
#  devtools::use_data(vecbyloc, overwrite=TRUE)
#  

## ---- eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE, results='hide', fig.width=7, fig.height=7----
#locations should be got from saved ver. in package
#data(locations)

plot_config_gdocs_emergences()


## ---- eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE, results='hide', fig.width=7, fig.height=3----

  #for each vecloc combination
  for (i in 1:nrow(vecbyloc))
  {
      #expanding the emergence string
      tmp5 <- expand_season(vecbyloc$emer_string[i], return_tstep = 'weeks')

      #run sim & just plot pop
      plot_sim_pop( run_sim(num_tsteps=144,
                    emergence=tmp5, survival=vecbyloc$survival[i]),
         title=paste0(vecbyloc$id[i]," survival:",vecbyloc$survival[i]), axis_x=TRUE, leg_pos='topleft')
  }


## ---- eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE, results='hide', fig.width=7, fig.height=4----

  #for each vecloc combination
  for (i in 1:nrow(vecbyloc))
  {
      #expanding the emergence string
      tmp5 <- expand_season(vecbyloc$emer_string[i], return_tstep = 'weeks')
      
      #run sim
      l_config2 <- config_plan(read_config(), t_strt=c(12*4, 24*4), t_stop=c(18*4,30*4),
                          control_id=c('irs_pyr'))
      plot_sim( run_sim(num_tsteps=144, emergence=tmp5, survival=vecbyloc$survival[i],
                   l_config=l_config2,
                   insecticide_kill=0.9, resist_freq_start = 0.05, resist_mech='metabolic',
                   resist_incr=0.1, resist_decr = 0.02),           
                   plot_emergence=TRUE, title=paste0(vecbyloc$id[i]), leg_pos='topleft' )
      
  }



