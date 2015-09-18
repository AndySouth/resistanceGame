#' reads in game model configuration files
#'
#' creates configuration list that can be used to specify game behaviour
#'
#' @param in_folder folder containing the config files
#' @examples
#' list_config <- read_config()
#' @return list containing configuration details
#' @export

read_config <- function(in_folder=NULL) 
{

  #to read in game model configuration files
  #perhaps I could read in to a list of dataframes ?
  #so that it is in one object I can move around
  #I want to be able to pass components of it to run_sim or similar
  
  #to get example configuration files from the package
  #first need to set the folder that they are in
  
  if (is.null(in_folder))  
     in_folder <- system.file("extdata","config1", package="resistanceGame")
  
  
  #name <- "C:\\Dropbox\\Serious Gaming Initiative - Resistance Game (1)\\game model\\gm_config_test1\\resistances.csv"
  #name <- "C:\\Dropbox\\Serious Gaming Initiative - Resistance Game (1)\\game model\\gm_config_test1\\vectors.csv"
  #name <- "C:\\Dropbox\\Serious Gaming Initiative - Resistance Game (1)\\game model\\gm_config_test1\\controls.csv"
  
  resistances <- read.csv(file.path(in_folder,"resistances.csv"), as.is=TRUE)
  vectors <- read.csv(file.path(in_folder,"vectors.csv"), as.is=TRUE)
  controls <- read.csv(file.path(in_folder,"controls.csv"), as.is=TRUE)
  control_plan <- read.csv(file.path(in_folder,"control_plan.csv"), as.is=TRUE)
    
  #put into a list
  list_config <- list(vectors=vectors,
                      controls=controls,
                      resistances=resistances,
                      control_plan=control_plan)
  
  #initially I want to try running for one vector
  #and see how I can access the interaction between controls and resistances
  #start with a single resistance and multiple controls (similar to my current UI setup)
  
  #it's going to get tricky if we start to allow multiple resistances
  #so initially just allow one resistance at a time
  
  #control_id will need to be in some time dataframe (which will be similar to my current dF in resistanceGame)
  #but it will need later to be able to store multiple controls
  #so it may need to be a list rather than a dataframe
  
  #controls$control_id %in% resistances$control_id

}


