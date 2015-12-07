#' initialise simulation results list based on config file
#'
#' create list to contain simulation results
#'
#' @param num_tsteps number of timesteps to put in dataframe
#' @param l_config list of config parameters
#' @examples
#' dF <- init_sim(20, l_config=read_config())
#' @return list to contain simulation results
#' @export

init_sim <- function(num_tsteps=20, l_config=NULL) 
{
  
  #read default config if none specified
  if (is.null(l_config))
    l_config <- read_config()
  
  
  controls_blank <- rep(NA,length( l_config$controls$control_id ))
  names(controls_blank) <- l_config$controls$control_id
  
  
  #this has to be a list of lists to allow me to store in another master list after
  l_one_time <- list(list(pop = NA,
                 resist = NA,
                 cost = NA,
                 controls_used = controls_blank,
                 emergence = NA))
                 #l_controls_used = l_controls_blank))
  
  #create a list with blanks for each timestep
  l_time <- vector("list", length=num_tsteps)
  
  #fill it with blank l_one_time lists
  l_time[1:num_tsteps] <- l_one_time
  
  #to access data
  #l_time[[1]] #to get first timestep
  #l_time[[1]]$pop #to get pop in first tstep
  #l_time[[1]]$controls_used['irs_pyr'] #get whether a control used in first tstep  
  
  #but for controls_used I don't need all the blank NAs I may just 
  #be able to store the ids of the controls used in each timestep
  
  #first go at filling controls_used from l_config$control_plan
  #num rows in control_plan
  n_controls <- nrow(l_config$control_plan)
  #seq(len=n_controls) copes if 0 controls
  for(control_num in seq(len=n_controls))
  {
    
    #for each row in config list
    t_strt <- l_config$control_plan[['t_strt']][[control_num]]
    t_stop <- l_config$control_plan[['t_stop']][[control_num]] 
    control_id <- l_config$control_plan[['control_id']][[control_num]]
    
    #this doesn't work
    #l_time[ t_strt:t_stop ]$controls_used[[control_id]] <- 1  
    
    for(t in t_strt:t_stop )
    {
      l_time[[t]]$controls_used[[control_id]] <- 1       
    }

           
  }

  
  return(l_time)
  
}