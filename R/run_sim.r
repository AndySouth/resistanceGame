#' run flexible simulation of population and resistance change based on emergence driven by config file
#'
#' some params driven by config file, others by function args
#'
#' @param num_tsteps number of timesteps to run simulation
#' @param pop_start start vector population
#' @param resist_freq_start frequency of resistance at start, 0 to 1
#' @param resist_intensity_start intensity of resistance at start, 1 to 10
#' @param survival adult survival rate
#' @param emergence emerging adults, can be a vector can be greater than 1
#' @param insecticide_kill kill rate due to insecticide
#' @param resistance_modifier modifies effect of resistance
#' @param resist_incr increase in resistance when correct insecticide present
#' @param resist_decr decrease in resistance when correct insecticide absent
#' @param l_config list of config parameters
#' @param randomness 0-1 0=none, 1=maximum
#' 
#' @examples
#' l_time <- run_sim(pop_start=0.5, resist_freq_start=0.2, survival=0.8, emergence=0.2, insecticide_kill=0.4, resistance_modifier=1)
#' #plot default run
#' plot_sim( run_sim())
#' #modify params
#' plot_sim( run_sim( insecticide_kill = 0.3, resist_incr = 0.05 ))
#' #modify config file
#' l_config <- read_config()
#' l_config2 <- config_plan(l_config, t_strt=c(1,11), t_stop=c(10,20), control_id=c('irs_pyr','irs_ddt'))
#' plot_sim( run_sim(l_config=l_config2, resist_incr=0.1))
#' @return list of simulation results
#' @export

run_sim <- function(num_tsteps=20,
                     pop_start=0.5,
                     resist_freq_start=0.1,
                     resist_intensity_start=1,
                     resist_mech='metabolic',
                     survival=0.7, 
                     emergence=0.3, #(equilibrium pop = emergence/(1-survival))
                     insecticide_kill=0.8, #default put up from 0.2 for emerge version
                     resistance_modifier=1,
                     resist_incr = 0.2,
                     resist_decr = 0.1,
                     l_config=NULL, #list got from configuration files
                     randomness = 0
) 
{
  
  #read default config if none specified
  if (is.null(l_config))
    l_config <- read_config()
  
  
  #initialise the list storing time data including what controls used
  l_time <- init_sim(num_tsteps=num_tsteps, l_config=l_config)
  
  l_time[[1]]$pop <- pop_start
  l_time[[1]]$resist <- resist_freq_start
  
  #todo later put this here
  #resist_intense_start
  
  #allowing seasonal emergence to be got from config files 
  #emergence <- expand_season(season_string=l_config$places$emergence[1]) 
  #doesn't work yet ...
  #because we need to decide which entry in config file do we want if there are multiple places
  #instead can use expand_season() to set emergence (e.g. in vignette) & pass emergence to this function
  #emergence <- expand_season(season_string="6:0.1;6:0.9")
 
  
  #sneaky bit of code to replicate emergence as many times as needed to fill all tsteps
  #this allows some flexibility in creating seasonal patterns
  if (length(emergence) < num_tsteps)
  {
    emergence <- rep_len(emergence, num_tsteps)
  }
  
  for( tstep in 1:(num_tsteps) )
  {
    l_time[[tstep]]$emergence <- emergence[tstep]
  }
  
  
  #tstep loop
  for( tstep in 1:(num_tsteps-1) )
  {
    
    #cat("t",tstep,"\n")
    
    #initially insecticide on is just if a control measure is present
    #todo later this will need to get the kill_rate from somewhere
    #or even assess whether this control measure works on this vectorl_time
    
    #insecticide_on <- l_time$use_pyr[tstep] | l_time$use_ddt[tstep] | l_time$use_ops[tstep] | l_time$use_car[tstep]
    
    #this sums all control measures
    #todo be careful with whether this should add to > 1 and what happens
    insecticide_on <- sum(l_time[[tstep]]$controls_used, na.rm=TRUE)
    
    
    #resistance_on <-  l_time$use_pyr[tstep] | l_time$use_ddt[tstep]
    
    #resistance_on is whether there is an appropriate combination
    #of resistance mechanism and control method
    #initially assume just one resistance mechanism at a time
    #it will need to test both l_time and list_config
    
    resistance_on <- is_control_incr_resist( controls_used = l_time[[tstep]]$controls_used,
                                             l_config = l_config )
    
    #cat("insecticide & resistance on ",insecticide_on, resistance_on,"\n")
    
    #8/12/15 set resist_intensity to 10*resist_freq
    #unless target-site in which case restrict it to 1
    if (resist_mech=='metabolic')
        resist_intensity <- 10 * l_time[[tstep]]$resist
    
    else if (resist_mech=='target')
      resist_intensity <- 1
    
    else stop("resist_mech needs to be one of 'metabolic' or 'target', not", resist_mech)
    
    
    # change population
    l_time[[tstep+1]]$pop <- change_pop( pop = l_time[[tstep]]$pop,
                                         resist_freq = l_time[[tstep]]$resist,
                                         #initially have this at constant
                                         resist_intensity = resist_intensity,
                                         survival = survival,
                                         emergence = l_time[[tstep]]$emergence,
                                         insecticide_kill = insecticide_kill,
                                         resistance_modifier = resistance_modifier,
                                         #initially just test whether any insecticide
                                         insecticide_on = insecticide_on,
                                         #initially just test whether pyr or ddt
                                         resistance_on = resistance_on,
                                         randomness = randomness )
    
    # change resistance
    l_time[[tstep+1]]$resist <- change_resistance( resistance = l_time[[tstep]]$resist,
                                                   resist_incr = resist_incr,
                                                   resist_decr = resist_decr,
                                                   #initially just test whether pyr or ddt
                                                   resistance_on = resistance_on )
    
  }
  
  
  return(l_time)
  
  
}