#' run simulation of population and resistance change
#'
#' initially accepts single arg values next get it to accept vectors by time
#'
#' @param num_tsteps number of timesteps to run simulation
#' @param pop_start start vector population
#' @param rate_resistance_start effect of resistance on insecticide kill rate
#' @param rate_growth population growth rate
#' @param carry_cap carrying capacity (K) in the logistic model
#' @param rate_insecticide_kill kill rate due to insecticide
#' @param resistance_modifier modifies effect of resistance
# @param insecticide_on whether insecticide is applied 0=no, 1=yes
# @param resistance_on whether there is resistance to the applied insecticide 0=no, 1=yes
#' @param resist_incr increase in resistance when correct insecticide present
#' @param resist_decr decrease in resistance when correct insecticide absent
#' @param use_pyr use pyrethroids NA for no, 1 for yes, or a vector e.g. c(NA,1) to give alternate use
#' @param use_ddt use ddt see use_pyr
#' @param use_ops use organophosphates see use_pyr
#' @param use_car use carbamates see use_pyr
#' @param randomness 0-1 0=none, 1=maximum
#' 
#' @examples
#' dF <- run_sim(pop_start=0.5, rate_resistance_start=0.2, rate_growth=0.4, carry_cap=1, rate_insecticide_kill=0.4, resistance_modifier=1)
#' #plot default run
#' plot_sim( run_sim())
#' #modify params
#' plot_sim( run_sim( rate_insecticide_kill = 0.3, resist_incr = 0.05 ))
#' #alternate use of pyr
#' plot_sim( run_sim(use_pyr=c(NA,1)))
#' @return dataframe of simulation results
#' @export

run_sim <- function(num_tsteps=20,
                    pop_start=0.5,
                    rate_resistance_start=0.1,
                    rate_growth=0.2,
                    carry_cap=1,
                    rate_insecticide_kill=0.2,
                    resistance_modifier=1,
                    #insecticide_on=1,
                    #resistance_on=1,
                    resist_incr = 0.2,
                    resist_decr = 0.1,
                    use_pyr = rep(1,num_tsteps),
                    use_ddt = NA,
                    use_ops = NA,
                    use_car = NA,
                    randomness = 0
) 
{

  dF <- init_sim(num_tsteps)

  dF$pop[1] <- pop_start
  dF$resist_pyr[1] <- rate_resistance_start

  dF$use_pyr <- use_pyr
  dF$use_ddt <- use_ddt
  dF$use_ops <- use_ops
  dF$use_car <- use_car
  
  #tstep loop
  for( tstep in 1:(num_tsteps-1) )
  {
    
    #cat("t",tstep,"\n")
    
    insecticide_on <- dF$use_pyr[tstep] | dF$use_ddt[tstep] | dF$use_ops[tstep] | dF$use_car[tstep]
    resistance_on <-  dF$use_pyr[tstep] | dF$use_ddt[tstep]
    
    #cat("insecticide & resistance on ",insecticide_on, resistance_on,"\n")
    

    # change population
    dF$pop[tstep+1] <- change_pop( pop = dF$pop[tstep],
                                    rate_resistance = dF$resist_pyr[tstep],
                                    rate_growth = rate_growth,
                                    carry_cap = carry_cap,
                                    rate_insecticide_kill = rate_insecticide_kill,
                                    resistance_modifier = resistance_modifier,
                                    #initially just test whether any insecticide
                                    insecticide_on = insecticide_on,
                                    #initially just test whether pyr or ddt
                                    resistance_on = resistance_on,
                                    randomness = randomness )
    
    # change resistance
    dF$resist_pyr[tstep+1] <- change_resistance( resistance = dF$resist_pyr[tstep],
                                                  resist_incr = resist_incr,
                                                  resist_decr = resist_decr,
                                                  #initially just test whether pyr or ddt
                                                  resistance_on = resistance_on )
        
  }
  

  return(dF)
 
  
}


#' run flexible simulation of population and resistance change driven by config file
#'
#' some params driven by config file, others by function args
#'
#' @param num_tsteps number of timesteps to run simulation
#' @param pop_start start vector population
#' @param rate_resistance_start effect of resistance on insecticide kill rate
#' @param rate_growth population growth rate
#' @param carry_cap carrying capacity (K) in the logistic model
#' @param rate_insecticide_kill kill rate due to insecticide
#' @param resistance_modifier modifies effect of resistance
#' @param resist_incr increase in resistance when correct insecticide present
#' @param resist_decr decrease in resistance when correct insecticide absent
#' @param l_config list of config parameters
#' @param randomness 0-1 0=none, 1=maximum
#' 
#' @examples
#' dF <- run_sim2(pop_start=0.5, rate_resistance_start=0.2, rate_growth=0.4, carry_cap=1, rate_insecticide_kill=0.4, resistance_modifier=1)
#' #plot default run
#' plot_sim2( run_sim2())
#' #modify params
#' plot_sim2( run_sim2( rate_insecticide_kill = 0.3, resist_incr = 0.05 ))
#' #alternate use of pyr
#' #plot_sim( run_sim(use_pyr=c(NA,1)))
#' @return list of simulation results
#' @export

run_sim2 <- function(num_tsteps=20,
                    pop_start=0.5,
                    rate_resistance_start=0.1,
                    rate_growth=0.2,
                    carry_cap=1,
                    rate_insecticide_kill=0.2,
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
  
  
  l_time <- init_sim2(num_tsteps=num_tsteps, l_config=l_config)
  
  l_time[[1]]$pop <- pop_start
  
  l_time[[1]]$resist <- rate_resistance_start
  
  #l_time$l_controls_used <- l_controls_used

  
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
    
    
    # change population
    l_time[[tstep+1]]$pop <- change_pop( pop = l_time[[tstep]]$pop,
                                   rate_resistance = l_time[[tstep]]$resist,
                                   rate_growth = rate_growth,
                                   carry_cap = carry_cap,
                                   rate_insecticide_kill = rate_insecticide_kill,
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
