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
  #todo
  #pass use_pyr etc. rather than insecticide_on
  #calc whether resistance_on
  #clac rate_resistance change over time

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