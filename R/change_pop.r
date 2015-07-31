#' change vector population
#'
#' simple logistic model to change vector population based on growth rate, K, insecticide and resistance
#'
#' @param pop vector population in this timestep
#' @param rate_growth population growth rate
#' @param carry_cap carrying capacity (K) in the logistic model
#' @param rate_insecticide_kill kill rate due to insecticide
#' @param rate_resistance effect on resistance on insecticide kill rate
#' @param resistance_modifier modifies effect of resistance
#' @param insecticide_on whether insecticide is applied 0=no, 1=yes
#' @param resistance_on whether there is resistance to the applied insecticide 0=no, 1=yes
#' @examples
#' change_pop(pop=0.5, rate_growth=0.4, carry_cap=1, rate_insecticide_kill=0.4, rate_resistance=0.2, resistance_modifier=1)
#' @return float population in next timestep
#' @export

change_pop <- function(pop,
                       rate_growth,
                       carry_cap,
                       rate_insecticide_kill,
                       rate_resistance,
                       resistance_modifier,
                       insecticide_on,
                       resistance_on
) 
{
  
  #todo do I want to put numTimesteps as an arg to allow it to run & return a vector ? or have as other func.

  pop2 <- NULL
  
  if ( insecticide_on && resistance_on )
  {
    
    pop2 <- pop +
      rate_growth * pop * (1-pop/carry_cap) -
      rate_insecticide_kill * pop *
      (1-rate_resistance ^ (1/resistance_modifier) )    
    
  } else if ( insecticide_on )
  {
    
    pop2 <- pop +
      rate_growth * pop * (1-pop/carry_cap) -
      rate_insecticide_kill * pop 
    
  } else
  {
    
    pop2 <- pop +
      rate_growth * pop * (1-pop/carry_cap) 
    
  }
  
  
  return(pop2)
  
#   dF$vectorPop[runNum+1] <<- dF$vectorPop[runNum] + 
#     rate_growth * dF$vectorPop[runNum] * (1-dF$vectorPop[runNum]/K) -   #density dependence
#     rate_insecticide_kill * dF$vectorPop[runNum] *                       #insecticide
#     (1-dF$pyrResist[runNum]^(1/resistance_modifier))                    #resistance  
  
}