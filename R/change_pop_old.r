#' change vector population based on old carrying capacity driven simulation
#'
#' simple logistic model to change vector population based on growth rate, K, insecticide and resistance
#'
#' @param pop vector population in this timestep
#' @param rate_growth population growth rate
#' @param carry_cap carrying capacity (K) in the logistic model
#' @param insecticide_kill kill rate due to insecticide
#' @param resist_freq frequency of resistance
#' @param resistance_modifier modifies effect of resistance
#' @param insecticide_on whether insecticide is applied 0=no, 1=yes
#' @param resistance_on whether there is resistance to the applied insecticide 0=no, 1=yes
#' @param randomness 0-1 0=none, 1=maximum
#' @param never_go_below restock at this level if pop goes below it
#' @examples
#' change_pop_oldcc(pop=0.5, rate_growth=0.4, carry_cap=1, insecticide_kill=0.4, resist_freq=0.2, resistance_modifier=1, resistance_on=1, insecticide_on=1)
#' @return float population in next timestep
#' @export

change_pop_oldcc <- function(pop,
                             rate_growth,
                             carry_cap,
                             insecticide_kill,
                             resist_freq,
                             resistance_modifier,
                             insecticide_on,
                             resistance_on,
                             randomness = 0,
                             never_go_below = 0.01
) 
{
  
  #todo fix this temp fudge
  if(is.na(insecticide_on)) insecticide_on <- 0
  if(is.na(resistance_on)) resistance_on <- 0  
  
  #warnings
  if ( length(insecticide_on) > 1 || 
       length(resistance_on) > 1 ||
       insecticide_on > 1 ||
       resistance_on > 1)
  {
    warning("insecticide_on & resistance_on should be single value between 0 & 1 they are : ", insecticide_on,", ", resistance_on, "\n")
  }
  
  pop2 <- pop +
    rate_growth * pop * (1-pop/carry_cap) -
    insecticide_on * insecticide_kill * pop *
    (1-resistance_on * resist_freq ^ (1/resistance_modifier) )     
  
  
  #randomness 0-1
  if (randomness > 0)
    pop2 <- pop2 + (randomness * runif(1, min=-1, max=1))
  
  
  #restock popn if it goes below a defined level
  #to stop unrealistic situation of negative pop
  #and to allow realistic recolonisation
  if ( pop2 < never_go_below )
    pop2 <- never_go_below
  
  
  #   if ( insecticide_on && resistance_on )
  #   {
  #     
  #     pop2 <- pop +
  #       rate_growth * pop * (1-pop/carry_cap) -
  #       insecticide_kill * pop *
  #       (1-resist_freq ^ (1/resistance_modifier) )    
  #     
  #   } else if ( insecticide_on )
  #   {
  #     
  #     pop2 <- pop +
  #       rate_growth * pop * (1-pop/carry_cap) -
  #       insecticide_kill * pop 
  #     
  #   } else
  #   {
  #     
  #     pop2 <- pop +
  #       rate_growth * pop * (1-pop/carry_cap) 
  #     
  #   }
  
  return(pop2)
  
}


