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
#' @param randomness 0-1 0=none, 1=maximum
#' @param never_go_below restock at this level if pop goes below it
#' @examples
#' change_pop(pop=0.5, rate_growth=0.4, carry_cap=1, rate_insecticide_kill=0.4, rate_resistance=0.2, resistance_modifier=1, resistance_on=1, insecticide_on=1)
#' @return float population in next timestep
#' @export

change_pop <- function(pop,
                       rate_growth,
                       carry_cap,
                       rate_insecticide_kill,
                       rate_resistance,
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
  
  #todo incorporate dealing with multiple kill rates
  #this might be how I can add multiple kill rates
  #i'm sure there must be a simpler way of doing, ask Ian
#   mortn = function(x) { y<-x[1] 
#                         for(i in 2:length(x))
#                           { y = y + (1-y) * x[i] } 
#                         y
#                       }
  #cool yes it seems to work
  #mortn(c(0.8,0.5,0.5))
  #0.95

  
 
  pop2 <- pop +
    rate_growth * pop * (1-pop/carry_cap) -
    insecticide_on * rate_insecticide_kill * pop *
    (1-resistance_on * rate_resistance ^ (1/resistance_modifier) )     
  
  
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
#       rate_insecticide_kill * pop *
#       (1-rate_resistance ^ (1/resistance_modifier) )    
#     
#   } else if ( insecticide_on )
#   {
#     
#     pop2 <- pop +
#       rate_growth * pop * (1-pop/carry_cap) -
#       rate_insecticide_kill * pop 
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


#' change vector population based on emergence 
#'
#' simple simulation of vector population based upon emergence suggested by Nakul Chitnis
#'
#' @param pop vector population in this timestep
#' @param rate_growth population growth rate
#' @param carry_cap carrying capacity (K) in the logistic model
#' @param rate_insecticide_kill kill rate due to insecticide
#' @param rate_resistance effect on resistance on insecticide kill rate
#' @param resistance_modifier modifies effect of resistance
#' @param insecticide_on whether insecticide is applied 0=no, 1=yes
#' @param resistance_on whether there is resistance to the applied insecticide 0=no, 1=yes
#' @param randomness 0-1 0=none, 1=maximum
#' @param never_go_below restock at this level if pop goes below it
#' @examples
#' change_pop(pop=0.5, rate_growth=0.4, carry_cap=1, rate_insecticide_kill=0.4, rate_resistance=0.2, resistance_modifier=1, resistance_on=1, insecticide_on=1)
#' @return float population in next timestep
#' @export

change_pop_emerge <- function(pop,
                       rate_growth,
                       carry_cap,
                       rate_insecticide_kill,
                       rate_resistance,
                       resistance_modifier,
                       insecticide_on,
                       resistance_on,
                       randomness = 0,
                       never_go_below = 0.01
) 
{
  
  #warnings
  if ( length(insecticide_on) > 1 || 
       length(resistance_on) > 1 ||
       insecticide_on > 1 ||
       resistance_on > 1)
  {
    warning("insecticide_on & resistance_on should be single value between 0 & 1 they are : ", insecticide_on,", ", resistance_on, "\n")
  }
  
  #todo incorporate dealing with multiple kill rates
  #this might be how I can add multiple kill rates
  #i'm sure there must be a simpler way of doing, ask Ian
  #   mortn = function(x) { y<-x[1] 
  #                         for(i in 2:length(x))
  #                           { y = y + (1-y) * x[i] } 
  #                         y
  #                       }
  #cool yes it seems to work
  #mortn(c(0.8,0.5,0.5))
  #0.95
  
  
  #todo fix this temp fudge
  if(is.na(insecticide_on)) insecticide_on <- 0
  if(is.na(resistance_on)) resistance_on <- 0  
  
# previous logistic model  
#   pop2 <- pop +
#     rate_growth * pop * (1-pop/carry_cap) -
#     insecticide_on * rate_insecticide_kill * pop *
#     (1-resistance_on * rate_resistance ^ (1/resistance_modifier) )     
 
  
  #beware first go
  #just copy emergence from cc
  emergence <- carry_cap
  #and survival from rate_growth
  survival <- rate_growth
  
  # new emergence model 
  #pop2 <- emergence + survivors
  
  control_kill <- (1-insecticide_on * rate_insecticide_kill *
                  (1-resistance_on * rate_resistance ^ (1/resistance_modifier) ))    
  
  
  surviving_adults <- pop * survival * control_kill
  
  
  pop2 <- emergence + surviving_adults
          
  
  
  #randomness 0-1
  if (randomness > 0)
    pop2 <- pop2 + (randomness * runif(1, min=-1, max=1))
  
  
  #restock popn if it goes below a defined level
  #this shouldn't be needed with new emergence based sim
#   if ( pop2 < never_go_below )
#     pop2 <- never_go_below
  
  
  
  return(pop2)
  
}
