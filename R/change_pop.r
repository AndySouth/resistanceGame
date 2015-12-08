#' change vector population based on emergence 
#'
#' simple simulation of vector population based upon emergence suggested by Nakul Chitnis
#'
#' @param pop vector population in this timestep
#' @param survival adult survival rate
#' @param emergence emerging adults, can be a vector can be greater than 1
#' @param rate_insecticide_kill kill rate due to insecticide
#' @param resist_freq ferquency of resistance
#' @param resistance_modifier modifies effect of resistance (unlikely to be used in game)
#' @param resist_intensity intensity of resistance 1,2,5 & 10 fold
#' @param insecticide_on whether insecticide is applied 0=no, 1=yes
#' @param resistance_on whether there is resistance to the applied insecticide 0=no, 1=yes
#' @param randomness 0-1 0=none, 1=maximum
#' @param verbose whether to output diagnostics to console
#' @examples
#' change_pop(pop=0.5, survival=0.4, emergence=1, rate_insecticide_kill=0.4, resist_freq=0.2, resistance_modifier=1, resistance_on=1, insecticide_on=1, verbose=TRUE)
#' change_pop(pop=0.5, survival=0.7, emergence=0.3, rate_insecticide_kill=0.4, resistance_on=0, insecticide_on=1, verbose=TRUE)
#' @return float population in next timestep
#' @export

change_pop <- function(pop,
                       survival,
                       emergence,
                       rate_insecticide_kill = 0.5,
                       resist_freq = 0,
                       resistance_modifier = 1,
                       resist_intensity = 10,
                       insecticide_on = 0,
                       resistance_on = 0,
                       randomness = 0,
                       verbose = FALSE
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
  
  
  # new emergence model 
  #pop2 <- emergence + survivors
  
#previous working version without intensity   
#   control_kill <- insecticide_on * rate_insecticide_kill *
#                       (1-(resistance_on * resist_freq ^ (1/resistance_modifier) ))
  
  #11/11/15 adding in resistance_intensity which can be 1,2,5,10
  #for 10 should have same effect
  #8/12/15 corrected twice
  control_kill <- insecticide_on * rate_insecticide_kill *
                  (1-(resistance_on * (resist_freq^(1/resistance_modifier)) * 
                      resist_intensity/10 ))
  
    
  surviving_adults <- pop * survival * (1-control_kill)
  
  #7/12/15 I could multiply emergence by pop to represent 
  #that higher populations of adults are likely to generate greater emergence
  #this should help to show a slower effect of intereventions
  #but didn't seem to work
  #emergence <- emergence * pop
  
  
  pop2 <- emergence + surviving_adults
   
  if (verbose)
    message("insecticide_on:",insecticide_on,
            " rate_insecticide_kill:",rate_insecticide_kill,
            " resistance_on:",resistance_on,
            " resist_freq:",resist_freq,
            " control_kill:",control_kill )       
  
  
  #randomness 0-1
  if (randomness > 0)
    pop2 <- pop2 + (randomness * runif(1, min=-1, max=1))
  
  
  #stop pop going negative by randomness
  if ( pop2 < 0 ) pop2 <- 0

  

  return(pop2)
  
}
