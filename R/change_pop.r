#' change vector population
#'
#' simple logistic model to change vector population based on growth rate, K, insecticide and resistance
#'
#' @param pop vector population in this timestep
#' @param rateGrowth population growth rate
#' @param carryCap carrying capacity (K) in the logistic model
#' @param rateInsecticideKill kill rate due to insecticide
#' @param rateResistance effect on resistance on insecticide kill rate
#' @param resistanceModifier modifies effect of resistance
#' @examples
#' change_pop(pop=0.5, rateGrowth=0.4, carryCap=1, rateInsecticideKill=0.4, rateResistance=0.2, resistanceModifier=1)
#' @return float population in next timestep
#' @export

change_pop <- function(pop,
                       rateGrowth,
                       carryCap,
                       rateInsecticideKill,
                       rateResistance,
                       resistanceModifier
) 
{
  
  #todo do I want to put numTimesteps as an arg to allow it to run & return a vector ? or have as other func.
  #todo do I want T/F args for insecticideOn & resistanceOn
  
  pop2 <- pop +
          rateGrowth * pop * (1-pop/carryCap) -
          rateInsecticideKill * pop *
          (1-rateResistance ^ (1/resistanceModifier) )
  
  return(pop2)
  
#   dF$vectorPop[runNum+1] <<- dF$vectorPop[runNum] + 
#     rateGrowth * dF$vectorPop[runNum] * (1-dF$vectorPop[runNum]/K) -   #density dependence
#     rateInsecticideKill * dF$vectorPop[runNum] *                       #insecticide
#     (1-dF$pyrResist[runNum]^(1/resistanceModifier))                    #resistance  
  
}