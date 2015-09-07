#' change resistance rate
#'
#' change resistance to one insecticide
#'
#' @param resistance resistance rate in this timestep
#' @param resist_incr increase in resistance in presence of insecticide
#' @param resist_decr decrease in resistance in absence of insecticide
# @param insecticide_on whether insecticide is applied 0=no, 1=yes
#' @param resistance_on whether the insecticide applied prompts resistance 0=no, 1=yes
#' @examples
#' change_resistance(resistance=0.2, resist_incr=0.4, resist_decr=0.2, resistance_on=1)
#' @return float resistance in next timestep
#' @export

change_resistance <- function(resistance,
                              resist_incr,
                              resist_decr,
                              resistance_on
) 
{
  
  #todo fix this temp fudge
  if(is.na(resistance_on)) resistance_on <- 0  
  
  resistance2 <- NULL
  
  
  resistance2 <- ifelse( resistance_on, resistance + resist_incr * (1-resistance),
                                        resistance * (1-resist_decr) )
    
  
#   if ( resistance_on )
#   {
#     
#     resistance2 <- resistance + resist_incr * (1-resistance)    
#     
#   } else
#   {
#     
#     resistance2 <- resistance * (1-resist_decr) 
#     
#   }
  
  
  return(resistance2)
  
#   if ( input$pyrOn || input$ddtOn ) 
#     #constraining resistance to 1
#     dF$pyrResist[runNum+1] <<- dF$pyrResist[runNum] +
#       rateResistance * (1 - dF$pyrResist[runNum])
#   else 
#     dF$pyrResist[runNum+1] <<- dF$pyrResist[runNum] * (1-rateResistance)  
  
}