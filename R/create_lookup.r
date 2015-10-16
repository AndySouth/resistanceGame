#' create an example lookup table that could be used by a game to direct it's behaviour
#'
#' this is a prototype to explore what a lookup table might look like
#'
#' @param input_values a list of inputs and values
#' @param rate_growth vector popn growth rate
#' @param rate_insecticide_kill insecticide kill rate
#' @param resistance_modifier resistance modifier
#' @param carry_cap carrying capacity
#' @param resist_incr increase in resistance when correct insecticide present
#' @param resist_decr decrease in resistance when correct insecticide absent
#' @param write_csv a filename or NULL for no csv output
#' @examples
#' create_lookup(write_csv=NULL)
#' @return a lookup table of input values and population and resistance outputs 
#' @export


create_lookup <- function(   input_values = list( use_pyr=c(0,1), 
                                                 use_ddt=c(0,1),
                                                 use_ops=c(0,1),
                                                 use_car=c(0,1),
                                                 pop_vector=seq(0.1,1,0.1),
                                                 resist_pyr=seq(0.1,1,0.1) ),
                             rate_growth = 0.4,
                             rate_insecticide_kill = 0.4,
                             resistance_modifier = 1,
                             carry_cap = 1,
                             resist_incr = 0.4,
                             resist_decr = 0.4,
                             write_csv = 'demoLookupTable.csv'
                          ){
 
  
  inputs <- expand.grid(input_values)

  #adding on outputs columns
  #columnsOutput <- c('change_pop_vector','change_resist_pyr')
  
  #todo: get this to fill the output columns based on the logistic equations
  #maybe put the logistic equations from shinyGame1.r into package functions
  #but only bother doing that if we are going to do much more with this
  #remember that this is not the objective
  #inputs$change_pop_vector <- 0
  insecticide_on <- inputs$use_pyr | inputs$use_ddt | inputs$use_ops | inputs$use_car
  resistance_on <- inputs$use_pyr | inputs$use_ddt
  
  inputs$changed_pop_vector <- change_pop_oldcc( pop = inputs$pop_vector,
                                          rate_growth = rate_growth,
                                          carry_cap = carry_cap,
                                          rate_insecticide_kill = rate_insecticide_kill,
                                          rate_resistance = inputs$resist_pyr,
                                          resistance_modifier = resistance_modifier,
                                          #initially just test whether any insecticide
                                          insecticide_on = insecticide_on,
                                          #initially just test whether pyr or ddt
                                          resistance_on = resistance_on )
  
  
  #inputs$change_resist_pyr <- 0
  inputs$changed_resist_pyr <- change_resistance( resistance = inputs$resist_pyr,
                                                 resist_incr = resist_incr,
                                                 resist_decr = resist_decr,
                                                 #initially just test whether pyr or ddt
                                                 resistance_on = resistance_on )
    
  
  if ( !is.null(write_csv) )
    write.csv(inputs, file=write_csv)
  
  return(inputs)
   
}