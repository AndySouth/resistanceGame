#' tests whether controls used promote the resistance present
#'
#' based on controls used and a configuration file
#'
#' @param controls_used vector of numbers named with control_ids
#' @examples
#' controls_used <- 1
#' names(controls_used) <- 'irs_pyr'
#' is_control_incr_resist( controls_used, read_config())
#' @return integer 0=no, 1 =yes
#' @export
is_control_incr_resist <- function(controls_used,
                                   l_config)
{
  
  #todo beware this can be tricky
  #need to be careful whether using NA or 0
  #plus be careful if multiple controls prompting resistance
  
  #which controls being used  
  controls_used <- controls_used[ which(controls_used > 0) ]
  
  #are these controls ones that prompt the resistance
  #this returns TRUE or logical(0) i think
  resistance_up <- names(controls_used) %in% l_config$resistances[['control_id']]
  
  #todo check whether I want to return just 0 or 1
  #?or T/F
  
  #just return 0 or 1
  if (length(resistance_up)==0) resistance_up <- 0
  else if (resistance_up > 1) resistance_up <- 1
  else if (resistance_up) resistance_up <- 1
    
  return(resistance_up)
}
