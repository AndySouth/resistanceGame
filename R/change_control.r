#' configure control plan
#'
#' allows start & stop of controls to be set, depends on controls available in config file
#'
#' @param l_config list of config parameters
#' @param t_strt vector of start times
#' @param t_stop vector of stop times
#' @param control_id vector of control_ids for the times
#' @param add whether to add the new controls to exisiting ones, default FALSE to replace
#' @examples
#' l_config <- read_config()
#' l_config2 <- change_control(l_config, t_strt=1, t_stop=15, control_id='irs_pyr')
#' l_config2 <- change_control(l_config, t_strt=c(1,15), t_stop=c(10,20), control_id='irs_pyr')
#' l_config2 <- change_control(l_config, t_strt=c(1,11), t_stop=c(10,20), control_id=c('irs_pyr','irs_ops'))
#' l_config2 <- change_control(l_config, t_strt=c(1,11), t_stop=c(10,20), control_id=c('irs_pyr','irs_ddt'))
#' plot_sim2( run_sim2(l_config=l_config2))
#' @return list with modified control_plan
#' @export
change_control <- function( l_config,
                                 t_strt,
                                 t_stop,
                                 control_id,
                                 add = FALSE) 
{
  
  #todo later add an option for add=TRUE
  if (add)
  {
    l_config$control_plan[['t_strt']] <- c(l_config$control_plan[['t_strt']], t_strt)
    l_config$control_plan[['t_stop']] <- c(l_config$control_plan[['t_stop']], t_stop)
    l_config$control_plan[['control_id']] <- c(l_config$control_plan[['control_id']], control_id)
  }
  
  #todo add checks that times are within those specified
  #todo add check that control_ids are present
 
# this didn't work when no controls present before   
#   l_config$control_plan[['t_strt']] <- t_strt
#   l_config$control_plan[['t_stop']] <- t_stop
#   l_config$control_plan[['control_id']] <- control_id
  
  dF <- data.frame(t_strt=t_strt, t_stop=t_stop, control_id=control_id, stringsAsFactors = FALSE)
  
  l_config$control_plan <- dF
  
  return(l_config)
  
}