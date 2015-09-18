#' configure control plan
#'
#' allows start & stop of controls to be set, depends on controls available in config file
#'
#' @param l_config list of config parameters
#' @param time_strt vector of start times
#' @param time_stop vector of stop times
#' @param control_id vector of control_ids for the times
#' @param add whether to add the new controls to exisiting ones, default FALSE to replace
#' @examples
#' l_config <- read_config()
#' l_config2 <- config_control_plan(l_config, time_strt=1, time_stop=15, control_id='irs_pyr')
#' l_config2 <- config_control_plan(l_config, time_strt=c(1,15), time_stop=c(10,20), control_id='irs_pyr')
#' l_config2 <- config_control_plan(l_config, time_strt=c(1,11), time_stop=c(10,20), control_id=c('irs_pyr','irs_ops'))
#' l_config2 <- config_control_plan(l_config, time_strt=c(1,11), time_stop=c(10,20), control_id=c('irs_pyr','irs_ddt'))
#' plot_sim2( run_sim2(l_config=l_config2))
#' @return list with modified control_plan
#' @export
config_control_plan <- function( l_config,
                                 time_strt,
                                 time_stop,
                                 control_id,
                                 add = FALSE) 
{
  
  #todo later add an option for add=TRUE
  if (add)
  {
    l_config$control_plan[['time_strt']] <- c(l_config$control_plan[['time_strt']], time_strt)
    l_config$control_plan[['time_stop']] <- c(l_config$control_plan[['time_stop']], time_stop)
    l_config$control_plan[['control_id']] <- c(l_config$control_plan[['control_id']], control_id)
  }
  
  #todo add checks that times are within those specified
  #todo add check that control_ids are present
  
  l_config$control_plan[['time_strt']] <- time_strt
  l_config$control_plan[['time_stop']] <- time_stop
  l_config$control_plan[['control_id']] <- control_id
  
  return(l_config)
  
}