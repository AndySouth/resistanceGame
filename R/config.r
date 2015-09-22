#' configure controls available
#'
#' modifies the input from config files
#'
#' @param l_config list of config parameters
#' @param control_id vector of control_ids
#' @param control_name vector of control_names (optional)
#' @param control_desc vector of control descriptions (optional)
#' @param vector_id vector of vector_ids
#' @param control_kill_rate vector of control_kill_rates
#' @param add whether to add, default FALSE to replace
#' @examples
#' l_config <- read_config()
#' l_config2 <- config_controls(l_config, control_id=c('irs_pyr','irs_ops'), vector_id='an_gamb', control_kill_rate=c(0.3,0.4))
#' plot_sim2( run_sim2(l_config=l_config2))
#' @return list with modified controls
#' @export
config_controls <- function( l_config,
                             control_id,
                             control_name=NA,
                             control_desc=NA,
                             vector_id,
                             control_kill_rate,
                             add = FALSE) 
{
  

  #check that control_ids are present  
#   bad_indices <- which(!control_id %in% l_config$controls[['control_id']])
#   if (length(bad_indices)>0)
#   {
#     warning("these control_ids are not in your controls config file and will cause problems :",control_id[bad_indices])
#   }
  
  #put passed data into a dataframe ready to put into list
  dF <- data.frame(control_id=control_id, 
                   control_name=control_name,
                   control_desc=control_desc,
                   vector_id=vector_id, 
                   control_kill_rate=control_kill_rate, stringsAsFactors = FALSE)
  
  
  if (add)
  {
    l_config$controls <- rbind(l_config$controls, dF) 
  } else
  {
    l_config$controls <- dF    
  }
  
  
  return(l_config)
  
}