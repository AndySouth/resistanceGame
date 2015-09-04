#' initialise simulation results dataframe
#'
#' create dataframe to contain simulation results
#'
#' @param num_tsteps number of timesteps to put in dataframe
#' @examples
#' dF <- init_sim(20)
#' @return dataframe to contain simulation results
#' @export

init_sim <- function(num_tsteps=20) 
{
  
  dF <- data.frame( use_pyr = rep(NA,num_tsteps),
                     use_ddt = rep(NA,num_tsteps),
                     use_ops = rep(NA,num_tsteps),
                     use_car = rep(NA,num_tsteps),
                     pop = rep(NA,num_tsteps),
                     resist_pyr = rep(NA,num_tsteps),
                     cost = rep(NA,num_tsteps) )
  
}