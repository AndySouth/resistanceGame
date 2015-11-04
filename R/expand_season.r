#' expand a seasonal pattern string in months to a vector of values by days,weeks or months
#'
#' initially assumes 28 days per month (4*7 day weeks) 336 day year
#'
#' @param season_string a season string of months:value;months:value e.g. "6:0.1;6:0.9"
#' @param return_tstep either 'months', 'weeks' or 'days'[default]
#' @examples
#' tst <- expand_season(season_string="6:0.1;6:0.9")
#' @return a vector of values by day
#' @export

expand_season <- function(season_string="6:0.1;6:0.9",
                          return_tstep = 'days') 
{
  
  #season_string <- '6:0;6:1'
  #I could maybe offer option to pass as '1;2;3;4' for one month each
  #could maybe even detect automatically by the result in tmp & just skip the next step
  
  tmp <- unlist(strsplit(season_string, split = ";"))
  
  tmp2 <- strsplit(tmp, split = ":") 
  
  by_month <- unlist(lapply(tmp2, function(x) rep(x[2],x[1])))
  
  if (return_tstep == 'days')
  {
    return(rep(by_month,each=28)) 
    
  } else if (return_tstep == 'weeks')
  {
    return(rep(by_month,each=4))   
    
  } else if (return_tstep == 'months')
  {
    return(by_month)   
    
  } else
  {
    stop("return_tstep should be one of days,weeks,months. It is:", return_tstep)
  }

  
}