#' plot parts of config files to aid checking
#'
# @param locations dataframe from locations config file
#' 
#' @return maybe nothing, produces a plot
#' @export
#' 
plot_config_gdocs_emergences <- function()
{

  library(ggplot2)
  library(reshape2)
  
  
  #emers
  #emers2 <- melt(vecbyloc,variable.name='month')
  emers2 <- melt(vecbyloc,variable.name='month',measure.vars=paste0('m',c(1:12)))
  
  print(
    ggplot(emers2, aes(x=month, y=value, colour=id)) +
             #scale_x_discrete("month")
             facet_wrap('id') +
             theme(legend.position="none") +
             geom_point() 
  )#end print
  
}
