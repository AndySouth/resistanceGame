#' plot simulation pop results for newest flexible config driven simulations
#'
#' pop only (with emergence) not interventions or resistance
#'
#' @param l_time list containing simulation results
#' @param plot_emergence whether to add emergence rate to population plot
#' @param verbose whether to output diagnostics to console
#' @param time_label time label to add to x axis, default='weeks'
#' @param plot_type 'month' to just plot points per month, '6month' to plot 6months
#' @param subset_data whether to subset_data, set to FALSE when calling from other plot funcs 
#' @param leg_pos legend position defaults to 'bottomleft', other combinations of 'top','right' etc.
#' @param axis_x whether to add x axis to pop lot. default FALSE because axis shown under lower plot
#' @param title plot title added at top
#' 
#' @examples
#' #blank plot
#' l_time <- init_sim(20)
#' plot_sim_pop(l_time)
#' #default run
#' plot_sim_pop( run_sim())
#' #modify params
#' plot_sim_pop( run_sim( insecticide_kill = 0.3, resist_incr = 0.05 ))
#' @return maybe nothing, produces a plot
#' @export

plot_sim_pop <- function(l_time, 
                     plot_emergence=TRUE,
                     verbose=FALSE,
                     time_label='weeks',
                     plot_type='month', #='line', 6month, year)
                     subset_data=TRUE,
                     leg_pos="bottomleft",
                     axis_x=FALSE,
                     title=NULL )
{
  
  
  #hack to avoid problem with null lists
  if (is.null(l_time))
  {
    warning("null list")
    return()
  }
  
  #to subset monthly points (every 4 weeks)
  if (subset_data)
  {
    if ( plot_type == 'month' )  
    {
      month_weeks <- seq(1,length(l_time),4)
      l_time <- l_time[month_weeks]
      time_label='months'
    } else if ( plot_type == '6month' )
    {
      month6_weeks <- seq(1,length(l_time),4*6)
      l_time <- l_time[month6_weeks]
      time_label='6 months'   
    } else if ( plot_type == 'year' )
    {
      year_weeks <- seq(1,length(l_time),4*12)
      l_time <- l_time[year_weeks]
      time_label='year'   
    }    
  }

  
  
  #plot vector population
  
  pop <- sapply(l_time, "[[", "pop")
  
  if (verbose) cat("plotting pop :",pop,"\n")
  
  #for if just plotting months
  #create indices of months every 4 weeks
  month_indices <- seq(1,length(pop),4)  
  
  if (plot_type == 'month' | plot_type == '6month' | plot_type == 'year')
  {
    #plot as points
    #plot.default(pop[month_indices], axes=FALSE, ylim=c(0,1), type='p', main="vector population", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
    #above looked right but axis & emergence were wrong
    #instead could replace non month points with NAs
    #pop[-month_indices] <- NA
    #above did work but not necessary as data now subsetted at start
    plot.default(pop, axes=FALSE, ylim=c(0,1), type='p', main=title, adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='', xlab ='')
  }   else 
  {
    #plot as a line
    plot.default(pop, axes=FALSE, ylim=c(0,1), type='l', main=title, adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='', xlab ='')
  } 
  
  
  #y axis
  axis(2,at=c(0,1), labels=c('lo','hi'), las=1, cex.axis=1.3, tick=TRUE)
  
  #x axis, let R set values
  if (axis_x)
    axis(1)
  
  #
  if (plot_emergence)
  {
    emergence <- sapply(l_time, "[[", "emergence")
    lines( emergence, col='red', lty=2 ) #lty=3 dotted, 2 dashed
    legend( leg_pos, legend=c("popn","emergence"), 
            col=c("black","red"), lty=c(1,2), bty="n" )
  }
  

  # if (!is.null(title))
  # {
  #   mtext(title)
  # }  
  
}
