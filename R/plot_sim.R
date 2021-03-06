#' plot simulation results for newest flexible config driven simulations
#'
#' plot insecticide use and resulting changes in population and resistance
#'
#' @param l_time list containing simulation results
#' @param plot_emergence whether to add emergence rate to population plot
#' @param plot_thresholds whether to add WHO resistance threholds to resistance plot
#' @param verbose whether to output diagnostics to console
#' @param time_label time label to add to x axis, default='weeks'
#' @param plot_type 'month' to just plot points per month, '6month' to plot 6months
#' @param plot_resist 'frequency' or 'mortality'
#' @param leg_pos legend position defaults to 'bottomleft', other combinations of 'top','right' etc.
#' @param title plot title added at top
#' 
#' @examples
#' #blank plot
#' l_time <- init_sim(20)
#' plot_sim(l_time)
#' #default run
#' plot_sim( run_sim())
#' #modify params
#' plot_sim( run_sim( insecticide_kill = 0.3, resist_incr = 0.05 ))
#' @return maybe nothing, produces a plot
#' @export

plot_sim <- function(l_time, 
                     plot_emergence=TRUE,
                     plot_thresholds=TRUE,
                     verbose=FALSE,
                     time_label='weeks',
                     plot_type='month', #='line', 6month, year)
                     plot_resist='mortality', #frequency
                     leg_pos="bottomleft",
                     title=NULL )
{
  
  
  #hack to avoid problem with null lists
  if (is.null(l_time))
  {
    warning("null list")
    return()
  }
  
  #to subset monthly points (every 4 weeks)
  #if (weeks2months)
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
  
  #set up space for a number of plots
  #par(mfrow=c(4,1), mar=c(0,4,2,0)) #bltr
  #disbaled cost for now so just need 3
  par(mfrow=c(3,1), mar=c(2,4,2,0)) #bltr
  
  #plot insecticide use
  #in v2 I need to get it to cope with variable num controls
  #take controls from l_time (it does have all potential controls from the config)
  #names(l_time[[1]]$controls_used)
  #tricky to get out the time series of use for each insecticide
  #actaully by chance found it might not be too hard
  #this produces a matrix of control names by time
  #mat_control <- sapply(l_time, "[[", "controls_used")
  #drop=FALSE to cope when just one control
  mat_control <- sapply(l_time, function(x) x[["controls_used", drop=FALSE]])
  
  #testing
  if (verbose) print(mat_control)
  
  #hack to cope if just one control (otherwise it stops being a matrix & fails)
  if (is.null(nrow(mat_control)))
  {
    mat_control <- as.matrix(t(mat_control))
    rownames(mat_control) <- names(l_time[[1]]$controls_used)
  }
  
  
  if ( sum(mat_control,na.rm=TRUE) > 0 )
  {
    
    #this multiplies by row numbers
    mat_control <- mat_control * row(mat_control)
    #plot pattern of control use, zlim keeps colours constant
    image(t(mat_control),yaxt="n",xaxt="n",zlim=c(1,length(l_time[[1]]$controls_used)),col=rainbow(7))
    
  } else {
    
    mat_control[] <- 0
    #blank plot
    image(t(mat_control),yaxt="n",xaxt="n",col='white')
    
  }
  
  if (!is.null(title))
  {
    mtext(title)
  }
  

  
  #xaxis
  #axis( 1, at=seq(0,1,length.out=ncol( mat_control ) ), labels= colnames( mat_control ), las= 2 )
  #num_x_labs <- ncol( mat_control )
  num_x_labs <- 5
  axis( 1, at=seq(0,1,length.out=num_x_labs), labels=seq(0,ncol( mat_control ),length.out=num_x_labs))
  #y axis
  axis( 2, at=seq(0,1,length.out=nrow( mat_control ) ), labels= rownames( mat_control ), las= 2)
  
  #cat(dF$pop,"\n")
  
  #######################
  #plot vector population
  #now in its own function
  plot_sim_pop(l_time=l_time, 
               plot_emergence=plot_emergence,
               verbose=verbose,
               time_label=time_label,
               subset_data=FALSE,
               title="vector population",
               leg_pos=leg_pos,
               plot_type=plot_type)
  
  
  #plot resistance (can have diff colour lines for diff insecticides)
  
  resist <- sapply(l_time, "[[", "resist")
  
  if (verbose) cat("plotting resist :",resist,"\n")
  
  if (plot_resist == 'mortality')
  {
    ylim <- c(1,0)
    main <- "mortality in resistance assay"
    labels=c(100,0)
  } else
  {
    ylim <- c(0,1) 
    main <- "resistance frequency"
    labels=c(0,1)
  }
  
  if (plot_type == 'month' | plot_type == '6month' | plot_type == 'year')
  {
    #plot as points
    #plot.default(resist[month_indices], axes=FALSE, ylim=ylim, type='p', col='green', main=main, adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
    #instead could replace non month points with NAs
    #resist[-month_indices] <- NA
    #above did work but not necessary as data now subsetted at start
    plot.default(resist, axes=FALSE, ylim=ylim, type='p', col='green', main=main, adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
  } else 
  {
    #plot as a line
    plot.default(resist, axes=FALSE, ylim=ylim, type='l', col='green', main=main, adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
  }
  
  #y axis labels, las=1 to make labels horizontal
  #0,1 for resist freq, 1,0 for mortality
  axis(2,at=c(0,1), labels=labels, las=1, cex.axis=1.2, tick=TRUE)
  
  #add an x axis to the lower plot, let R set values
  axis(1)
  #or use modify axis of upper plot
  #num_x_labs <- 5
  #axis( 1, at=seq(0,1,length.out=num_x_labs), labels=seq(0,ncol( mat_control ),length.out=num_x_labs))
  
  
  #add WHO resistance thresholds to lower plot
  #mortalities
  #>0.1       < 90% = resistant
  #0.1-0.02   90-98 = suggested resistance
  #<0.02      > 98% = susceptible
  
  if ( plot_thresholds )
  {
    if (plot_resist == 'mortality')
    {
      leg_pos="bottomleft"
    } else
    {
      leg_pos="topleft"    
    }
    
    
    abline(h = 0.1, col='orange', lty=3)
    abline(h = 0.02, col='blue', lty=3)  
    
    legend( leg_pos, legend=c("resistant <90% mortality","susceptible >98% mortality"), 
            col=c("orange","blue"), lty=c(3), bty="n" )
  }

  mtext(time_label,side=1,line=1)
  
}
