#' plot simulation results for newest flexible config driven simulations
#'
#' plot insecticide use and resulting changes in population and resistance
#'
#' @param l_time list containing simulation results
#' @param plot_emergence whether to add emergence rate to population plot
#' @param plot_thresholds whether to add WHO resistance threholds to resistance plot
#' @param verbose whether to output diagnostics to console
#' @param time_label time label to add to x axis, default='weeks'
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
                     time_label='weeks') 
{
  
  
  #hack to avoid problem with null lists
  if (is.null(l_time))
  {
    warning("null list")
    return()
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
  
  #xaxis
  #axis( 1, at=seq(0,1,length.out=ncol( mat_control ) ), labels= colnames( mat_control ), las= 2 )
  #num_x_labs <- ncol( mat_control )
  num_x_labs <- 5
  axis( 1, at=seq(0,1,length.out=num_x_labs), labels=seq(0,ncol( mat_control ),length.out=num_x_labs))
  #y axis
  axis( 2, at=seq(0,1,length.out=nrow( mat_control ) ), labels= rownames( mat_control ), las= 2)
  
  #cat(dF$pop,"\n")
  
  #plot vector population
  
  pop <- sapply(l_time, "[[", "pop")
  
  if (verbose) cat("plotting pop :",pop,"\n")
  
  plot.default(pop, axes=FALSE, ylim=c(0,1), type='l', main="vector population", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
  axis(2,at=c(0,1), labels=c('lo','hi'), las=1, cex.axis=1.3, tick=TRUE)
  
  #
  if (plot_emergence)
  {
    emergence <- sapply(l_time, "[[", "emergence")
    lines( emergence, col='red', lty=2 ) #lty=3 dotted, 2 dashed
    legend( "bottomleft", legend=c("popn","emergence"), 
            col=c("black","red"), lty=c(1,2), bty="n" )
  }
  
  
  
  #plot resistance (can have diff colour lines for diff insecticides)
  
  resist <- sapply(l_time, "[[", "resist")
  
  if (verbose) cat("plotting resist :",resist,"\n")
  
  #plot as a line
  plot.default(resist, axes=FALSE, ylim=c(0,1), type='l', col='green', main="resistance frequency", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
  
  #to add x axis labels, las=1 to make labels horizontal
  #for resistance constrain 0-1
  axis(2,at=c(0,1), labels=c(0,1), las=1, cex.axis=1.3, tick=TRUE)
  
  #add an x axis to the lower plot, let R set values
  axis(1)
  
  #add WHO resistance thresholds to lower plot
  #mortalities
  #>0.1       < 90% = resistant
  #0.1-0.02   90-98 = suggested resistance
  #<0.02      > 98% = susceptible
  
  if ( plot_thresholds )
  {
    abline(h = 0.1, col='orange', lty=3)
    abline(h = 0.02, col='blue', lty=3)  
    
    legend( "topleft", legend=c("resistant <90% mortality","susceptible >98% mortality"), 
            col=c("orange","blue"), lty=c(3), bty="n" )
  }

  mtext('weeks',side=1,line=1)
  
}
