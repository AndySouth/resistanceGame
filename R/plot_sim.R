#' plot simulation results
#'
#' plot insecticide use and resulting changes in population and resistance
#'
#' @param dF dataframe containing simulation results
#' @examples
#' #blank plot
#' dF <- init_sim(20)
#' plot_sim(dF)
#' #default run
#' plot_sim( run_sim())
#' #modify params
#' plot_sim( run_sim( rate_insecticide_kill = 0.3, resist_incr = 0.05 ))
#' @return maybe nothing, produces a plot
#' @export

plot_sim <- function(dF) 
{
  
  #so that it can be called from elsewhere, e.g. to plot scenarios in a document
  #initially just get function to accept the dataframe with use_*, pop & resist_pyr
  #plot_sim(dF)
  
  
  #set up space for a number of plots
  #par(mfrow=c(4,1), mar=c(0,4,2,0)) #bltr
  #disbaled cost for now so just need 3
  par(mfrow=c(3,1), mar=c(2,4,2,0)) #bltr
  
  #plot insecticide use
  #adj=0 to left justify title
  #, pch=15 filled square
  plot(dF$use_car, axes=FALSE, col='orange', pch=15, cex=2, xlim=c(0,nrow(dF)), ylim=c(0.5,4.5), main="insecticide used", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='' )
  points(dF$use_ops*2, col='blue', pch=15, cex=2)    
  points(dF$use_ddt*3, col='red', pch=15, cex=2) 
  points(dF$use_pyr*4, col='green', pch=15, cex=2)  
  #to add x axis labels, las=1 to make labels horizontal
  axis(2,at=1:4,labels=c('carb','ops','ddt','pyr'),las=1,cex.axis=1.3, tick=FALSE)
  
  #cat(dF$pop,"\n")
  
  #plot vector population
  plot.default(dF$pop, axes=FALSE, ylim=c(0,1), type='l', main="vector population", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
  axis(2,at=c(0,1), labels=c('lo','hi'), las=1, cex.axis=1.3, tick=TRUE)
  
  #plot resistance (can have diff colour lines for diff insecticides)
  plot.default(dF$resist_pyr, axes=FALSE, ylim=c(0,1), type='l', col='green', main="resistance to pyrethroids", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
  #to add x axis labels, las=1 to make labels horizontal
  #for resistance constrain 0-1
  axis(2,at=c(0,1), labels=c(0,1), las=1, cex.axis=1.3, tick=TRUE)
  
  #add an x axis to the lower plot, let R set values
  axis(1)
  
  #disabled cost for now      
  #plot cost
  #plot.default(dF$cost, axes=FALSE, type='l', main="cost", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
  
   
}


#' plot simulation results for new flexible simulations
#'
#' plot insecticide use and resulting changes in population and resistance
#'
#' @param l_time list containing simulation results
#' @examples
#' #blank plot
#' l_time <- init_sim2(20)
#' plot_sim2(l_time)
#' #default run
#' plot_sim2( run_sim2())
#' #modify params
#' plot_sim2( run_sim2( rate_insecticide_kill = 0.3, resist_incr = 0.05 ))
#' @return maybe nothing, produces a plot
#' @export

plot_sim2 <- function(l_time) 
{
  

  #set up space for a number of plots
  #par(mfrow=c(4,1), mar=c(0,4,2,0)) #bltr
  #disbaled cost for now so just need 3
  par(mfrow=c(3,1), mar=c(2,4,2,0)) #bltr
  
  #plot insecticide use
  #in v2 I need to get it to cope with variable num controls
  #take controls from l_time (it does have all potential controls from the config)
  names(l_time[[1]]$controls_used)
  #tricky to get out the time series of use for each insecticide
  #actaully by chance found it might not be too hard
  #this produces a matrix of control names by time
  mat_control <- sapply(l_time, "[[", "controls_used")
  
  if ( sum(mat_control,na.rm=TRUE) > 0 )
  {
    #this multiplies by row numbers
    mat_control <- mat_control * row(mat_control)
    #plot pattern of control use
    image(t(mat_control),yaxt="n",xaxt="n",col=rainbow(n=nrow(mat_control)))
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
  
#old way  
#   #adj=0 to left justify title
#   #, pch=15 filled square
#   plot(dF$use_car, axes=FALSE, col='orange', pch=15, cex=2, xlim=c(0,nrow(dF)), ylim=c(0.5,4.5), main="control used", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='' )
#   points(dF$use_ops*2, col='blue', pch=15, cex=2)    
#   points(dF$use_ddt*3, col='red', pch=15, cex=2) 
#   points(dF$use_pyr*4, col='green', pch=15, cex=2)  
#   #to add x axis labels, las=1 to make labels horizontal
#   axis(2,at=1:4,labels=c('carb','ops','ddt','pyr'),las=1,cex.axis=1.3, tick=FALSE)
  
  #cat(dF$pop,"\n")
  
  #plot vector population
  
  pop <- sapply(l_time, "[[", "pop")
  
  plot.default(pop, axes=FALSE, ylim=c(0,1), type='l', main="vector population", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
  axis(2,at=c(0,1), labels=c('lo','hi'), las=1, cex.axis=1.3, tick=TRUE)
  
  
  
  
  #plot resistance (can have diff colour lines for diff insecticides)
  
  resist <- sapply(l_time, "[[", "resist")
  
  plot.default(resist, axes=FALSE, ylim=c(0,1), type='l', col='green', main="resistance", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
  #to add x axis labels, las=1 to make labels horizontal
  #for resistance constrain 0-1
  axis(2,at=c(0,1), labels=c(0,1), las=1, cex.axis=1.3, tick=TRUE)
  
  #add an x axis to the lower plot, let R set values
  axis(1)
  
  #disabled cost for now      
  #plot cost
  #plot.default(dF$cost, axes=FALSE, type='l', main="cost", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
  
  
}

