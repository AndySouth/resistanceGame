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