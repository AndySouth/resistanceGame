#shinyGame1/server.r
#andy south 18/6/15
#first go at a simple IRM 'game'

library(shiny)


#global dataframe to hold results
runNum <- 0
maxGos <- 20
dF <- NULL
#logistic model params
K <- 1
# now set from input boxes
# rateGrowth <- 0.4
# rateInsecticideKill <- 1.2
# #allows changing the effect of resistance
# resistanceModifier <- 1.5

#if I scale all measures between 0 & 1 that may make life a lot easier later on


shinyServer(function(input, output) {
  
  
  #to set up data storage etc. for the simulation
  startSim <- function(){
    
    runNum <<- 0
    
    dF <<- data.frame( pyrUse = rep(NA,maxGos),
                       ddtUse = rep(NA,maxGos),
                       opsUse = rep(NA,maxGos),
                       carUse = rep(NA,maxGos),
                       vectorPop = rep(NA,maxGos),
                       pyrResist = rep(NA,maxGos),
                       cost = rep(NA,maxGos) )
    
    
    #if I scale all measures between 0 & 1 that may make life a lot easier later on
    
    #set start value for vector popn
    dF$vectorPop[1] <<- 0.3
    #set start value for pyrethroid resistance
    dF$pyrResist[1] <<- 0.01
    #set start for cost
    dF$cost[1] <<- 0
    
    
  }
  
  
  
  # advance one timestep  ##########################
  runApply <- reactive({
    
    cat("in runApply button=",input$aButtonRun,"\n")
    
    #add dependency on the button
    if ( input$aButtonRun == 0 ) 
    {
      startSim()
      
#       #set start value for vector popn
#       dF$vectorPop[1] <<- 100
#       #set start value for pyrethroid resistance
#       dF$pyrResist[1] <<- 20
#       #set start for cost
#       dF$cost[1] <<- 0
      return()
    }

    
    #isolate reactivity of other objects
    isolate({
      #to allow this to be reset later
      #remember global assignment <<-
      runNum <<- runNum + 1
 
      #i could get runs to restart when they get to maxGos ?
      #or could I extend the dF and allow it to go on indefinitely
      if (runNum >= nrow(dF))
      {
        dF <<- rbind(dF,dF[1,]) #copy row 1 on end
        dF[nrow(dF),] <<- NA #set to NA just in case
        maxGos <<- maxGos+1 #increment
      }

      
      #record which insecticide used (for plotting)
      if ( input$pyrOn ) dF$pyrUse[runNum] <<- 1 
      if ( input$ddtOn ) dF$ddtUse[runNum] <<- 1 
      if ( input$opsOn ) dF$opsUse[runNum] <<- 1 
      if ( input$carOn ) dF$carUse[runNum] <<- 1 
      
      
      ## increment vector populations
      ## based on insecticide used and resistance
      
      #set input parameters here to keep formulas more manageable
      rateGrowth <- input$rateGrowth
      rateInsecticideKill <- input$rateInsecticideKill
      resistanceModifier <- input$resistanceModifier
        
      
      ## if no insecticides used
      if (sum( dF$pyrUse[runNum],dF$ddtUse[runNum],dF$opsUse[runNum],dF$carUse[runNum],na.rm=TRUE) == 0)
      {
        
        #logistic model
        dF$vectorPop[runNum+1] <<- dF$vectorPop[runNum] + rateGrowth * 
                                                          dF$vectorPop[runNum] * (1-dF$vectorPop[runNum]/K)
        

      ## if just ddt or pyrethroid (i.e. no ops or carb)                
      } else if (sum( dF$opsUse[runNum],dF$carUse[runNum],na.rm=TRUE) == 0)
      {
        #no resistance : pop decline
        #mid resistance : stay constant
        #high resistance : pop increase
        
        dF$vectorPop[runNum+1] <<- dF$vectorPop[runNum] + 
          rateGrowth * dF$vectorPop[runNum] * (1-dF$vectorPop[runNum]/K) -   #density dependence
          rateInsecticideKill * dF$vectorPop[runNum] *                       #insecticide
          (1-dF$pyrResist[runNum]^(1/resistanceModifier))                    #resistance  
        
      #no kill at resistance=1, a higher resistance modifier means that lower resistances have a greater effect
      #on reducing mortality

      ## ops or car used          
      } else 
      {
        #effective insecticide reduce popn.
        dF$vectorPop[runNum+1] <<- dF$vectorPop[runNum] + 
                                   rateGrowth * dF$vectorPop[runNum] * (1-dF$vectorPop[runNum]/K) -   #density dependence
                                   rateInsecticideKill * dF$vectorPop[runNum]                         #insecticide
      }

      
      ## increment resistance
      ## as a first test, just increase resistance to pyrethroids
      #if pyr or ddt are used
      if ( input$pyrOn || input$ddtOn ) 
        #dF$pyrResist[runNum+1] <<- dF$pyrResist[runNum] * 1.2
        #constraining resistance to 1
        dF$pyrResist[runNum+1] <<- dF$pyrResist[runNum] +
                                   0.2 * (1 - dF$pyrResist[runNum])
      else 
        dF$pyrResist[runNum+1] <<- dF$pyrResist[runNum] * 0.8        

            
      #increment cost (can insert relative costs here)
      dF$cost[runNum+1] <<- dF$cost[runNum] + 
                            input$pyrOn * 1 +
                            input$ddtOn * 2 +
                            input$opsOn * 5 +
                            input$carOn * 5 
        
      
      
    }) #end isolate   
  }) #end of runApply
  # run mortality seeking  ##########################
  restart <- reactive({
    
    cat("in restart button=",input$aButtonRestart,"\n")
    
    #add dependency on the button
    if ( input$aButtonRestart > 0 ) 
    {
      startSim()
      
#       #todo put these into a function that can be called at start
#       runNum <<- 0
#       #zero by re-creating the dF
#       dF <<- data.frame( pyrUse = rep(NA,maxGos),
#                         ddtUse = rep(NA,maxGos),
#                         opsUse = rep(NA,maxGos),
#                         carUse = rep(NA,maxGos),
#                         vectorPop = rep(NA,maxGos),
#                         pyrResist = rep(NA,maxGos),
#                         cost = rep(NA,maxGos) )
#       #set start value for vector popn
#       dF$vectorPop[1] <<- 100
#       #set start value for pyrethroid resistance
#       dF$pyrResist[1] <<- 0.2
#       #set start for cost
#       dF$cost[1] <<- 0
    }
  
  }) #end of restart
  
  
  output$plot1 <- renderPlot({
    
    #check if restart has been pressed
    restart()
    #check if apply has been pressed
    runApply()
    
    cat("in plot1 runNum=",runNum,"\n")
    
    #isolate reactivity of other objects
    isolate({
 
      if ( runNum == 0 ) return()
      
      #set up space for a number of plots
      par(mfrow=c(4,1), mar=c(0,4,2,0)) #bltr
      
      #plot insecticide use
      #adj=0 to left justify title
      plot(dF$carUse, axes=FALSE, col='orange', xlim=c(0,nrow(dF)), ylim=c(0,5), main="insecticide used", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='' )
      points(dF$opsUse*2, col='blue')    
      points(dF$ddtUse*3, col='red') 
      points(dF$pyrUse*4, col='green')  
      #to add x axis labels, las=1 to make labels horizontal
      axis(2,at=1:4,labels=c('carb','ops','ddt','pyr'),las=1,cex.axis=1.3, tick=FALSE)
    
      cat(dF$vectorPop,"\n")
      
      #plot vector population
      plot.default(dF$vectorPop, axes=FALSE, ylim=c(0,1), type='l', main="vector population", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
      axis(2,at=c(0,1), labels=c(0,1), las=1, cex.axis=1.3, tick=FALSE)
           
      #plot resistance (can have diff colour lines for diff insecticides)
      plot.default(dF$pyrResist, axes=FALSE, ylim=c(0,1), type='l', col='green', main="resistance to pyrethroids", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
      #to add x axis labels, las=1 to make labels horizontal
      #for resistance constrain 0-1
      axis(2,at=c(0,1), labels=c(0,1), las=1, cex.axis=1.3, tick=FALSE)
            
      #plot cost
      plot.default(dF$cost, axes=FALSE, type='l', main="cost", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
      
      
    }) #end isolate   
  })
  
})