#shinyGame2/server.r
#andy south 26/8/15

library(shiny)

#library(devtools)
#install_github("andysouth/resistanceGame")

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
      rateResistance <- input$rateResistance  
      K <- input$ccModifier
      
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
      ## if pyr or ddt are used
      if ( input$pyrOn || input$ddtOn ) 
        #constraining resistance to 1
        dF$pyrResist[runNum+1] <<- dF$pyrResist[runNum] +
                                   rateResistance * (1 - dF$pyrResist[runNum])
      else 
        dF$pyrResist[runNum+1] <<- dF$pyrResist[runNum] * (1-rateResistance)        

            
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
 
      #if ( runNum == 0 ) return()
      if ( runNum == 0 ) {
        plot.new()
        mtext("press the advance... button on the left to start the simulation")
        return()
      }

            
      #set up space for a number of plots
      #par(mfrow=c(4,1), mar=c(0,4,2,0)) #bltr
      #disbaled cost for now so just need 3
      par(mfrow=c(3,1), mar=c(2,4,2,0)) #bltr
            
      #plot insecticide use
      #adj=0 to left justify title
      #, pch=15 filled square
      plot(dF$carUse, axes=FALSE, col='orange', pch=15, cex=2, xlim=c(0,nrow(dF)), ylim=c(0.5,4.5), main="insecticide used", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='' )
      points(dF$opsUse*2, col='blue', pch=15, cex=2)    
      points(dF$ddtUse*3, col='red', pch=15, cex=2) 
      points(dF$pyrUse*4, col='green', pch=15, cex=2)  
      #to add x axis labels, las=1 to make labels horizontal
      axis(2,at=1:4,labels=c('carb','ops','ddt','pyr'),las=1,cex.axis=1.3, tick=FALSE)
    
      cat(dF$vectorPop,"\n")
      
      #plot vector population
      plot.default(dF$vectorPop, axes=FALSE, ylim=c(0,1), type='l', main="vector population", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
      axis(2,at=c(0,1), labels=c('lo','hi'), las=1, cex.axis=1.3, tick=TRUE)
           
      #plot resistance (can have diff colour lines for diff insecticides)
      plot.default(dF$pyrResist, axes=FALSE, ylim=c(0,1), type='l', col='green', main="resistance to pyrethroids", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
      #to add x axis labels, las=1 to make labels horizontal
      #for resistance constrain 0-1
      axis(2,at=c(0,1), labels=c(0,1), las=1, cex.axis=1.3, tick=TRUE)
      
      #add an x axis to the lower plot, let R set values
      axis(1)
      
      #disabled cost for now      
      #plot cost
      #plot.default(dF$cost, axes=FALSE, type='l', main="cost", adj=0, cex.main=1.4, font.main=1, frame.plot=FALSE, ylab='')
      
      
    }) #end isolate   
  }) #end plot1
  
  
  
  ## text about the simulation equations
  output$about <- renderText({ 
    
    print("This simple simulation could go on in the background of the game, game players could be provided selected information, e.g. with added randomness.

Within the game equation parameters could be altered based on game play e.g. : 
dry season : low growth rate and/or low carrying capacity
rain events : increased growth rate or carrying capacity
'better' insecticides : increased insecticide kill rate
poor insecticide application : decreased insecticide kill rate, increased resistance change rate.

These are the simple equations that drive the simulation.

A) N[t+1] = N[t] + rateGrowth * N[t] * (1-N[t] / carryingCapacity)
                         - (rateInsecticideKill * N[t]
                         * (1-resistance[t] ^ (1/resistanceModifier) ) )

Where N[t] is population now, and N[t+1] is population in the next time step.

Line 1 does density dependence it just makes the population increase in a curve until it reaches the carrying capacity. This is the logistic model.
Line 2 does killing of vectors by the insecticide
Line 3 modifies vectors killed in line 2 according to resistance

So in line 3, '1-resitance[t]' ensures that fewer vectors are killed when resistance is higher, and that no vectors are killed when resistance=1.

The equation for the change in resistance is even simpler.

If an insecticide prompting resistance is present :
B) resistance[t+1] = resistance[t] + rateResistance * (1 - resistance[t])

If no insecticide prompting resistance :
C) resistance[t+1] = resistance[t]  * (1 - resistance[t])

These simply make resistance go up towards a plateau when the insecticide is present and down towards 0 when it is absent.")
    
    
    
  }) #end about
  
  
  ## lookup table
  #output$table <- renderTable(, { 
  #specifying num digits in each table column must match ncol(x)+1
  output$table <- renderTable( digits=c(0,0,0,0,0,1,1,2,2), { 
    
    #add dependence on the advance button
    #so that the table changes when parameter values are changed and advance is pressed
    runApply()
    
    #default lookup table
    resistanceGame::create_lookup(write_csv=NULL,
                                  rate_growth = input$rateGrowth,
                                  rate_insecticide_kill = input$rateInsecticideKill,
                                  resistance_modifier = input$resistanceModifier,
                                  carry_cap = input$ccModifier,
                                  #beware increase & decrease set to same rate here
                                  resist_incr = input$rateResistance,
                                  resist_decr = input$rateResistance )
    

    
  }) #end table
    
  
})