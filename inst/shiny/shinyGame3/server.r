#shinyGame3/server.r
#andy south 10/9/15

library(shiny)

#library(devtools)
#install_github("andysouth/resistanceGame")
library(resistanceGame)

#global dataframe to hold results
tstep <- 0
num_tsteps <- 20
dF <- NULL



#if I scale all measures between 0 & 1 that may make life a lot easier later on


shinyServer(function(input, output) {
  
  
  #to set up data storage etc. for the simulation
  startSim <- function(){
    
    tstep <<- 0
    
    dF <<- init_sim(num_tsteps=num_tsteps)
    
    #if I scale all measures between 0 & 1 that may make life a lot easier later on
    
    #set start value for vector popn
    dF$pop[1] <<- 0.3
    #set start value for pyrethroid resistance
    dF$resist_pyr[1] <<- 0.01
    #set start for cost
    dF$cost[1] <<- 0
    
    
  }
  
  
  
  # advance one timestep  ##########################
  runApply <- reactive({
    
    #cat("in runApply button=",input$aButtonRun,"\n")
    
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
      tstep <<- tstep + 1
 
      #i could get runs to restart when they get to num_tsteps ?
      #or could I extend the dF and allow it to go on indefinitely
      if (tstep >= nrow(dF))
      {
        dF <<- rbind(dF,dF[1,]) #copy row 1 on end
        dF[nrow(dF),] <<- NA #set to NA just in case
        num_tsteps <<- num_tsteps+1 #increment
      }

      
      #record which insecticide used (for plotting)
      if ( input$use_pyr ) dF$use_pyr[tstep] <<- 1 
      if ( input$use_ddt ) dF$use_ddt[tstep] <<- 1 
      if ( input$use_ops ) dF$use_ops[tstep] <<- 1 
      if ( input$use_car ) dF$use_car[tstep] <<- 1 
      
      
      ## increment vector populations
      ## based on insecticide used and resistance
      
      #set input parameters here to keep formulas more manageable
      rate_growth <- input$rate_growth
      rate_insecticide_kill <- input$rate_insecticide_kill
      resistance_modifier <- input$resistance_modifier
      rate_resistance <- dF$resist_pyr[tstep]
      carry_cap <- input$cc_modifier
      
      #set resistance increase & decrease to same
      resist_incr <- input$resist_incr
      resist_decr <- input$resist_decr   
      
      insecticide_on <- input$use_pyr | input$use_ddt | input$use_ops | input$use_car
      resistance_on <-  input$use_pyr | input$use_ddt
      
      #cat("insecticide & resistance on ",insecticide_on, resistance_on,"\n")
      
      
      # todo get this to run_sim(num_tsteps=tsteps_to_run)
      # may need to rbind new results onto end of existing ones
      
      # change population
      dF$pop[tstep+1] <<- change_pop( pop = dF$pop[tstep],
                                             rate_growth = rate_growth,
                                             carry_cap = carry_cap,
                                             rate_insecticide_kill = rate_insecticide_kill,
                                             rate_resistance = rate_resistance,
                                             resistance_modifier = resistance_modifier,
                                             #initially just test whether any insecticide
                                             insecticide_on = insecticide_on,
                                             #initially just test whether pyr or ddt
                                             resistance_on = resistance_on )
      
      # change resistance
      dF$resist_pyr[tstep+1] <<- change_resistance( resistance = rate_resistance,
                                                    resist_incr = resist_incr,
                                                    resist_decr = resist_decr,
                                                    #initially just test whether pyr or ddt
                                                    resistance_on = resistance_on )
        

            
      #increment cost (can insert relative costs here)
      dF$cost[tstep+1] <<- dF$cost[tstep] + 
                            input$use_pyr * 1 +
                            input$use_ddt * 2 +
                            input$use_ops * 5 +
                            input$use_car * 5 
        
      
      
    }) #end isolate   
  }) #end of runApply
  
  
  # run mortality seeking  ##########################
  restart <- reactive({
    
    #cat("in restart button=",input$aButtonRestart,"\n")
    
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
    
    #cat("in plot1 tstep=",tstep,"\n")
    
    #isolate reactivity of other objects
    isolate({
 
      #if ( tstep == 0 ) return()
      if ( tstep == 0 ) {
        plot.new()
        mtext("press the advance... button on the left to start the simulation")
        return()
      }

      
      #put this plotting into a package function
      #so that it can be called from elsewhere, e.g. to plot scenarios in a document
      #initially just get function to accept the dataframe with use_*, pop & resist_pyr
      plot_sim(dF)
      
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

A) N[t+1] = N[t] + rate_growth * N[t] * (1-N[t] / carryingCapacity)
                         - (rate_insecticide_kill * N[t]
                         * (1-resistance[t] ^ (1/resistance_modifier) ) )

Where N[t] is population now, and N[t+1] is population in the next time step.

Line 1 does density dependence it just makes the population increase in a curve until it reaches the carrying capacity. This is the logistic model.
Line 2 does killing of vectors by the insecticide
Line 3 modifies vectors killed in line 2 according to resistance

So in line 3, '1-resitance[t]' ensures that fewer vectors are killed when resistance is higher, and that no vectors are killed when resistance=1.

The equation for the change in resistance is even simpler.

If an insecticide prompting resistance is present :
B) resistance[t+1] = resistance[t] + rate_resistance * (1 - resistance[t])

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
                                  rate_growth = input$rate_growth,
                                  rate_insecticide_kill = input$rate_insecticide_kill,
                                  resistance_modifier = input$resistance_modifier,
                                  carry_cap = input$cc_modifier,
                                  #beware increase & decrease set to same rate here
                                  resist_incr = input$rate_resistance,
                                  resist_decr = input$rate_resistance )
    

    
  }) #end table
    
  
})