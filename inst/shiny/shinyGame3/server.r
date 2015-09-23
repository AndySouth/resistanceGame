#shinyGame3/server.r
#andy south 10/9/15

library(shiny)

#library(devtools)
#install_github("andysouth/resistanceGame")
library(resistanceGame)

#global dataframe to hold results
tstep <- 0
num_tsteps <- 20
#dF <- NULL
l_time <- NULL

#read config files into a list
#later could offer option to read different one
l_config <- resistanceGame::read_config()



shinyServer(function(input, output) {
  
  
  #to set up data storage etc. for the simulation
  startSim <- function(){
    
    tstep <<- 0
    
    l_time <<- init_sim2(num_tsteps=num_tsteps, l_config=l_config)
    
    #set start value for vector popn
    l_time[[1]]$pop <<- 0.3
    #set start value for resistance
    l_time[[1]]$resist <<- 0.01

    
    
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
      
      #todo work out how to be able to run this for multiple timesteps

      
      #to allow this to be reset later
      #remember global assignment <<-
      tstep <<- tstep + 1
 
      #i could get runs to restart when they get to num_tsteps ?
      #or could I extend the dF and allow it to go on indefinitely
#       if (tstep >= nrow(dF))
#       {
#         dF <<- rbind(dF,dF[1,]) #copy row 1 on end
#         dF[nrow(dF),] <<- NA #set to NA just in case
#         num_tsteps <<- num_tsteps+1 #increment
#       }

      
      cat(input$controls_used,"\n")
      
      # set config file control_plan from inputs
      #first just set start & stop to the whole time
      l_config <<- config_plan(l_config, control_id = input$controls_used, t_strt = 1, t_stop = num_tsteps )
      
      ## increment vector populations
      ## based on insecticide used and resistance
      
      #set input parameters here to keep formulas more manageable
      rate_growth <- input$rate_growth
      rate_insecticide_kill <- input$rate_insecticide_kill
      resistance_modifier <- input$resistance_modifier
      rate_resistance <- l_time[[tstep]]$resist
      carry_cap <- input$cc_modifier
      
      #set resistance increase & decrease to same
      resist_incr <- input$resist_incr
      resist_decr <- input$resist_decr   
      
      
      l_time <<- run_sim2( l_config=l_config, 
                          num_tsteps=input$tsteps_to_run,
                          pop_start=l_time[[tstep]]$pop,
                          rate_resistance_start=rate_resistance,
                          rate_growth = rate_growth,
                          carry_cap = carry_cap,
                          rate_insecticide_kill = rate_insecticide_kill,
                          resistance_modifier = resistance_modifier,
                          resist_incr = resist_incr,
                          resist_decr = resist_decr,
                          randomness = 0 )
      
      #dF <- rbind(dF,dF2)
           
#       # change population
#       dF$pop[tstep+1] <<- change_pop( pop = dF$pop[tstep],
#                                              rate_growth = rate_growth,
#                                              carry_cap = carry_cap,
#                                              rate_insecticide_kill = rate_insecticide_kill,
#                                              rate_resistance = rate_resistance,
#                                              resistance_modifier = resistance_modifier,
#                                              #initially just test whether any insecticide
#                                              insecticide_on = insecticide_on,
#                                              #initially just test whether pyr or ddt
#                                              resistance_on = resistance_on )
#       
#       # change resistance
#       dF$resist_pyr[tstep+1] <<- change_resistance( resistance = rate_resistance,
#                                                     resist_incr = resist_incr,
#                                                     resist_decr = resist_decr,
#                                                     #initially just test whether pyr or ddt
#                                                     resistance_on = resistance_on )

        
      
      
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
  
  # plot #####################
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
      #plot_sim(dF)
      plot_sim2(l_time)      
      
    }) #end isolate   
  }) #end plot1
  
  
  
  ## text about the simulation equations ###############
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

 
  
  # controls checkboxes for UI ###############
  output$checkboxGroupControls <- renderUI({   
    
    #checkboxGroupInput(inputId, label, choices, selected = NULL, inline = FALSE)
    
    # if wanted just one at a time (i.e. no mixtures) change to radioButtons
    
    choices <- l_config$controls$control_id
    #or could use names, but as they are optional could leave blank
    #choices <- l_config$controls$control_name
    
    checkboxGroupInput("controls_used", 
                       label = "Controls:",
                       choices = choices )
    #I guess I get at selections by :
    #input$controls_used #which should be a named list
    
  })   
     
  
})