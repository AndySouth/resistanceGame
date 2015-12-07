#shinyGame3/server.r
#andy south 10/9/15

library(shiny)

#library(devtools)
#install_github("andysouth/resistanceGame")
library(resistanceGame)

#global list to hold results
tstep <- 0
l_time <- NULL


#read config files into a list, this is the old carrying capacity driven one
#later could offer option to read different one
l_config <- resistanceGame::read_config(in_folder=system.file("extdata","config_oldcc_no_control", package="resistanceGame"))



shinyServer(function(input, output) {
  
  
  #to set up data storage etc. for the simulation
  startSim <- function(){
    
    tstep <<- 0
    
    #l_time <<- init_sim(num_tsteps=input$tsteps_to_run, l_config=l_config)
    l_time <<- NULL    
    
    
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
      
      #remember global assignment <<-
      #tstep <<- tstep + 1
      #tstep now incremented after simulation run
      
      
      #i could get runs to restart when they get to num_tsteps ?
      #or could I extend the dF and allow it to go on indefinitely
#       if (tstep >= nrow(dF))
#       {
#         dF <<- rbind(dF,dF[1,]) #copy row 1 on end
#         dF[nrow(dF),] <<- NA #set to NA just in case
#         num_tsteps <<- num_tsteps+1 #increment
#       }

      
      cat(input$controls_used," length:",length(input$controls_used),"\n")
      
      # set config file control_plan from inputs
      #first just set start & stop to the whole time
      
      # tryting to get it to work when no control
      if (length(input$controls_used)==0)
        l_config$control_plan <<- l_config$control_plan[0,]
      else
        l_config <<- config_plan(l_config, control_id = input$controls_used, t_strt = 1, t_stop = input$tsteps_to_run )
      
      ## increment vector populations
      ## based on insecticide used and resistance
      
      
      if (is.null(l_time))
      {
        pop <- input$pop_start
        resist_freq <- input$resist_start
      } else
      {
        pop <- l_time[[tstep]]$pop
        resist_freq <- l_time[[tstep]]$resist
      }
      
      
      #set input parameters here to keep formulas more manageable
      rate_growth <- input$rate_growth
      rate_insecticide_kill <- input$rate_insecticide_kill
      resistance_modifier <- input$resistance_modifier
      carry_cap <- input$cc_modifier
      
      #set resistance increase & decrease to same
      resist_incr <- input$resist_incr
      resist_decr <- input$resist_decr   
      
      
      l_time_this <<- run_sim_oldcc( l_config=l_config, 
                          num_tsteps=input$tsteps_to_run,
                          pop_start=pop,
                          resist_freq_start=resist_freq,
                          rate_growth = rate_growth,
                          carry_cap = carry_cap,
                          rate_insecticide_kill = rate_insecticide_kill,
                          resistance_modifier = resistance_modifier,
                          resist_incr = resist_incr,
                          resist_decr = resist_decr,
                          randomness = 0 )
      
      #concatenate new time series onto existing
      #l_time <<- c(l_time, l_time_this)
      #miss off first time step of the new one, otherwise this is a repeat of last tstep of previous
      l_time <<- c(l_time, l_time_this[-1])
               
      #set tstep to the last one in the current run  
      #this is -1 to miss repeated tstep (last tstep of one run is same as first tstep of next)
      tstep <<- tstep -1 + input$tsteps_to_run
      
      cat("tstep:",tstep," length(l_time):",length(l_time),"\n")
      
      
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
 

      if ( tstep == 0 ) {
        plot.new()
        mtext("press the advance... button on the left to start the simulation")
        return()
      }

      
      #put this plotting into a package function
      #so that it can be called from elsewhere, e.g. to plot scenarios in a document
      #initially just get function to accept the dataframe with use_*, pop & resist_pyr
      plot_sim_oldcc(l_time)      
      
    }) #end isolate   
  }) #end plot1
  
  
  ## show config files ###############
  ## SUPERCEDED by the table methods below 
  #output$show_config_files <- renderText({ 
  output$show_config_files <- renderPrint({ 
      
    print("The relationships between vectors, controls and resistance mechanisms are specified in simple 
          configuration files. Here is a simple example of a collection of such configuration files :/n")
      
    
    print("/nplaces.csv/n")
    places <- read.csv( system.file('extdata','config1','places.csv', package='resistanceGame'))
    print(places)    
    
    print("/nvectors.csv/n")
    vectors <- read.csv( system.file('extdata','config1','vectors.csv', package='resistanceGame'))
    print(vectors)
    
    print("/ncontrols.csv/n")
    controls <- read.csv( system.file('extdata','config1','controls.csv', package='resistanceGame'))
    print(controls)
    
    print("\nresistances.csv\n")
    resistances <- read.csv( system.file('extdata','config1','resistances.csv', package='resistanceGame'))
    print(resistances)

    
  }) #end show_config_files    


  ## table of places from config files ###############  
  output$table_places <- renderTable({
    
    places <- read.csv( system.file('extdata','config1','places.csv', package='resistanceGame'))
  })  
    
  ## table of vectors from config files ###############  
  output$table_vectors <- renderTable({
    
    vectors <- read.csv( system.file('extdata','config1','vectors.csv', package='resistanceGame'))
  })  
  
  ## table of controls from config files ###############  
  output$table_controls <- renderTable({
    
    controls <- read.csv( system.file('extdata','config1','controls.csv', package='resistanceGame'))
  })  
    
  ## table of resistances from config files ###############  
  output$table_resistances <- renderTable({
    
    resistances <- read.csv( system.file('extdata','config1','resistances.csv', package='resistanceGame'))
  })    
  
  
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

A2) If N < low_threshold : N = low_threshold

An additional line to stop the vector population from going extinct (which would be unrealistic).


The equations for the change in resistance are even simpler.

If an insecticide prompting resistance is present :

B) resistance[t+1] = resistance[t] + resist_incr * (1 - resistance[t])

If no insecticide prompting resistance :

C) resistance[t+1] = resistance[t]  * (1 - resist_decr)

          
These simply make resistance go up towards a plateau when the insecticide is present and down towards 0 when it is absent.")
    
    
    
  }) #end about

 
  
  # controls checkboxes for UI ###############
  output$checkboxGroupControls <- renderUI({   
    
    #checkboxGroupInput(inputId, label, choices, selected = NULL, inline = FALSE)
    
    # if wanted just one at a time (i.e. no mixtures) change to radioButtons
    
    choices <- l_config$controls$control_id
    #or could use names, but as they are optional could leave blank
    #choices <- l_config$controls$control_name
    
#     checkboxGroupInput("controls_used", 
#                        label = "Controls : tick boxes to turn on & off",
#                        choices = choices,
#                        selected = choices[1]) #to select first box as default

    radioButtons("controls_used", 
                       label = "Controls : tick boxes to turn on & off",
                       choices = choices,
                       selected = choices[1]) #to select first box as default        
    
    #I guess I get at selections by :
    #input$controls_used #which should be a named list
    
  })   

  
       
  
})