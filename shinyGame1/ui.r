#shinyGame1/ui.r
#andy south 18/6/15
#first go at a simple IRM 'game'

library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("flatly"),

  headerPanel("Insecticide Resistance Management 'Game model' prototype1"),  
  
  helpText("To demonstrate a potential vector population simulation for the game, press buttons on the left to advance the simulation plots will appear on the right."),
                  
  fluidRow(
    
    column(4,
                 
      
      helpText("Choose one or more insecticides to use in each time step. ",
               "This version only includes metabolic resistance prompted by use of either pyr or ddt."),    
      # may want to set these so that you can just choose 
      # one at a time (i.e. no mixtures)
      checkboxInput('pyrOn', 'pyrethroid', TRUE),
      checkboxInput('ddtOn', 'ddt', FALSE),
      checkboxInput('opsOn', 'organophosphate', FALSE),
      checkboxInput('carOn', 'carbamate', FALSE),
      
      wellPanel(
        actionButton('aButtonRun', 'advance 1 timestep'),
        br(),br(),
        actionButton('aButtonRestart', 'restart')
      ),
      
      #growth parameters
      numericInput('rateGrowth', 'population growth rate', value = 0.4, min = 0.1, max = 2, step = 0.1),
      numericInput('rateInsecticideKill', 'insecticide kill rate', value = 0.4, min = 0.1, max = 2, step = 0.1),
      
      numericInput('resistanceModifier', 'resistance modifier', value = 1, min = 0.1, max = 5, step = 0.1),
      helpText("Values above 1 for resistance modifier cause lower resistances to have a greater effect on reducing mortality")
      
    ), #end column
  column(8,
    plotOutput('plot1')
  ) #end column
  ) #end fluid row
))