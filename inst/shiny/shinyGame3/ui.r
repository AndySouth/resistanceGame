#shinyGame3/ui.r
#andy south 10/9/15
#runs 20 tsteps at once so you don't have to keep pressing button

library(shiny)
library(shinythemes)


shinyUI(fluidPage(theme = shinytheme("flatly"),

  headerPanel("Insecticide Resistance Management 'Game model' prototype3"),  
  
  helpText("A vector simulation for an Insecticide Resistance Management game. This version runs whole scenario rather than step by step. Press buttons on the left to advance, plots will appear in the 'Simulation' tab. Feedback to southandy@gmail.com."),
  
  fluidRow(
    
    column(4,
                 
      helpText("Choose one or more controls to use in each time step. ",
               "Controls and resistances can be modified in configuration files."),    
      
#       checkboxInput('use_pyr', 'pyrethroid', TRUE),
#       checkboxInput('use_ddt', 'ddt', FALSE),
#       checkboxInput('use_ops', 'organophosphate', FALSE),
#       checkboxInput('use_car', 'carbamate', FALSE),
      
      #controls checkboxes got from config file
      uiOutput("checkboxGroupControls"),
      
      
      wellPanel(
        numericInput('tsteps_to_run', 'Number of timesteps to advance', value = 20, min = 1, max = 50, step = 1),
        actionButton('aButtonRun', 'advance'),
        #br(),br(),
        actionButton('aButtonRestart', 'restart')
      ),
      
      helpText("Parameters that could be modified within the game to generate different scenarios."),      
      #growth parameters
      numericInput('rate_growth', 'population growth rate', value = 0.4, min = 0.1, max = 2, step = 0.1),
      numericInput('rate_insecticide_kill', 'insecticide kill rate', value = 0.4, min = 0.1, max = 2, step = 0.1),
      
      numericInput('resistance_modifier', 'resistance mortality modifier', value = 1, min = 0.1, max = 5, step = 0.1),
      helpText("Values above 1 for resistance modifier cause lower resistances to reduce mortality more"),
      
      numericInput('resist_incr', 'increase in resistance', value = 0.2, min = 0.01, max = 1, step = 0.1),
      numericInput('resist_decr', 'decrease in resistance', value = 0.1, min = 0.01, max = 1, step = 0.1),
      
      numericInput('cc_modifier', 'vector carrying capacity modifier', value = 1, min = 0.1, max = 1, step = 0.1)
      
    ), #end column
  column(8,
    
    #could put a tab panel in here for tabs
    #about : to show equations
    #lookup table : to show an alternative lookup table approach
    tabsetPanel(
      # to increase plot area vertically use height=         
      tabPanel("Simulation", plotOutput('plot1', height=700)),
      tabPanel("About", verbatimTextOutput("about")),
      tabPanel("Lookup Table", 
               helpText("An alternative lookup table that could be used to drive game behaviour. Here generated from the simulation equations."),
               helpText("The final columns 'changed_pop_vector' and 'changed_resist_pyr' are the new values for population and resistance under the values specified in the earlier columns."),
               helpText("A lookup table would be more cumbersome than using the equations directly in the game. This table is just for one set of the values that you can change in the boxes, extra columns could be added for those."),
               helpText("A lookup table would have the advantage that it could be generated from other existing models."),
               tableOutput("table") )
    )
         
    # to increase plot area vertically use height=         
    #plotOutput('plot1', height=700)
    
  ) #end column
  ) #end fluid row
))