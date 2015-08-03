#shinyGame1/ui.r
#andy south 18/6/15
#2nd go at a simple IRM 'game'

library(shiny)
library(shinythemes)


shinyUI(fluidPage(theme = shinytheme("flatly"),

  headerPanel("Insecticide Resistance Management 'Game model' prototype1"),  
  
  helpText("A potential vector simulation for an Insecticide Resistance Mangement game. Press buttons on the left to advance the simulation, plots will appear in the 'Simulation' tab. Feedback to southandy@gmail.com."),
  
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
        #br(),br(),
        actionButton('aButtonRestart', 'restart')
      ),
      
      helpText("Parameters that could be modified within the game to generate different scenarios."),      
      #growth parameters
      numericInput('rateGrowth', 'population growth rate', value = 0.4, min = 0.1, max = 2, step = 0.1),
      numericInput('rateInsecticideKill', 'insecticide kill rate', value = 0.4, min = 0.1, max = 2, step = 0.1),
      
      numericInput('resistanceModifier', 'resistance mortality modifier', value = 1, min = 0.1, max = 5, step = 0.1),
      helpText("Values above 1 for resistance modifier cause lower resistances to reduce mortality more"),
      
      numericInput('rateResistance', 'resistance change rate', value = 0.2, min = 0.01, max = 1, step = 0.1),
      
      numericInput('ccModifier', 'vector carrying capacity modifier', value = 1, min = 0.1, max = 1, step = 0.1)
      
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