#shinyGame1/ui.r
#andy south 18/6/15
#first go at a simple IRM 'game'

library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("flatly"),

  headerPanel("IRM 'Game model' prototype1"),                  
                  
  fluidRow(
    
    column(3,
           #wellPanel(                  
      
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
      numericInput('resistanceModifier', 'resistance modifier', value = 1, min = 0.1, max = 5, step = 0.1)
    ), #end column
  column(9,
    plotOutput('plot1')
  ) #end column
  ) #end fluid row
))