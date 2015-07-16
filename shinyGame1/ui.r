#shinyGame1/ui.r
#andy south 18/6/15
#first go at a simple IRM 'game'

library(shiny)


shinyUI(pageWithSidebar(
  headerPanel("IRM 'Game model' prototype1"),
  sidebarPanel(
    
    # may want to set these so that you can just choose 
    # one at a time (i.e. no mixtures)
    checkboxInput('pyrOn', 'pyrethroid', TRUE),
    checkboxInput('ddtOn', 'ddt', FALSE),
    checkboxInput('opsOn', 'organophosphate', FALSE),
    checkboxInput('carOn', 'carbamate', FALSE),
    
    actionButton('aButtonRun', 'advance 1 timestep'),
    actionButton('aButtonRestart', 'restart'),
    
    #growth parameters
    numericInput('rateGrowth', 'population growth rate', value = 0.4, min = 0.1, max = 2, step = 0.1),
    numericInput('rateInsecticideKill', 'insecticide kill rate', value = 1.2, min = 0.1, max = 2, step = 0.1),
    numericInput('resistanceModifier', 'resistance modifier', value = 1.5, min = 0.1, max = 2, step = 0.1)
  ),
  mainPanel(
    plotOutput('plot1')
  )
))