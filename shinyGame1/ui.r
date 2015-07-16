#resistance/shiny/shinyGame1/ui.r
#andy south 18/6/15
#first go at a simple IRM 'game'

library(shiny)


shinyUI(pageWithSidebar(
  headerPanel("IRM 'Game model' prototype1"),
  sidebarPanel(
    checkboxInput('pyrOn', 'pyrethroid', TRUE),
    checkboxInput('ddtOn', 'ddt', FALSE),
    checkboxInput('opsOn', 'organophosphate', FALSE),
    checkboxInput('carOn', 'carbamate', FALSE),
    
    actionButton('aButtonRun', 'apply insecticide'),
    actionButton('aButtonRestart', 'restart')    
  ),
  mainPanel(
    plotOutput('plot1')
  )
))