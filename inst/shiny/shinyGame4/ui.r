#shinyGame4/ui.r
#andy south 19/10/15
#based on emergence instead of carrying capacity

library(shiny)
library(shinythemes)


shinyUI(fluidPage(theme = shinytheme("flatly"),

  #headerPanel("Insecticide Resistance Management 'Game model' prototype4"),  
  h4("Insecticide Resistance Management 'Game model' prototype4"), 
  
  #29/9/2015    
  #navbarPage sets up navbar, title appears on left
#   navbarPage("IRM game model v3", id="selectedTab",
# 
#      # tab "Scenarios" ---------------------------
#      tabPanel("Scenarios", includeHTML("IRM-prototype-game-scenarios2.html")),
#      
#      # tab "User Interface" ---------------------------
#      tabPanel("User Interface", value="seek",
  
  
  helpText("A vector simulation for an Insecticide Resistance Management game. Press buttons on left to advance, plots will appear in the 'Simulation' tab. Feedback to southandy@gmail.com."),

  #29/9/2015 simply include a link to pdf on dropbox, target="_blank" to open in new tab
  helpText(a("View a document of scenarios", 
             href="https://www.dropbox.com/s/wudl9ic86oml7t1/IRM-prototype-game-scenarios3.pdf?dl=0",
             target="_blank")),
  
  fluidRow(
    
    column(4,
                 
      helpText("Choose one or more controls to use in each time step. ",
               "Controls and resistances can be modified in configuration files."),    
      
      #controls checkboxes got from config file
      uiOutput("checkboxGroupControls"),
      
      
      wellPanel(
        numericInput('tsteps_to_run', 'Num. timesteps to advance', value = 10, min = 1, max = 50, step = 1),
        actionButton('aButtonRun', 'advance'),
        #br(),br(),
        actionButton('aButtonRestart', 'restart')
      ),
      
      #helpText("Parameters that could be modified within the game to generate different scenarios."),      
      #growth parameters
      numericInput('emergence', 'emergence of adults from larvae', value = 0.3, min = 0, max = 1, step = 0.1),
      numericInput('survival', 'adult survival', value = 0.7, min = 0.1, max = 1, step = 0.1),
      numericInput('rate_insecticide_kill', 'insecticide kill rate', value = 0.4, min = 0.1, max = 2, step = 0.1),
      
      numericInput('pop_start', 'start population (vectors)', value = 0.3, min = 0.01, max = 1, step = 0.05),
      numericInput('resist_start', 'starting resistance', value = 0.05, min = 0.01, max = 1, step = 0.05),
      
      numericInput('resist_incr', 'increase in resistance', value = 0.2, min = 0.01, max = 1, step = 0.05),
      numericInput('resist_decr', 'decrease in resistance', value = 0.1, min = 0.01, max = 1, step = 0.05),

      numericInput('resistance_modifier', 'resistance mortality modifier', value = 1, min = 0.1, max = 5, step = 0.1),
      helpText("Values above 1 for resistance modifier cause lower resistances to reduce mortality more")
      
    ), #end column
  column(8,
    
    #could put a tab panel in here for tabs
    #about : to show equations
    #lookup table : to show an alternative lookup table approach
    tabsetPanel(
      # to increase plot area vertically use height=         
      tabPanel("Simulation", plotOutput('plot1', height=700)),
      #just include the HTML which I copied into the shiny folder
      #unfortunately it restricts width of plotting area on other tabs
      #tabPanel("Scenarios",includeHTML("IRM-prototype-game-scenarios2.html")),
      #tabPanel("Config Files", verbatimTextOutput("show_config_files")),
      tabPanel("Config Files",
               helpText("The relationships between vectors, controls and resistance mechanisms are specified in simple 
                         configuration files. Below is a simple example of a collection of such configuration files. 
                         (In this version of the UI the configuration files do not yet determine rates of growth and 
                         control thus allowing you to modify them in the boxes on the left)"),
               
               helpText("places.csv"),
               tableOutput("table_places"),
               helpText("vectors.csv"),
               tableOutput("table_vectors"),
               helpText("controls.csv"),
               tableOutput("table_controls"),
               helpText("resistances.csv"),               
               tableOutput("table_resistances")               
               ),
      tabPanel("About", verbatimTextOutput("about"))
    ) #end tabsetPanel
         
    # to increase plot area vertically use height=         
    #plotOutput('plot1', height=700)
    
  ) #end column
  ) #end fluid row

#29/9/2015
# ) # end tabPanel("User Interface") 
# ) # end navbarPage  

) # end fluidPage
) # end shinyUI