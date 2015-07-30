


#a function to have a go at creating an example lookup table
#that could be used by a game to direct it's behaviour


create_lookup <- function(){
 
  #todo set this up as a list

  
#   name <- list('use_pyr','use_ddt','use_ops','use_car','pop_vector', 'resist_pyr')
#   values <- list( c(0,1), 
#                              c(0,1),
#                              c(0,1),
#                              c(0,1),
#                              seq(0,1,0.1),
#                              seq(0,1,0.1)
#                              )
  
  inputRanges <- list( use_pyr=c(0,1), 
                  use_ddt=c(0,1),
                  use_ops=c(0,1),
                  use_car=c(0,1),
                  pop_vector=seq(0,1,0.1),
                  resist_pyr=seq(0,1,0.1)
  )  

  inputs <- expand.grid(inputRanges)

  #adding on outputs columns
  #columnsOutput <- c('change_pop_vector','change_resist_pyr')
  
  #todo: get this to fill the output columns based on the logistic equations
  #maybe put the logistic equations from shinyGame1.r into package functions
  #but only bother doing that if we are going to do much more with this
  #remember that this is not the objective
  inputs$change_pop_vector <- 0
  inputs$change_resist_pyr <- 0
  
  write.csv(inputs, file='demoLookupTable.csv')
  
   
}