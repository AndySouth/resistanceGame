#' plot parts of config files to aid checking
#'
#' @param locations dataframe from locations config file
#' 
#' @return maybe nothing, produces a plot
#' @export
#' 
plot_config_gdocs <- function(locations)
{

  library(ggplot2)
  
  emers <- data.frame()
  
  #not sure this bit below quite copes with emergences being a vector
  for (i in 1:nrow(locations))
  {
    tmp <- locations$emergences[i]
    
    #remove comma from middle and split in two
    tmp2 <- unlist(strsplit(tmp, split = ","))
    #remove brackets
    tmp3 <- gsub('\\(','',tmp2)
    tmp4 <- gsub('\\)','',tmp3)
    
    #this might not work if just one vector
    vec_ids <- unlist(strsplit(locations$`Vector IDs`[i], split = ","))
    
    #for each vector at this location
    for (j in 1:length(tmp4))
    {
      #expanding the string
      tmp5 <- expand_season(tmp4[j], return_tstep = 'months')
      
      #cat(tmp5,"\n")

      emers <- rbind(emers,tmp5)
      rownames(emers)[nrow(emers)] <- paste0(locations$`Location ID`[i],"_",vec_ids[j])
    }    
    
  }
  colnames(emers) <- paste0("m",c(1:ncol(emers)))
  emers$id <- rownames(emers)
  
  #emers
  library(reshape2)
  emers2 <- melt(emers,variable.name='month')
  print(
    ggplot(emers2, aes(x=month, y=value, colour=id)) +
             #scale_x_discrete("month")
             facet_wrap('id') +
             theme(legend.position="none") +
             geom_point() 
  )#end print
  
}
