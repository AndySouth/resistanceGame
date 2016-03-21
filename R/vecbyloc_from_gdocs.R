#' create a dataframe of vector behaviour by location from the gdoc config files stored in data
#'
#' returned dataframe has 1 row per vector,location combination
#'
# @param locations dataframe from locations config file
#' 
#' @return dataframe
#' @export
#' 
vecbyloc_from_gdocs <- function()
{

  #uses locations & vectors dataframes which are read from googledocs config files & stored in package
  
  emers <- data.frame(stringsAsFactors=FALSE)
  emer_strings <- NULL  
  
  #for each location
  for (i in 1:nrow(locations))
  {
    tmp <- locations$emergences[i]
    
    #remove comma from middle and split in two
    tmp2 <- unlist(strsplit(tmp, split = ","))
    #remove brackets
    tmp3 <- gsub('\\(','',tmp2)
    tmp4 <- gsub('\\)','',tmp3)
    
    #this might not work if just one vector
    vec_ids <- unlist(strsplit(locations$`vec_ids`[i], split = ","))
    
    #for each vector at this location
    for (j in 1:length(tmp4))
    {
      #expanding the string
      tmp5 <- expand_season(tmp4[j], return_tstep = 'months')
      
      #cat(tmp5,"\n")
      #add string itself onto vector, causes factor problems
      #tmp5 <- c(tmp4[j], tmp5)
      emer_strings <- c(emer_strings, tmp4[j]) 
      
      emers <- rbind(emers, tmp5) #, stringsAsFactors=FALSE)

      #add string itself onto vector
      #emers$emer_str <- tmp4[j]
            
      rownames(emers)[nrow(emers)] <- paste0(locations$loc_id[i],"_",vec_ids[j])
    }    
    
  }
  colnames(emers) <- paste0("m",c(1:ncol(emers)))
  emers$id <- rownames(emers)
  #add the vectorID on its own
  emers$vec_id <- substr(emers$id, nchar(emers$id)-1, nchar(emers$id))
  
  #now can I add the survival for the vec
  #vectors is read from gdocs & sored in package
  emers$survival <- vectors$survival[ match( emers$vec_id, vectors$vec_id) ]
  
  emers$emer_string <- emer_strings
  
  return(emers)

}