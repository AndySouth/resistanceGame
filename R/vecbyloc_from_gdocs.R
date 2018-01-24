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
  loc_vec_res_ids <- NULL
  emers$vec_res_ids <- NULL
  
  #for each location
  for (loc_num in 1:nrow(locations))
  {
    tmp <- locations$emergences[loc_num]
    
    #remove comma from middle and split in two
    tmp2 <- unlist(strsplit(tmp, split = ","))
    #remove brackets
    tmp3 <- gsub('\\(','',tmp2)
    tmp4 <- gsub('\\)','',tmp3)
    
    #this might not work if just one vector
    vec_ids <- unlist(strsplit(locations$`vec_ids`[loc_num], split = ","))
    
    #sort emergences for each vector at this location
    for (vec_num in 1:length(vec_ids))
    {
      #expanding the string
      tmp5 <- expand_season(tmp4[vec_num], return_tstep = 'months')
      
      #cat(tmp5,"\n")
      #add string itself onto vector, causes factor problems
      emer_strings <- c(emer_strings, tmp4[vec_num]) 
      
      emers <- rbind(emers, tmp5) #, stringsAsFactors=FALSE)

      #add string itself
      emers$emer_string <- emer_strings
            
      rownames(emers)[nrow(emers)] <- paste0(locations$loc_id[loc_num],"_",vec_ids[vec_num])
    }  
    
    
    #*****sort how to get resistanceIDs and freqs onto this file
    #*****without making it too ugly !!
    
    #sort resistance IDs & frequencies for each vec at this loc
    #e.g. (0.7);(0.3,0.7), but beware there can be multiple resistances per vector
    #so may not be possible to get this to work perfectly ...
    for (vec_num in 1:length(vec_ids))
    {
      #this might not work if just one vector
      vec_res_ids <- unlist(strsplit(locations$`vec_res_ids`[loc_num], split = ";"))
      #remove brackets
      vec_res_ids <- gsub('\\(','',vec_res_ids)
      vec_res_ids <- gsub('\\)','',vec_res_ids)
      
      #loc_vec_res_ids <- c(loc_vec_res_ids,vec_res_ids)
      emers$vec_res_ids[] <- loc_vec_res_ids
    }
    emers$vec_res_ids <- loc_vec_res_ids    
    
    
  }
  colnames(emers) <- paste0("m",c(1:ncol(emers)))
  emers$id <- rownames(emers)
  #add the vectorID on its own
  emers$vec_id <- substr(emers$id, nchar(emers$id)-1, nchar(emers$id))
  
  #now can I add the survival for the vec
  #vectors is read from gdocs & sored in package
  emers$survival <- vectors$survival[ match( emers$vec_id, vectors$vec_id) ]
  

  
  #add to file the 
  
  
  return(emers)

}