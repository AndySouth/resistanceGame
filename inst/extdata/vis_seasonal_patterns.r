#vis_seasonal_patterns.r

#maybe change this into a function that can vis seasonal patterns from config files later

filename <- "C:\\Dropbox\\Serious Gaming Initiative - Resistance Game (1)\\game model\\seasonal patterns readable.csv" 

seasonal <- read.csv(filename, stringsAsFactors=FALSE)


#need to get data in long format first
library(tidyr)

#this excludes species column
#calls the key column habitat
#calls values column emergence
s2 <- tidyr::gather(seasonal, habitat, emergence, -species)
#remove blanks
s2 <- s2[ s2$emergence!="",]

#aha now I still need to get out each seasonal pattern into a by month
#tried this & couldn't get it to work
#lapply(s2, function(x) expand_season(x['emergence'], return_tstep='months'))
#instead try this loop hack
#gets emergence in each month in as a column
months <- NULL
for (row in 1:nrow(s2))
{
  #cat(s2[['emergence']][row],"\n")
  tmp <- expand_season(s2[['emergence']][row], return_tstep='months')
  
  months <- rbind(months,tmp)
}
s3 <- cbind(s2,months)

#remove the emergence string column
s3 <- s3[ , names(s3)!='emergence' ]
#gather the month columns into long format
s4 <- tidyr::gather(s3, month, emergence, 1:12)

library(ggplot2)

# 1 plot for each, titles get very messy
#  ggplot(s4, aes(month, emergence)) +
#    geom_point() +
#    facet_wrap(~habitat,species) 

#this nearly does what I want just slight problem seeing points that are on top of each other
#shape=1 for unfilled circles
ggplot(s4, aes(month, emergence)) +
      #geom_point(aes(colour = species)) +
      #geom_jitter(aes(colour = species)) +
      geom_jitter(aes(colour=species), position = position_jitter(height=0, width = .3)) +
      #geom_jitter(aes(colour=species), shape=1, position = position_jitter(height=0, width = .3)) +
      facet_wrap(~habitat) +
      theme_classic()



#failed line attempt
ggplot(s4, aes(month, emergence)) +
  #geom_point(aes(colour = species)) +
  #geom_jitter(aes(colour = species)) +
  geom_line() +
  #geom_jitter(aes(colour=species), shape=1, position = position_jitter(height=0, width = .3)) +
  facet_wrap(~habitat)

#position_nudge seems not to be in cran version
ggplot(s4, aes(month, emergence)) +
  geom_point(aes(colour = species), position = position_nudge(x=-0.2)) +
  facet_wrap(~habitat)

#dodge fails too
ggplot(s4, aes(month, emergence)) +
  geom_point(aes(colour = species), position = 'dodge') +
  facet_wrap(~habitat)

#may need to think of a different way of doing 

#plus can I use the readable csv to generate places.csv
# places <- data.frame(cbind(0,0,0,0,0))
# place_fields <- c('place_id','place_name','place_desc','vector_id','emergence')			
# names(places) <- place_fields
#or maybe using tidyr would be easier
# I can just rename columns in s2 from above & add th blank columns (or don't even bother with them yet)
#> names(s2)
#[1] "species"   "habitat"   "emergence"
names(s2) <- c('vector_id','place_id','emergence')
#then just write csv
outfile <- "C:\\Dropbox\\Serious Gaming Initiative - Resistance Game (1)\\game model\\config_seasonal1\\places.csv" 
write.csv(s2, file=outfile, row.names=FALSE)

#creating vectors.csv
#easier to copy/paste by hand for now
#but could do something like this to start creating it from seasonal readable.csv
#vectors <- seasonal[1] 
