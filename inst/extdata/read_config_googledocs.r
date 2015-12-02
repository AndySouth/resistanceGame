#can I read the google docs config files directly from R ?

require(googlesheets)
suppressMessages(library(dplyr)) #for pipes etc.

#you must do File publish spreadsheet from google docs first
# url_resistances <- "https://docs.google.com/spreadsheets/d/1f9omxuMru95xBkmR0hLhrM88tvnxvJjsEXIiDjJOgL4"
# url_interventions <- "https://docs.google.com/spreadsheets/d/1SVyZZTR6tmDHRr0hLAyn3UYS6n0x9vuu1YEKJ4M8MIs/pub?output=tsv"

#note googlesheets potentially allows to to write to googlesheets which may be useful later

#create googlesheets object
# gs_r <- gs_url(url_resistances)
# gs_i <- gs_url(url_interventions)

#but these give erros
#df1 <- gs1 >%> gs_read() 
#Error: unexpected input in "df1 <- gs1 >%> gs_read()"

#df1 <- gs_read(gs1) 
# Accessing worksheet titled "Sheet1"
# Error in gsheets_GET(this_ws$exportcsv, to_xml = FALSE, use_auth = !ss$is_public) : 
#   Not expecting content-type to be:
#   text/html; charset=UTF-8

#aha! did work when I specified ranges
# df_interventions <- gs_read(gs_i, range = "A1:Z100")
# df_resistances <- gs_read(gs_r, range = "A1:Z100")


#trying 1 config file with multiple sheets

#you must do File publish spreadsheet from google docs first
url_all <- "https://docs.google.com/spreadsheets/d/1YTHErCMEpWvX-Tw6Jcj9xHJtNKY8QIk512zUYBRV94U"

gs_all <- gs_url(url_all) 

df_interventions <- gs_read(gs_all, range = "A1:Z100", ws="Interventions")
df_resistances   <- gs_read(gs_all, range = "A1:Z100", ws="Resistances")
df_locations        <- gs_read(gs_all, range = "A1:Z100", ws="Locations")
df_vectors       <- gs_read(gs_all, range = "A1:Z100", ws="Vectors")


#can I read one of Stuarts files that has dropdowns in later columns ?
# url_test <- "https://docs.google.com/spreadsheets/d/1okg0K53G4P6LlZ9XNWGvE_D-Cp0NZm-Qzhz6wFw3JqQ"
# gs_test <- gs_url(url_test)
# df_test <- gs_read(gs_test, range = "A1:H30")
#yes it does work
#ways of excluding dropdowns :
#1 by the range reading param
#2 by naming them something in row 1 that allows them to be identified e.g. options_
#3 no name in row1 & r gives them a name starting x, I could then exclude them




