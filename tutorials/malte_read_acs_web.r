library(data.table)

# Daten finden:
# 1. Go to website http://factfinder.census.gov/
# 2. -> Guided Search
# 3. -> people
# 4. -> Income/Earnings (Households)
# 5. -> 5-Digit ZIP Code Tabulation Area
# 6. -> skip race & ethnicity
# 7. -> select Table ID 1902 "MEAN INCOME IN THE PAST 12 MONTHS (IN 2014 INFLATION-ADJUSTED DOLLARS)"
# 8. -> Download data (with options: Comma delimited, Data and annotations in a single file, include descriptive data element names)

# Daten einlesen
data <- fread("ACS_14_5YR_S1902_with_ann.csv")

# rausfinden wie die Tabelle aufgebaut ist
str(data)
data[GEO.id2 == "Id2" | GEO.id2 == "60007"] # zeige Überschriften und Daten für zip code 60007

# nur folgende Informationen anzeigen:
# - zip code
# - anzahl haushalte (+ margin of error)
# - mittleres Haushaltseinkommen (+margin of error)
mean_income_by_zip <- data[-1, list(zip = GEO.id2, num_households = HC01_EST_VC02, num_households_me = HC01_MOE_VC02,
                             mean_income = HC02_EST_VC02, mean_income_me = HC02_MOE_VC02)]

str(mean_income_by_zip) # all numbers are currently saved as characters -> change this!
mean_income_by_zip[, num_households := as.numeric(num_households)]
mean_income_by_zip[, num_households_me := as.numeric(num_households_me)]
mean_income_by_zip[, c("mean_income", "mean_income_me") := list(as.numeric(mean_income), as.numeric(mean_income_me))] # works also in a single statement

####################################
#
# insert code to merge data from the ACS with DataFest data
# subsequent will not run without this code
#
####################################
# get central latitude and longitude coordinate for our zip codes
library("zipcode")
data(zipcode)
zipcode <- data.table(zipcode, key = "zip")
zip_data[, zip := clean.zipcodes(zip)] # clean zip
zip_data <- zipcode[zip_data] # merge our data with latitudes and longitudes from zipcode data

###################################
# prepare a map
library("ggmap")

# download map, zoom level 4 works well for the United States
map<-get_map(location='united states', zoom=4, maptype = "terrain",
             source='google',color='color')

# Where are the big cities (number of households)
ggmap(map) + geom_point(
  aes(x=longitude, y=latitude, show_guide = TRUE, size = 0.1* log(num_households), colour=log(num_households)), 
  data=zip_data, alpha=.2, na.rm = T)  + 
  scale_color_gradient(low="beige", high="blue")

# And where is big money at home? (mean income)
ggmap(map) + geom_point(
  aes(x=longitude, y=latitude, show_guide = TRUE, size = log(mean_income), colour=mean_income), 
  data=zip_data, alpha=.2, na.rm = T)  + 
  scale_color_gradient(low="beige", high="blue")

# plot dataFest data
ggmap(map) + geom_point(
  aes(x=longitude, y=latitude, show_guide = TRUE, size = 0.1* log(datafest_data), colour=log(datafest_data)), 
  data=zip_data, alpha=.2, na.rm = T)  + 
  scale_color_gradient(low="beige", high="blue")

# We have a weak positive correlation between the average income and the datafest data
# cor(zip_data$mean_income, Data Fest Data)
# It looks like the poor have less access to cultural events...
# ... but one can certainly improve this analysis.