
######################################
# Cleaning and Combining the Data
######################################

#Solar powerplant characteristics:
## Download EIA form 860 data from https://www.eia.gov/electricity/data/eia860/

# Monthly plant-level generation data
## Download EIA for 923 data from https://www.eia.gov/electricity/data/eia923/

#Install and load packages

library(dplyr)
library(data.table)
library(tidyverse)
library(fs)

# Import 860 data from 2___Plant_Yyyyy and 3_3_Solar_Yyyyy (only operable plants)

plant2020 <- read.csv("data/raw/form_860/EIA860_2020_plant.csv")

####solar2020 <- read.csv("data/raw/form_860/EIA860_2020_solar.csv")

#Remove unnecessary columns from plant2020

plant2020 <- subset(plant2020, select = c(Plant.Code,
                                          Plant.Name,
                                          Street.Address,
                                          City,
                                          State,
                                          Zip,
                                          County,
                                          Latitude,
                                          Longitude
))

#Combine form 860 data using Plant Id

####plantchar2020 <- inner_join(x = solar2020, y = plant2020, by = "Plant.Code")

#Remove unwanted info by creating a subset of imported data

#Check if new joined dataset is matched correctly. If utility IDs and counties match,
#then duplicates are removed and a simpler dataset is created.

####if(all(plantchar2020$Utility.ID.x == plantchar2020$Utility.ID.y)
####   &(all(plantchar2020$County.x == plantchar2020$County.y))
####    ){simple_plantchar2020 <- plantchar2020[!duplicated(as.list(plantchar2020))]
####    }else print("MISMATCH")

#Remove unwanted columns

####plantchar2020 <- subset(plantchar2020, select = -c(Ash.Impoundment., Ash.Impoundment.Lined.,
####                                              Ash.Impoundment.Status, Natural.Gas.LDC.Name,
####                                              Natural.Gas.Pipeline.Name.1, Natural.Gas.Pipeline.Name.2,
####                                              Natural.Gas.Pipeline.Name.3, Pipeline.Notes,
####                                              Natural.Gas.Storage, Liquefied.Natural.Gas.Storage
####))

##Import form 923 generation data for years 2013-2020:

#Create a vector for each file name
#(NOTE: THE 2013 DATA HAS SLIGHTLY DIFFERENT COLUMN HEADINGS, I CHANGED THEM
#MANUALLY IN EXCEL TO MATCH 2014-2020)

form923_paths <- fs::dir_ls("data/raw/form_923")
form923_paths

# Write a loop that will read in all files

list_form923 <- list()

for (i in seq_along(form923_paths)) {
    list_form923[[i]] <- read.csv(
      file = form923_paths[[i]], fileEncoding = 'UTF-8-BOM'
    )
}

list_form923 <- set_names(list_form923, form923_paths)

#Eliminate all non-utility and non-solar generation from each

pvlist_form923 <- lapply(list_form923, function(x)
                         filter(x, NAICS.Code == "22",
                                Reported.Prime.Mover == "PV")
                         )

#Get rid of unnecessary columns of data

simplepvlist_form923 <- lapply(pvlist_form923, function(x)
                              subset(x, select = c(Plant.Id, 
                                                   Plant.Name, 
                                                   Plant.State, 
                                                   Census.Region, 
                                                   NERC.Region, 
                                                   Netgen.January, 
                                                   Netgen.February,	
                                                   Netgen.March, 
                                                   Netgen.April,	
                                                   Netgen.May, 
                                                   Netgen.June,	
                                                   Netgen.July, 
                                                   Netgen.August,	
                                                   Netgen.September,	
                                                   Netgen.October,	
                                                   Netgen.November,	
                                                   Netgen.December,	
                                                   YEAR)))

#Combine each annual form 923  data set to form one complete table 

monthlygen <- simplepvlist_form923 %>% reduce(full_join, by="Plant.Id")
write.csv(monthlygen, "temp/monthlygen.csv")

#Combine form 923 data with form 860 data by matching plant code with plant id

monthlygen2 <- left_join(x = monthlygen, y = plant2020, by = c("Plant.Id" = "Plant.Code"))
write.csv(monthlygen2, "temp/monthlygen2.csv")

#Attempt to convert from wide to long

setDT(monthlygen2)
longmonthlygen <- melt(monthlygen2, id.vars=c("County"))
write.csv(longmonthlygen, "temp/longmonthlygen.csv")

#Remove duplicates created by multiple generator ids

spv2020data <- distinct(pv2020data, Plant.Id, .keep_all = TRUE)

#Remove unnecessary data columns

cpv2020data <- subset(spv2020data, select = c(Plant.Id, Plant.Name,
                                             Census.Region, NERC.Region.x,
                                             Respondent.Frequency, Netgen.January,	
                                             Netgen.February,	Netgen.March,	Netgen.April,	
                                             Netgen.May,	Netgen.June,	Netgen.July,	
                                             Netgen.August,	Netgen.September,	Netgen.October,	
                                             Netgen.November,	Netgen.December,	
                                             YEAR,	State.x,
                                             County.x,	Generator.ID,	Status,
                                             Nameplate.Capacity..MW.,	
                                             Operating.Year,	Street.Address,	
                                             City,	Zip,	Latitude,	Longitude))


#Set cpv2020data as a data table

setDT(cpv2020data)

#Convert data from wide to long form

Wide2020 <- melt(cpv2020data,
                 measure.vars = c("Netgen.January",	"Netgen.February","Netgen.March",	
                                  "Netgen.April",	"Netgen.May",	"Netgen.June","Netgen.July",	
                                  "Netgen.August","Netgen.September","Netgen.October",	
                                  "Netgen.November",	"Netgen.December"),
                 variable.name = "Month",
                 value.name = "Generation")
