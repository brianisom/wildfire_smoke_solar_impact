######################################
# Cleaning and Combining the Data
######################################

# Solar powerplant characteristics:
## Download EIA form 860 data from https://www.eia.gov/electricity/data/eia860/

# Monthly plant-level generation data
## Download EIA for 923 data from https://www.eia.gov/electricity/data/eia923/

# Install and load packages

library(dplyr)
library(data.table)

# Import 860 data from 2___Plant_Yyyyy and 3_3_Solar_Yyyyy (only operable plants)

plant2020 <- read.csv("WFSmokeAndSolarGen/data/raw/EIA860_2020_plant.csv")

solar2020 <- read.csv("WFSmokeAndSolarGen/data/raw/EIA860_2020_solar.csv")

# Combine form 860 data using Plant Id

data860 <- inner_join(x = solar2020, y = plant2020, by = "Plant.Code")

# Remove unwanted info by creating a subset of imported data

cdata860 <- subset(data860, select = -c(
    Ash.Impoundment., Ash.Impoundment.Lined.,
    Ash.Impoundment.Status, Natural.Gas.LDC.Name,
    Natural.Gas.Pipeline.Name.1, Natural.Gas.Pipeline.Name.2,
    Natural.Gas.Pipeline.Name.3, Pipeline.Notes,
    Natural.Gas.Storage, Liquefied.Natural.Gas.Storage
))

# Check if new joined dataset is matched correctly
## Match utility ids

all(cdata860$Utility.ID.x == cdata860$Utility.ID.y)

## Match counties

all(cdata860$County.x == cdata860$County.y)

# If both return TRUE, combine duplicate columns

fdata860 <- cdata860[!duplicated(as.list(cdata860))]

# Now import form 923 generation data from file:
# EIA923_Schedules_2_3_4_5_M_12_yyyy_Final_Revision (Page 1)

monthlygen <- read.csv("WFSmokeAndSolarGen/data/raw/EIA923_2020_monthlygen.csv", fileEncoding = "UTF-8-BOM")

# Filter form 923 data to include only solar PV generators

mpvgen <- filter(monthlygen, Reported.Prime.Mover == "PV")

# filter by NAICS code to remove all non-utility generators

umpvgen <- filter(mpvgen, NAICS.Code == "22")

# Combine form 923 data with form 860 data by matching plant code with plant id

pv2020data <- left_join(x = umpvgen, y = fdata860, by = c("Plant.Id" = "Plant.Code"))

# Remove duplicates created by multiple generator ids

spv2020data <- distinct(pv2020data, Plant.Id, .keep_all = TRUE)

# Remove unnecessary data columns

cpv2020data <- subset(spv2020data, select = c(
    Plant.Id, Plant.Name,
    Census.Region, NERC.Region.x,
    Respondent.Frequency, Netgen.January,
    Netgen.February, Netgen.March, Netgen.April,
    Netgen.May, Netgen.June, Netgen.July,
    Netgen.August, Netgen.September, Netgen.October,
    Netgen.November, Netgen.December,
    YEAR, State.x,
    County.x, Generator.ID, Status,
    Nameplate.Capacity..MW.,
    Operating.Year, Street.Address,
    City, Zip, Latitude, Longitude
))


# Set cpv2020data as a data table

setDT(cpv2020data)

# Convert data from wide to long form

Wide2020 <- melt(cpv2020data,
    measure.vars = c(
        "Netgen.January", "Netgen.February", "Netgen.March",
        "Netgen.April", "Netgen.May", "Netgen.June", "Netgen.July",
        "Netgen.August", "Netgen.September", "Netgen.October",
        "Netgen.November", "Netgen.December"
    ),
    variable.name = "Month",
    value.name = "Generation"
)