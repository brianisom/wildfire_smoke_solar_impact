###############################################################################

# Cleaning and Combining the Data

# This file combines the individual data from each generation/characteristic
# to make one single .csv that includes generation, characteristic, and census
# data for each plant in the model. It also fixes some irregularities that
# are present in the EIA data.

###############################################################################

# Load generation data ---------------------------------------------------------

raw_solar_gen <- fread("data/API/plant_level_monthly_solar_gen.csv")

raw_solar_gen_az <- fread("data/API/az_plant_level_monthly_solar_gen.csv")

raw_solar_gen_ca <- fread("data/API/ca_plant_level_monthly_solar_gen.csv")

monthly_solar_gen <- rbind(raw_solar_gen,
                           raw_solar_gen_az,
                           raw_solar_gen_ca) # Combines all state gen data

# Fix inconsistencies in the data
monthly_solar_gen <- monthly_solar_gen %>%
  mutate(plantCode = as.character(
    plantCode
  )) %>%
  mutate(state = ifelse(
    state == "NV" & plantCode == "57576", "NM", state
  )) %>%
  mutate(state = ifelse(
    state == "NV" & plantCode == "58646", "CA", state
  )) %>%
  mutate(state = ifelse(
    state == "AZ" & plantCode == "59826", "NV", state
  ))

# Load characteristic data -----------------------------------------------------

raw_plant_char <- fread("data/API/plant_char.csv")

raw_plant_char_az <- fread("data/API/az_plant_char.csv")

raw_plant_char_ca <- fread("data/API/ca_plant_char.csv")

raw_plant_char_co <- fread("data/API/co_plant_char.csv")

characteristic_full <- rbind(raw_plant_char,
                             raw_plant_char_az,
                             raw_plant_char_ca,
                             raw_plant_char_co) # Combines state char data

# Normalize county capitalization
characteristic_data <- characteristic_full %>%
  mutate(county = tolower(county))

# Fix inconsistencies in the data
characteristic_data <- characteristic_data %>% 
  mutate(plantid = as.character(
    plantid)) %>% 
  mutate(stateid = ifelse(
    stateid == "NV" & plantid == "57576", "NM", stateid)) %>% 
  mutate(county = ifelse(
    county == "Adams" & plantid == "57645", "Denver", county)) %>% 
  mutate(stateid = ifelse(
    stateid == "NV" & plantid == "58646", "CA", stateid)) %>% 
  mutate(county = ifelse(
    county == "Adams" & plantid == "59462", "Denver", county)) %>%
  mutate(latitude = ifelse(
    plantName == "Panoche Valley Solar Farm", 36.627, latitude)) %>%
  mutate(longitude = ifelse(
    plantName == "Panoche Valley Solar Farm", -120.879, longitude)) %>%
  mutate(latitude = ifelse(
    plantName == "Rio Grande Solar", 35.00745, latitude)) %>%
  mutate(longitude = ifelse(
    plantName == "Rio Grande Solar", -118.16451, longitude)) %>%
  filter(plantName != c("Paradise Valley H.S. PV",
                        "Phoenix Airport East Economy Lot",
                        "Phoenix Airport Rental Car Center"))

# Eliminate redundency, only need one row per plant id
characteristic_data <- characteristic_data %>%
 distinct(plantid, generatorid, .keep_all = TRUE)

# Some plants have multiple generators, this sums the nameplate capacity of
# each generator to give the total capacity per plant
gen_capacity_list <- characteristic_data %>%
  group_by(plantid) %>%
  dplyr::summarize(gen_cap = sum(nameplate.capacity.mw))

# Eliminates redundancy in plants and adds total gen capacity
characteristic_data <- characteristic_data %>%
  distinct(plantid, .keep_all = TRUE) %>% 
  left_join(gen_capacity_list, by = "plantid")

# Remove unnecessary columns from the data
characteristic_data <- subset(
  characteristic_data,
  select = c(
    "sector",
    "entityid",
    "entityName",
    "plantid",
    "balancing_authority_code",
    "balancing.authority.name",
    "status",
    "latitude",
    "longitude",
    "gen_cap",
    "operating.year.month"
  )
)

fwrite(characteristic_data, "data/modified/plant_char_latlon.csv")

# Add census level data using lat/lon-------------------------------------------
# Use a for loop to construct the census tracts df
state_fips <- c(
  "04",
  "06",
  "08",
  "16",
  "30",
  "32",
  "35",
  "41",
  "48",
  "49",
  "53",
  "56"
)

# This loop builds a df of census level information for each state in the model
j = 0
for (i in 1:12) {
  
  census_tracts_iter <- st_read(
    paste0(
      "data/census_tracts/cb_2018_",
      state_fips[i],
      "_tract_500k/cb_2018_",
      state_fips[i],
      "_tract_500k.shp"),
    quiet = TRUE
  )
  
  if (j == 0) {
    census_tracts <- census_tracts_iter
  }
  else {
    census_tracts <- rbind(census_tracts,
                           census_tracts_iter)
  }
  j = j + 1
}

# Convert latitude and longitude to point geometry
solar_char_sf <- characteristic_data %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(census_tracts))

intersected <- st_intersects(solar_char_sf,census_tracts)

# Add census tract for each plant location
characteristic_data <- solar_char_sf %>%
  mutate(intersection = as.integer(intersected),
         geoid = if_else(is.na(intersection), "",
                        census_tracts$GEOID[intersection]))

fwrite(characteristic_data, "data/modified/solar_char_final.csv")

# Match plant characteristics to generation data ------------------------------

# Add characteristics to generation numbers by matching IDs
monthly_solar_gen <- monthly_solar_gen %>%
  left_join(characteristic_data,
    by = c("plantCode" = "plantid")
  )

fwrite(monthly_solar_gen, "data/modified/monthly_solar_gen_full.csv")

# Create a .csv of individual plant characteristics for mapping in tableau
solar_lat_lon <-
  monthly_solar_gen %>%
  distinct(plantCode, .keep_all = TRUE)

separated_coord <- solar_lat_lon %>%
  mutate(long = unlist(map(solar_lat_lon$geometry,1)),
         lat = unlist(map(solar_lat_lon$geometry,2)))

fwrite(separated_coord, "data/modified/tableau_solar_lat_lon.csv")
