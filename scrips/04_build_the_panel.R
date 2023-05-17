###############################################################################

# Wildfire-related PM2.5 measures used in this model come from a machine-
# generated database produced by Childs et. al. The data and accompanying paper
# are available here: https://www.stanfordecholab.com/wildfire_smoke

# They can also be accesed via the following github repository:
# https://github.com/echolab-stanford/daily-10km-smokePM

# Climate data comes from the PRISM database at Oregon State University:
# https://prism.oregonstate.edu/

###############################################################################

# Add climate data and wildfire PM2.5 measures

###############################################################################

# Load EIA plant gen and char dataset -----------------------------------------

panel <- fread("data/modified/monthly_solar_gen_full.csv")

# Attach climate data by matching plantID and period

climate_data <- fread("data/PRISM_climate_1.csv")

panel <- panel %>%
  left_join(climate_data,
            by = c("plantCode" = "Name",
                   "period" = "Date"))

# Add leading zeros to geographic ids for states with single-digit FIPS codes
panel <- panel %>%
  mutate_at("geoid", as.character) %>%
  filter(gross.generation > 0)

panel$geoid <- str_pad(panel$geoid, 11, pad = "0")

# Group panel by census tracts
census_panel <- panel %>%
  group_by(
    geoid, period) %>%
  dplyr::summarize(total_gen = sum(gross.generation),
                   capacity_mw = sum(gen_cap),
                   precip = mean(ppt),
                   tmin = mean(tmin),
                   tmax = mean(tmax),
                   tdmean = mean(tdmean)
  )

# Split geoid into individual fips codes
census_panel$state_fips <- substr(census_panel$geoid,1,2)
census_panel$county_fips <- substr(census_panel$geoid,3,5)
census_panel$tract_fips <- substr(census_panel$geoid,6,11)

# Split year and month
census_panel <- census_panel %>%
  separate(period, 
           sep = "-", 
           into = c("year", "month"),
           remove = FALSE
  )

# Prep PM2.5 data --------------------------------------------------------------

census_pm_pred <- fread(
  "data/smokePM2pt5_predictions_daily_tract_20060101-20201231.csv")

# Add leading zeros to geographic ids for states with single-digit FIPS codes
census_pm_pred <- census_pm_pred %>%
  mutate_at("GEOID", as.character)

census_pm_pred$GEOID <- str_pad(census_pm_pred$GEOID, 11, pad = "0")

# Generate a list of each unique geoid
pm_tract <- census_panel %>%
  distinct(geoid)

pm_tract_list <- pm_tract$geoid

# Match solar location data with PM2.5 data
census_pm <- census_pm_pred %>% 
  dplyr::filter(GEOID %in% pm_tract_list)

census_pm$year <- substr(census_pm$date,1,4)
census_pm$month <- substr(census_pm$date,5,6)
census_pm$day <- substr(census_pm$date,7,8)

census_pm <- census_pm %>%
  filter(year >= 2013)

census_pm_monthly <- census_pm %>%
  group_by(GEOID, year, month) %>%
  dplyr::summarize(smoke_days = n(),
                   total_pm = sum(smokePM_pred))

fwrite(census_pm_monthly, "data/modified/census_pm_monthly.csv")

# Add PM2.5 data to the panel --------------------------------------------------

pm_data <- census_pm_monthly

pm_data <- pm_data %>%
  mutate_at(c("year",
              "month",
              "GEOID"), 
            as.character)

census_panel <- census_panel %>%
  left_join(pm_data,
            by = c("geoid" = "GEOID",
                   "year" = "year",
                   "month" = "month"))

# Normalize generation by dividing total gen by generation capacity

census_panel$total_gen_norm <- census_panel$total_gen/census_panel$capacity_mw

census_panel <- census_panel %>%
  replace(is.na(.), 0) %>% 
  filter(total_gen > 0)
  
fwrite(census_panel, "data/panels/monthly_gen_census_panel.csv")