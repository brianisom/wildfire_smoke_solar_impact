# Import weather data
token <- "FjpmhEGKLgAlzijCYnEFqWipluaDIInv"

###############################################################################

# Weather data comes from the National Oceanic Atmospheric Administration's
# NClimDiv dataset: 
# https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc:C00005

# County-level data documentation:
# https://www.ncei.noaa.gov/data/nclimdiv-monthly/doc/county-readme.txt

###############################################################################

# Add weather data
# Max temp: https://www.ncei.noaa.gov/data/nclimdiv-monthly/access/climdiv-tmaxcy-v1.0.0-20230206
# Min temp:
# Precipitation:

###############################################################################

# Load in weather data ---------------------------------------------------------

max_temp_l <- fread("data/max_temp.csv")

min_temp_l <- fread("data/min_temp.csv")

precip_l <- fread("data/precip.csv")

avg_temp_l <- fread("data/avg_temp.csv")

# Convert weather data from wide to long and join together

max_temp  <- gather(max_temp_l, month, maxtemp, "1":"12", factor_key = TRUE)

min_temp  <- gather(min_temp_l, month, mintemp, "1":"12", factor_key = TRUE)

precip  <- gather(precip_l, month, precip, "1":"12", factor_key = TRUE)

avg_temp  <- gather(avg_temp_l, month, avgtemp, "1":"12", factor_key = TRUE)

weather_panel <- data.frame(avg_temp$geoid_cty,
                  avg_temp$year,
                  avg_temp$month,
                  max_temp$maxtemp,
                  min_temp$mintemp,
                  avg_temp$avgtemp,
                  precip$precip)

colnames(weather_panel)[1] = "geoidcty"
colnames(weather_panel)[2] = "year"
colnames(weather_panel)[3] = "month"
colnames(weather_panel)[4] = "maxtemp"
colnames(weather_panel)[5] = "mintemp"
colnames(weather_panel)[6] = "avgtemp"
colnames(weather_panel)[7] = "precip"

weather_panel <- weather_panel %>%
  subset(year >= "2003" & year <= "2020")

weather_panel <- weather_panel %>%
  mutate_at(c("geoidcty",
              "year",
              "month"), 
            as.character)

weather_panel$geoidcty <- str_pad(weather_panel$geoidcty, 5, pad = "0")

weather_panel$month <- str_pad(weather_panel$month, 2, pad = "0")

# Cosolidate the census tract panel into counties

census_panel$geoidcty <- substr(census_panel$geoid,1,5)

county_panel <- census_panel %>%
  group_by(geoidcty, period, year, month) %>%
  dplyr::summarize(total_gen = sum(total_gen),
                   capacity_mw = sum(capacity_mw),
                   smoke_days = sum(smoke_days),
                   total_pm = sum(total_pm),
                   avg_pm_smoke = mean(avg_pm_smoke))

# Attach weather data

county_panel_w <- county_panel %>%
  left_join(weather_panel,
            by = c("geoidcty" = "geoidcty",
                   "month" = "month",
                   "year" = "year")
            )

fwrite(county_panel, "data/panels/county_panel_w.csv")

county_panel_w_trim <- na.omit(county_panel_w, cols="maxtemp")

