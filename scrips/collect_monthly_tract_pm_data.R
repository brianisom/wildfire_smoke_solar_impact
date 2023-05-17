# Create pm data.tabe for census tracts
census_pm_pred <- fread(
  "data/smokePM2pt5_predictions_daily_tract_20060101-20201231.csv"
)

pm_tract <- census_panel %>%
  distinct(geoid)

pm_tract_list <- pm_tract$geoid

census_pm_pred <- census_pm_pred %>% 
  dplyr::filter(GEOID %in% pm_tract_list)

census_pm_pred$year <- substr(census_pm_pred$date,1,4)
census_pm_pred$month <- substr(census_pm_pred$date,5,6)
census_pm_pred$day <- substr(census_pm_pred$date,7,8)

census_pm_pred <- census_pm_pred %>%
  filter(year >= 2015)

census_pm_pred_monthly <- census_pm_pred %>%
  group_by(GEOID, year, month) %>%
  dplyr::summarize(smoke_days = n(),
                   total_pm = sum(smokePM_pred),
                   avg_pm_smoke = mean(smokePM_pred))

fwrite(census_pm_pred_monthly, "data/modified/census_pm_pred_monthly.csv")