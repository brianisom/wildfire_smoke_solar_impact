###############################################################################

# Air Quality Data 

#   The next few sections of code pull data from the Environmental Protection 
# Agency's Air Quality System API: <https://www.epa.gov/aqs>. 
# Wildfires produce a high volume of particulate matter 2.5 (pm2.5), which 
# will be used as a proxy for wildfire activity in a given area.

# EPA AQS Dictionary: 
# <https://aqs.epa.gov/aqsweb/documents/AQS_Data_Dictionary.html#>

# EPA AQS Code List: <https://www.epa.gov/aqs/aqs-code-list>
 
###############################################################################

# Define a function to call the daily PM2.5 data using EPA's API

pm25.daily.api.call <-
  function(date,
           state,
           epa_key = "blueswift59",
           parameter = "88101",
           ...) {
    url_daily_epa <-
      paste(
        "https://aqs.epa.gov/data/api/dailyData/byState?email=brian@thecgo.org",
        "&key=",
        epa_key,
        "&param=",
        parameter,
        date,
        "&state=",
        state,
        sep = ""
      )
    
    res_daily_epa <- GET(url_daily_epa)
    
    pm_daily_epa <- fparse(res_daily_epa$content)
    
    pm_daily_epa_final <- pm_daily_epa$Data
    
    return(pm_daily_epa_final)
  }

# Define the date ranges to collect

years <-
  c(
    "&bdate=20130101&edate=20131231",
    "&bdate=20140101&edate=20141231",
    "&bdate=20150101&edate=20151231",
    "&bdate=20160101&edate=20161231",
    "&bdate=20170101&edate=20171231",
    "&bdate=20180101&edate=20181231",
    "&bdate=20190101&edate=20191231",
    "&bdate=20200101&edate=20201231"
  )

# Designate each state using fips codes. Takes around an hour to run.

pmAZ <- map_dfr(years, ~ pm25.daily.api.call(.x, "04"))
write.csv(pmAZ, "data/raw/AZ_daily_pm25_raw.csv")

pmCA <- map_dfr(years, ~ pm25.daily.api.call(.x, "06"))
write.csv(pmCA, "data/raw/CA_daily_pm25_raw.csv")
 
pmCO <- map_dfr(years, ~ pm25.daily.api.call(.x, "08"))
write.csv(pmCO, "data/raw/CO_daily_pm25_raw.csv")

pmID <- map_dfr(years, ~ pm25.daily.api.call(.x, "16"))
write.csv(pmID, "data/raw/ID_daily_pm25_raw.csv")

pmMT <- map_dfr(years, ~ pm25.daily.api.call(.x, "30"))
write.csv(pmMT, "data/raw/MT_daily_pm25_raw.csv")

pmNV <- map_dfr(years, ~ pm25.daily.api.call(.x, "32"))
write.csv(pmNV, "data/raw/NV_daily_pm25_raw.csv")

pmNM <- map_dfr(years, ~ pm25.daily.api.call(.x, "35"))
write.csv(pmNM, "data/raw/NM_daily_pm25_raw.csv")

pmOR <- map_dfr(years, ~ pm25.daily.api.call(.x, "41"))
write.csv(pmOR, "data/raw/OR_daily_pm25_raw.csv")

pmTX <- map_dfr(years, ~ pm25.daily.api.call(.x, "48"))
write.csv(pmTX, "data/raw/TX_daily_pm25_raw.csv")

pmUT <- map_dfr(years, ~ pm25.daily.api.call(.x, "49"))
write.csv(pmUT, "data/raw/UT_daily_pm25_raw.csv")

pmWA <- map_dfr(years, ~ pm25.daily.api.call(.x, "53"))
write.csv(pmWA, "data/raw/WA_daily_pm25_raw.csv")

pmWY <- map_dfr(years, ~ pm25.daily.api.call(.x, "56"))
write.csv(pmWY, "data/raw/WY_daily_pm25_raw.csv")
