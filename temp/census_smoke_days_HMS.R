
plant_info <- fread("data/modified/plant_char_latlon.csv")

ba_daily_gen <- fread("data/API/ba_daily_gen.csv")

# Using sf's st_centroid
plant_sf <- st_as_sf(plant_info, coords = c('latitude', 'longitude'))

plant_centroid <-
  plant_sf %>%
  group_by(balancing.authority.name) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  st_centroid

centroid_coord <- plant_centroid %>%
  st_coordinates(geometry)

centroid_coord_df <- as.data.frame(centroid_coord)

plant_centroid$latitude <- centroid_coord_df$X
plant_centroid$longitude <- centroid_coord_df$Y

BA_centroid <- plant_centroid[,c(1,3,4,2)]

write.csv(BA_centroid, "~/temp/BA_centroid.csv")

####################################################
# Weighted Centroid
####################################################

plant_info$weight <- ceiling(plant_info$nameplate.capacity.mw)

n.times <- as.vector(plant_info$weight)

plant_info_w <- plant_info %>% slice(rep(1:n(), n.times))

plant_sf_w <- st_as_sf(plant_info_w, 
                       coords = c("longitude","latitude"),
                       crs = 4326
)

plant_centroid_w <-
  plant_sf_w %>%
  dplyr::group_by(balancing_authority_code) %>% 
  dplyr::summarize(geometry = st_union(geometry)) %>% 
  st_centroid()

centroid_coord_w <- plant_centroid_w %>%
  st_coordinates(geometry)

centroid_coord_w_df <- as.data.frame(centroid_coord_w)

plant_centroid_w$longitude <- centroid_coord_w_df$X
plant_centroid_w$latitude <- centroid_coord_w_df$Y

BA_centroid_w <- plant_centroid_w[,c(1,3,4,2)]

fwrite(BA_centroid_w, "temp/BA_centroid_w.csv")

#########################################################
# functions
#########################################################

## download and unzip gzip files

HMSunzip <- function(zFiles) {
  directory <- tempdir()
  setwd(directory)
  
  temp <- tempfile()
  temp2 <- tempfile()
  
  # Download the zip file and save to 'temp' 
  URL <- zFiles
  download.file(URL, temp)
  
  # Unzip the contents of the temp and save unzipped content in 'temp2'
  GISfiles <- unzip(zipfile = temp, exdir = temp2)
  
}

## write the daily HMS .csv for each centroid

HMSdensity_map <- function(HMSday) {
  year <- substr(HMSday, 1, 4)
  month <- substr(HMSday, 5, 6)
  day <- substr(HMSday, 7, 8)
  
  directory <- tempdir()
  
  setwd(directory)
  
  #Define the URL for each day's GIS files
  
  zFiles <-
    paste0(
      "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/",
      year,
      "/",
      month,
      "/hms_smoke",
      HMSday,
      ".zip"
    )
  
  #Feed URL into the HMSunzip function to extract the necessary .shp file
  
  GISfiles <- HMSunzip(zFiles)
  
  #path <- "~/tempZip/hms_smoke20180303.shp"
  
  #Create a df for that day's HMS polygons
  
  Smk <- st_read(dsn = GISfiles[3]) %>%  
    st_set_crs(4326)
  
  #Identify any polygons containing a BA centroid on the given day and connect the density measure to each
  
  SmkTractDayInt <-
    st_join(plant_centroid_w, st_make_valid(Smk), left = TRUE) %>%
    select(balancing_authority_code, Density) %>%
    mutate(date = HMSday, .keep = "all")
  
  #Re-organize df so that each row list one BA, and contains columns for each Density measure
  #A value of 1 in a column means the BA centroid falls within that density polygon
  st_geometry(SmkTractDayInt) <- NULL
  
  SmkTractDayInt  %>%
    mutate(yn = 1) %>%
    group_by(date, Density, balancing_authority_code) %>%
    summarise(yn = mean(yn)) %>%
    spread(key = Density, value = yn) %>% 
    fwrite(paste0("data/HMScentroids/HMSdensity_", HMSday, ".csv"))
  
  # clear temp folder
  
  unlink(paste0(tempdir(), '/*'))
  
  
  # }
  # else{
  #   return()
  # }
  
}

#########################################################
# execution
#########################################################

# List of dates being analyzed
HMSday <- 
  format(seq(as.Date("2021/07/09"), as.Date("2021/12/31"), by = "day"), "%Y%m%d")

# create a .csv of density data for each day (takes abt 30 min)
lapply(HMSday, HMSdensity_map)

#Dates with problems: "20180727 - Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
#                                 Loop 0 is not valid: Edge 0 crosses edge 82"
#                     "20190710 - requested URL not found on server"
#                     "20190810 - requested URL not found on server"
#                     "20210708 - Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
#                                 Loop 0 is not valid: Edge 0 crosses edge 63

# import those .csv and create a table
densityBAdata <- 
  list.files(path = "data/HMScentroids", 
             pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%                                          
  bind_rows(.)

#format table
colnames(densityBAdata)[5] = "Clear"
densityBAdata[is.na(densityBAdata)] = 0
densityBAdata <- densityBAdata[,c(1,2,5,3,4,6)]

fwrite(densityBAdata, "temp/densityBAdata.csv")

#isolate smoke layers
densityBAdataH <- densityBAdata %>%
  filter(., Heavy == 1) %>%
  mutate(.,
         Clear = 0,
         Light = 0,
         Medium = 0
  )

densityBAdataM <- densityBAdata %>%
  filter(., Heavy == 0 & Medium == 1) %>%
  mutate(.,
         Clear = 0,
         Light = 0,
         Heavy = 0
  )

densityBAdataL <- densityBAdata %>%
  filter(., Heavy == 0 & Medium == 0 & Light == 1) %>%
  mutate(.,
         Clear = 0,
         Medium = 0,
         Heavy = 0
  )

densityBAdataC <- densityBAdata %>%
  filter(., Heavy == 0 & Medium == 0 & Light == 0)

centroidHMSdaily <- rbind(densityBAdataH,
                          densityBAdataM,
                          densityBAdataL,
                          densityBAdataC) %>%
  arrange(., date, balancing_authority_code)

centroidHMSdaily$date <- as.character(centroidHMSdaily$date)

dailyBAgen <- ba_daily_gen %>% 
  arrange(., period, respondent)

dailyBAgen$period <- gsub("-","", dailyBAgen$period)

dailyBApanel <- left_join(
  dailyBAgen,
  centroidHMSdaily,
  by = c(
    "period" = "date",
    "respondent" = "balancing_authority_code"
  )
)

dailyBApanel <- drop_na(dailyBApanel) %>%
  filter(., value >= 0)

dailyBApanel$smoke <- ifelse(
  dailyBApanel$Clear == 1, 0, 1)

dailyBApanel$smokeMedium <- ifelse(
  dailyBApanel$Medium == 1, 1, ifelse(
    dailyBApanel$Heavy == 1, 1, 0
  ))

dailyBApanel$smokeHeavy <- ifelse(
  dailyBApanel$Heavy == 1, 1, 0)

dailyBApanel$month <- substr(dailyBApanel$period, 5, 6)
dailyBApanel$year <- substr(dailyBApanel$period, 1, 4)
dailyBApanel$logvalue <- log(dailyBApanel$value)

dailyBApanel <- dailyBApanel %>% left_join(.,
                            BAtract,
                            by = c(
                              "respondent" = "balancing_authority_code"
                            )
)

cons_ba_panel <- dailyBApanel %>%
  filter(., respondent %in% c("AVA","BANC","EPE","IID","SRP"))

banc_iid_panel <- dailyBApanel %>%
  filter(., respondent %in% c("BANC","IID"))

########################################################

plant_centroid_w$ID <- apply(plant_centroid_w, 1, 
                             function(row) call_geolocator_latlon(
                               row['latitude'], row['longitude'])
                             )

BAtract <- plant_centroid_w[,c(1,5)]
st_geometry(BAtract) <- NULL

BAtract$census_tract <- substr(BAtract$ID, 1, 11)

census.list <- BAtract$census_tract

census.pm <- fread("data/smokePM2pt5_predictions_daily_tract_20060101-20201231.csv")

census.pm$GEOID <- as.character(census.pm$GEOID)

census.pm <- census.pm %>% dplyr::filter(GEOID %in% census.list)

census.pm$date <- gsub("-","", census.pm$date)

dailyBApanel <- dailyBApanel %>% left_join(.,
                            census.pm,
                            by = c(
                              "period" = "date",
                              "census_tract" = "GEOID"
                            )
)

dailyBApanel <- dailyBApanel %>% mutate(
  smokePM_pred = ifelse(is.na(smokePM_pred),0,smokePM_pred))

fwrite(dailyBApanel, "data/panels/daily_gen_by_balancing_authority.csv")
