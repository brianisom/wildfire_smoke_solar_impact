rm(list = ls())
#########################################################
#########################################################
#
#   Fucntions to process HMS data
#   and connect to Block Group population centers
#
#   Jason Vargo jason.vargo@cdph.ca.gov
#   Dec 2018
#   A sample tutorial of this process can be found at https://cdphrusers.github.io/vargoISDSnotebook.nb.html
#
#########################################################
#########################################################

# necessary Packages
library(tidyverse)
library(data.table)
library(sf) 
library(parallel)
library(R.utils)
library(lwgeom)
library(RCurl)


# clear Temp directory
unlink(paste0(tempdir(), '/*'))

# define day for testing
HMSday <-"20201023"


#########################################################
# function to download and unzip gzip files

HMSunzip <- function(zFiles) {
  directory <- tempdir()
  setwd(directory)
  
  temp <- tempfile()
  temp2 <- tempfile()
  
  # Download the zip file and save to 'temp' 
  URL <- zFiles
  download.file(URL, temp)
  
  # Unzip the contents of the temp and save unzipped content in 'temp2'
  unzip(zipfile = temp, exdir = temp2)
  
}

#########################################################
# function to download process HMS (GIS) files in the archives
# http://satepsanone.nesdis.noaa.gov/pub/volcano/FIRE/HMS_ARCHIVE/

HMSdensity_map <- function(HMSday) {
  year <- substr(HMSday, 1, 4)
  month <- substr(HMSday, 5, 6)
  day <- substr(HMSday, 7, 8)
  
  # this `if` statement (and the `else` at the end of the function can be reinstated to handle errors for missing GIS files)
  # if(url.exists(paste0("http://satepsanone.nesdis.noaa.gov/pub/volcano/FIRE/HMS_ARCHIVE/",year,"/GIS/SMOKE/hms_smoke",HMSday,".shp.gz"))){
  
  directory <- tempdir()
  
  setwd(directory)
  
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
  
  GISfiles <- HMSunzip(zFiles)
  
  #path <- "~/tempZip/hms_smoke20180303.shp"
  
  Smk <- st_read(dsn = GISfiles[3]) %>%  
    st_set_crs(4326)
  
  SmkTractDayInt <-
    st_join(plant_centroid_w, st_make_valid(Smk), left = TRUE) %>%
    select(balancing.authority.name, Density) %>%
    mutate(date = HMSday, .keep = "all")
  
  st_geometry(SmkTractDayInt) <- NULL
  
  SmkTractDayInt  %>%
    mutate(yn = 1) %>%
    group_by(date, Density, balancing.authority.name) %>%
    summarise(yn = mean(yn)) %>%
    spread(key = Density, value = yn) %>%
    fwrite(paste0("Documents/Research/WFSmokeAndSolarGen/data/HMScentroids/HMSdensity_", HMSday, ".csv"))
  
  # clear temp folder
  
  unlink(paste0(tempdir(), '/*'))
  
  
  # }
  # else{
  #   return()
  # }
  
}

############### function to update with days not in the archive ############

unlink(paste0(tempdir(), '/*'))

list.files(tempdir())

HMSday <- dateListrecent[1]

#########################################################
# function to download process HMS (GIS) files in the the most recent 6 month archives
# https://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/
# these files do not need to be gunzipped

intersectHMSrecent <- function(HMSday) {
  year <- substr(HMSday, 1, 4)
  month <- substr(HMSday, 5, 6)
  day <- substr(HMSday, 7, 8)
  
  # this `if` statement (and the `else` at the end of the function can be reinstated to handle errors for missing GIS files)
  # if(url.exists(paste0("http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke",HMSday,".dbf"))){
  
  directory <- tempdir()
  
  setwd(directory)
  
  shpFiles <-
    list(
      paste0(
        "http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke",
        HMSday,
        ".dbf"
      ),
      paste0(
        "http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke",
        HMSday,
        ".shp"
      ),
      paste0(
        "http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke",
        HMSday,
        ".shx"
      )
    )
  
  shpNames <- list("HMSGIS.dbf", "HMSGIS.shp", "HMSGIS.shx")
  
  GISfiles <-
    mapply(download.file,
           shpFiles,
           shpNames,
           MoreArgs = list(method = "auto", mode = "wb"))
  
  Smk <- st_read(dsn = "HMSGIS.shp") %>%  st_set_crs(4326) %>%
    mutate(
      year = substr(HMSday, 1, 4),
      month = substr(HMSday, 5, 6),
      day = substr(HMSday, 7, 8),
      date = paste0(year, month, day),
      smoke = factor(
        ifelse(
          Density %in% c(5, "5.000"),
          "light",
          ifelse(
            Density %in% c(27, "27.000") ,
            "heavy",
            ifelse(Density %in% c(16, "16.000"), "medium", "missing")
          )
        ),
        levels = c("light", "medium", "heavy", "missing")
      )
    )
  
  SmkTractDayInt <-
    st_intersection(st_make_valid(Smk), popCentroidsBG) %>%
    select(date, smoke, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, POPULATION)
  
  st_geometry(SmkTractDayInt) <- NULL
  
  SmkTractDayInt  %>%
    mutate(yn = 1) %>%
    group_by(date, smoke, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, POPULATION) %>%
    summarise(yn = mean(yn)) %>%
    spread(key = smoke, value = yn) %>%
    fwrite(paste0(dailyFileFolderPath,"US_BG", HMSday, ".csv"))
  
  # clear temp folder
  
  unlink(paste0(tempdir(), '/*'))
  
  
  # }
  # else{
  #   return()
  # }
  
}



#########################################################
# function to process HMS (GIS) files that has been downloaded manually to your machine
# This is helpful for problematic dates


intersectHMSdirect <- function(HMSday) {
  year <- substr(HMSday, 1, 4)
  month <- substr(HMSday, 5, 6)
  day <- substr(HMSday, 7, 8)
  
  
  Smk <-
    st_read(dsn = paste0("~/temp/smokepolygons.", HMSday, ".shp")) %>%  st_set_crs(4326) %>%
    mutate(
      year = substr(HMSday, 1, 4),
      month = substr(HMSday, 5, 6),
      day = substr(HMSday, 7, 8),
      date = paste0(year, month, day),
      smoke = factor(
        ifelse(
          Density %in% c(5, "5.000"),
          "light",
          ifelse(
            Density %in% c(27, "27.000") ,
            "heavy",
            ifelse(Density %in% c(16, "16.000"), "medium", "missing")
          )
        ),
        levels = c("light", "medium", "heavy", "missing")
      )
    )
  
  
  SmkTractDayInt <-
    st_intersection(st_make_valid(Smk), popCentroidsBG) %>%
    select(date, smoke, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, POPULATION)
  
  st_geometry(SmkTractDayInt) <- NULL
  
  SmkTractDayInt  %>%
    mutate(yn = 1) %>%
    group_by(date, smoke, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, POPULATION) %>%
    summarise(yn = mean(yn)) %>%
    spread(key = smoke, value = yn) %>%
    fwrite(paste0(dailyFileFolderPath,"US_BG", HMSday, ".csv"))
  
  
  # clear temp folder
  
  unlink(paste0(tempdir(), '/*'))
  
}

#################################################################
# Read in spatial points - using 2010 block group population centers
# this path (URL) can be modified to isolate a single state
# https://www.census.gov/geo/reference/centersofpop.html

# convert the table of BG centers to spatial object
baCentroids <-
  st_as_sf(BA_centroid_w,
           coords = c("longitude", "latitude"),
           crs = 4326)

#####################################################################
# run function to process mulitple archived dates

# define a folder in which to place processed file for a single day
dailyFileFolderPath <- "/WFSmokeProcessing/HMSdata/US_BG/CSV/"

# create list over which to run the archive function
dateListarchive <-
  format(seq(as.Date("2015/03/23"), as.Date("2017/12/31"), by = "day"), "%Y%m%d")

# apply the function to each date in the archive list
lapply(dateListarchive, FUN = intersectHMSarchive)

#####################################################################
# run function to process mulitple recent dates

# create list over which to run the archive function
# this is usful for updating once you have run over the archive already
# check this website to see the most recent date available http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/
dateListrecent <-
  format(seq(as.Date("2019/01/09"), as.Date("2019/04/20"), by = "day"), "%Y%m%d")
# apply the function to each date in the archive list
lapply(dateListrecent, FUN = intersectHMSrecent)


#####################################################################
# Combine daily files into large smoke file
# this takes awy to o long since it recreates the master smoke file every time you update with new dates
# and should be adapted to only add new records

rbindlist(mclapply(list.files(dailyFileFolderPath,full.names = T), fread), fill = T)  %>%
  fwrite("R:/WFSmokeProcessing/HMSdata/smokeFile.csv" )




#######################################################################
# Update Smoke File for use in other locations - some data subsetting and aggregation

smokeFile <- fread("R:/WFSmokeProcessing/HMSdata/smokeFile.csv" )
smokeFile %>% saveRDS("R:/WFSmokeProcessing/HMSdata/smokeFile.rds" )
# smokeFile[STATEFP ==6L] %>% fwrite("~/data/CAsmokeFile.csv" )
smokeFile[STATEFP == 6L]  %>% replace_na(list(
  light = 0,
  medium = 0,
  heavy = 0
)) %>%
  .[, .(
    light = max(light, na.rm = T),
    medium = max(medium, na.rm = T),
    heavy = max(heavy, na.rm = T),
    POPULATION = mean(POPULATION, na.rm = T)
  ),
  by = .(date, STATEFP, COUNTYFP, TRACTCE)] %>%
  fwrite("R:/ClimateExposures/CAsmokeFileTracts.csv")

# %>% fwrite('R:/ClimateExposures/CAsmokeFile.csv')
# 
replace_na(smokeFile, list(light = 0, medium = 0, heavy= 0)) %>% .[, .(light = sum(light * POPULATION, na.rm=T),
                                                                       medium = sum(medium * POPULATION, na.rm=T),
                                                                       heavy = sum(heavy * POPULATION, na.rm=T),
                                                                       POPULATION = sum(POPULATION, na.rm=T)), by=.(date, STATEFP, COUNTYFP)] %>%
  fwrite("~/GitHub/WFSmokeExp/MMWRdashboard/USsmokeFile.csv")
# 


