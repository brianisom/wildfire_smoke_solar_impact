###############################################################################

# Solar Data Data for solar generation is collected via the US Energy 
# Information Agency's Open Data Application Programming Interface (API): 
# <https://www.eia.gov/opendata/>

# EIA APIv2 technical documentation: 
# <https://www.eia.gov/opendata/documentation.php>

###############################################################################

# Monthly Generation by Solar Plant 

# The following loop calls monthly solar generation for every solar PV producer
# in all 12 states in the model via EIA's API. The data spans 2013-2020.

###############################################################################

# This first loop finds the length of each call. Those over 5000 exceed the
# limit of the API and require multiple calls with an offset.

eia_key <- "iPjXaaJSiyfWNALV62yg1Onb0VTuUgVHa0eVHgbx"
start_date <- "2013-01"
end_date <- "2020-12"
frequency <- "monthly"
data <- "gross-generation"
prime_mover <- "PV"
state <-
  c(
    "AZ",
    "CA",
    "CO",
    "ID",
    "MT",
    "NM",
    "NV",
    "OR",
    "TX",
    "UT",
    "WA",
    "WY"
  )
sort_by <- "plantCode"

# Begin loop to identify the length of each state's df----

for (i in 1:12) {
  url <-
    paste(
      "https://api.eia.gov/v2/electricity/facility-fuel/data?api_key=",
      eia_key,
      "&frequency=",
      frequency,
      "&start=",
      start_date,
      "&end=",
      end_date,
      "&data[]=",
      data,
      "&facets[state][]=",
      state[i],
      "&facets[primeMover][]=",
      prime_mover,
      "&sort[0][column]=",
      sort_by,
      "&sort[0][direction]=asc",
      sep = ""
    )
  
  response <- GET(url)
  
  plant_gen_json <- fromJSON(rawToChar(response$content))
  
  assign(paste0("total_", state[[i]]), plant_gen_json$response$total)
  
}

# API call parameters ---------------------------------------------------------
# This pulls data for all states below the API's 5000 observation limit
# Plants with >5000 each have individual loops below

eia_key <- "iPjXaaJSiyfWNALV62yg1Onb0VTuUgVHa0eVHgbx"
start_date <- "2013-01"
end_date <- "2020-12"
frequency <- "monthly"
data <- "gross-generation"
prime_mover <- "PV"
state <-c(
  "CO",
  "ID",
  "MT",
  "NM",
  "NV",
  "OR",
  "TX",
  "UT",
  "WA",
  "WY"
  )
sort_by <- "plantCode"
n_iter <- 10

# Initializes the progress bar
pb <- txtProgressBar(
  min = 0,
  max = n_iter,
  style = 3,
  width = 50,
  char = "="
)

# Begin loop ----
j <- 0
for (i in 1:10) {
  url <-
    paste(
      "https://api.eia.gov/v2/electricity/facility-fuel/data?api_key=",
      eia_key,
      "&frequency=",
      frequency,
      "&start=",
      start_date,
      "&end=",
      end_date,
      "&data[]=",
      data,
      "&facets[state][]=",
      state[i],
      "&facets[primeMover][]=",
      prime_mover,
      "&sort[0][column]=",
      sort_by,
      "&sort[0][direction]=asc",
      sep = ""
    )

  response <- GET(url)

  plant_gen_json <- fromJSON(rawToChar(response$content))

  plant_gen_df <- data.frame(plant_gen_json$response$data)

  if (j == 0) {
    plant_gen <- plant_gen_df
  } else {
    plant_gen <- rbind(plant_gen, plant_gen_df)
  }

  setTxtProgressBar(pb, i)

  j <- j + 1
}

close(pb)

fwrite(plant_gen, "data/API/plant_level_monthly_solar_gen.csv")

# AZ API call parameters ------------------------------------------------------
eia_key <- "iPjXaaJSiyfWNALV62yg1Onb0VTuUgVHa0eVHgbx"
start_date <- "2013-01"
end_date <- "2020-12"
frequency <- "monthly"
data <- "gross-generation"
prime_mover <- "PV"
state <- "AZ"
sort_by <- "plantCode"
offset <- c("0", "5000")
n_iter <- 2

# Initializes the progress bar
pb <- txtProgressBar(
  min = 0,
  max = n_iter,
  style = 3,
  width = 50,
  char = "="
)

# Begin loop ----
j <- 0
for (i in 1:2) {
  url <-
    paste(
      "https://api.eia.gov/v2/electricity/facility-fuel/data?api_key=",
      eia_key,
      "&frequency=",
      frequency,
      "&start=",
      start_date,
      "&end=",
      end_date,
      "&data[]=",
      data,
      "&facets[state][]=",
      state,
      "&facets[primeMover][]=",
      prime_mover,
      "&sort[0][column]=",
      sort_by,
      "&sort[0][direction]=asc",
      "&offset=",
      offset[i],
      sep = ""
    )
  
  response <- GET(url)
  
  plant_gen_json <- fromJSON(rawToChar(response$content))
  
  plant_gen_df_az <- data.frame(plant_gen_json$response$data)
  
  if (j == 0) {
    plant_gen_az <- plant_gen_df_az
  } else {
    plant_gen_az <- rbind(plant_gen_az, plant_gen_df_az)
  }
  
  setTxtProgressBar(pb, i)
  
  j <- j + 1
}

close(pb)

fwrite(plant_gen_az, "data/API/az_plant_level_monthly_solar_gen.csv")

# CA API call parameters ------------------------------------------------------
eia_key <- "iPjXaaJSiyfWNALV62yg1Onb0VTuUgVHa0eVHgbx"
start_date <- "2013-01"
end_date <- "2020-12"
frequency <- "monthly"
data <- "gross-generation"
prime_mover <- "PV"
state <- "CA"
sort_by <- "plantCode"
offset <- c("0", "5000", "10000", "15000", "20000", "25000", "30000")
n_iter <- 7

# Initializes the progress bar
pb <- txtProgressBar(
  min = 0,
  max = n_iter,
  style = 3,
  width = 50,
  char = "="
)

# Begin loop ----
j <- 0
for (i in 1:7) {
  url <-
    paste(
      "https://api.eia.gov/v2/electricity/facility-fuel/data?api_key=",
      eia_key,
      "&frequency=",
      frequency,
      "&start=",
      start_date,
      "&end=",
      end_date,
      "&data[]=",
      data,
      "&facets[state][]=",
      state,
      "&facets[primeMover][]=",
      prime_mover,
      "&sort[0][column]=",
      sort_by,
      "&sort[0][direction]=asc",
      "&offset=",
      offset[i],
      sep = ""
    )
  
  response <- GET(url)
  
  plant_gen_json <- fromJSON(rawToChar(response$content))
  
  plant_gen_df_ca <- data.frame(plant_gen_json$response$data)
  
  if (j == 0) {
    plant_gen_ca <- plant_gen_df_ca
  } else {
    plant_gen_ca <- rbind(plant_gen_ca, plant_gen_df_ca)
  }
  
  setTxtProgressBar(pb, i)
  
  j <- j + 1
}

close(pb)

fwrite(plant_gen_ca, "data/API/ca_plant_level_monthly_solar_gen.csv")