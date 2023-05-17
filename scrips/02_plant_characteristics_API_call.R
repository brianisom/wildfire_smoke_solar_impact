###############################################################################

# Solar Data Data for solar generation is collected via the US Energy 
# Information Agency's Open Data Application Programming Interface (API): 
# <https://www.eia.gov/opendata/>

# EIA APIv2 technical documentation: 
# <https://www.eia.gov/opendata/documentation.php>

###############################################################################

# Solar Plant Characteristics

# This file calls characteristics for each solar PV plant in the analysis 
# via EIA's API. The AZ, CA, and CO data are so large that they have to be
# called via separate loops.

###############################################################################

# This first loop finds the length of each call. Those over 5000 exceed the
# limit of the API and require multiple calls with an offset.

#Define key parameters
eia_key <- "iPjXaaJSiyfWNALV62yg1Onb0VTuUgVHa0eVHgbx"
start_date <- "2013-01"
end_date <- "2020-12"
frequency <- "monthly"
prime_mover <- "PV"
state <-
  c("AZ",
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
    "WY")
sort_by <- "plantid"
n_iter <- 12


# Begin loop ----
for (i in 1:12) {
  url2 <-
    paste(
      "https://api.eia.gov/v2/electricity/operating-generator-capacity/data?api_key=",
      eia_key,
      "&frequency=",
      frequency,
      "&start=",
      start_date,
      "&end=",
      end_date,
      "&data[]=county",
      "&data[]=latitude",
      "&data[]=longitude",
      "&data[]=nameplate-capacity-mw",
      "&data[]=operating-year-month",
      "&facets[stateid][]=",
      state[i],
      "&facets[prime_mover_code][]=",
      prime_mover,
      "&sort[0][column]=",
      sort_by,
      "&sort[0][direction]=asc",
      sep = ""
    )
  
  response2 <- GET(url2)
  
  plant_char_json <- fromJSON(rawToChar(response2$content))
  
  assign(paste0("total_char_", state[[i]]), plant_char_json$response$total)
  
}

# API call parameters ----------------------------------------------------------
# This pulls data for all states below the API's 5000 observation limit
# Plants with >5000 each have individual loops below

eia_key <- "iPjXaaJSiyfWNALV62yg1Onb0VTuUgVHa0eVHgbx"
start_date <- "2013-01"
end_date <- "2020-12"
frequency <- "monthly"
prime_mover <- "PV"
state <- c(
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
sort_by <- "plantid"
n_iter <- 9

# Initializes the progress bar
pb <- txtProgressBar(
  min = 0,
  max = n_iter,
  style = 3,
  width = 50,
  char = "="
)

# Begin loop ----
j = 0
for (i in 1:9) {
  url2 <-
    paste(
      "https://api.eia.gov/v2/electricity/operating-generator-capacity/data?api_key=",
      eia_key,
      "&frequency=",
      frequency,
      "&start=",
      start_date,
      "&end=",
      end_date,
      "&data[]=county",
      "&data[]=latitude",
      "&data[]=longitude",
      "&data[]=nameplate-capacity-mw",
      "&data[]=operating-year-month",
      "&facets[stateid][]=",
      state[i],
      "&facets[prime_mover_code][]=",
      prime_mover,
      "&sort[0][column]=",
      sort_by,
      "&sort[0][direction]=asc",
      sep = ""
    )
  
  response2 <- GET(url2)
  
  plant_char_json <- fromJSON(rawToChar(response2$content))
  
  plant_char_df <- data.frame(plant_char_json$response$data)
  
  if (j == 0) {
    plant_char <- plant_char_df
  }
  else{
    plant_char <- rbind(plant_char, plant_char_df)
  }
  
  setTxtProgressBar(pb, i)
  
  j = j + 1
  
}

close(pb)

fwrite(plant_char, "data/API/plant_char.csv")

# API call parameters for AZ --------------------------------------------------
eia_key <- "iPjXaaJSiyfWNALV62yg1Onb0VTuUgVHa0eVHgbx"
start_date <- "2013-01"
end_date <- "2020-12"
frequency <- "monthly"
prime_mover <- "PV"
sort_by <- "plantid"
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
j = 0
for (i in 1:2) {
  url2 <-
    paste(
      "https://api.eia.gov/v2/electricity/operating-generator-capacity/data?api_key=",
      eia_key,
      "&frequency=",
      frequency,
      "&start=",
      start_date,
      "&end=",
      end_date,
      "&data[]=county",
      "&data[]=latitude",
      "&data[]=longitude",
      "&data[]=nameplate-capacity-mw",
      "&data[]=operating-year-month",
      "&facets[stateid][]=AZ",
      "&facets[prime_mover_code][]=",
      prime_mover,
      "&sort[0][column]=",
      sort_by,
      "&sort[0][direction]=asc",
      "&offset=",
      offset[i],
      sep = ""
    )
  
  response2 <- GET(url2)
  
  plant_char_json_az <- fromJSON(rawToChar(response2$content))
  
  plant_char_df_az <- data.frame(plant_char_json_az$response$data)
  
  if (j == 0) {
    plant_char_az <- plant_char_df_az
  }
  else{
    plant_char_az <- rbind(plant_char_az, plant_char_df_az)
  }
  
  setTxtProgressBar(pb, i)
  
  j = j + 1
  
}

close(pb)

fwrite(plant_char_az, "data/API/az_plant_char.csv")


# API call parameters for CA --------------------------------------------------
eia_key <- "iPjXaaJSiyfWNALV62yg1Onb0VTuUgVHa0eVHgbx"
start_date <- "2013-01"
end_date <- "2020-12"
frequency <- "monthly"
prime_mover <- "PV"
sort_by <- "plantid"
offset <- c("0", 
            "5000", 
            "10000",
            "15000",
            "20000",
            "25000",
            "30000",
            "35000",
            "40000",
            "45000",
            "50000",
            "55000"
            )
n_iter <- 12

# Initializes the progress bar
pb <- txtProgressBar(
  min = 0,
  max = n_iter,
  style = 3,
  width = 50,
  char = "="
)

# Begin loop ----
j = 0
for (i in 1:12) {
  url2 <-
    paste(
      "https://api.eia.gov/v2/electricity/operating-generator-capacity/data?api_key=",
      eia_key,
      "&frequency=",
      frequency,
      "&start=",
      start_date,
      "&end=",
      end_date,
      "&data[]=county",
      "&data[]=latitude",
      "&data[]=longitude",
      "&data[]=nameplate-capacity-mw",
      "&data[]=operating-year-month",
      "&facets[stateid][]=CA",
      "&facets[prime_mover_code][]=",
      prime_mover,
      "&sort[0][column]=",
      sort_by,
      "&sort[0][direction]=asc",
      "&offset=",
      offset[i],
      sep = ""
    )
  
  response2 <- GET(url2)
  
  plant_char_json_ca <- fromJSON(rawToChar(response2$content))
  
  plant_char_df_ca <- data.frame(plant_char_json_ca$response$data)
  
  if (j == 0) {
    plant_char_ca <- plant_char_df_ca
  }
  else{
    plant_char_ca <- rbind(plant_char_ca, plant_char_df_ca)
  }
  
  setTxtProgressBar(pb, i)
  
  j = j + 1
  
}

close(pb)

fwrite(plant_char_ca, "data/API/ca_plant_char.csv")

# API call parameters for CO --------------------------------------------------
eia_key <- "iPjXaaJSiyfWNALV62yg1Onb0VTuUgVHa0eVHgbx"
start_date <- "2013-01"
end_date <- "2020-12"
frequency <- "monthly"
prime_mover <- "PV"
sort_by <- "plantid"
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
j = 0
for (i in 1:2) {
  url2 <-
    paste(
      "https://api.eia.gov/v2/electricity/operating-generator-capacity/data?api_key=",
      eia_key,
      "&frequency=",
      frequency,
      "&start=",
      start_date,
      "&end=",
      end_date,
      "&data[]=county",
      "&data[]=latitude",
      "&data[]=longitude",
      "&data[]=nameplate-capacity-mw",
      "&data[]=operating-year-month",
      "&facets[stateid][]=CO",
      "&facets[prime_mover_code][]=",
      prime_mover,
      "&sort[0][column]=",
      sort_by,
      "&sort[0][direction]=asc",
      "&offset=",
      offset[i],
      sep = ""
    )
  
  response2 <- GET(url2)
  
  plant_char_json_co <- fromJSON(rawToChar(response2$content))
  
  plant_char_df_co <- data.frame(plant_char_json_co$response$data)
  
  if (j == 0) {
    plant_char_co <- plant_char_df_co
  }
  else{
    plant_char_co <- rbind(plant_char_co, plant_char_df_co)
  }
  
  setTxtProgressBar(pb, i)
  
  j = j + 1
  
}

close(pb)

fwrite(plant_char_co, "data/API/co_plant_char.csv")

