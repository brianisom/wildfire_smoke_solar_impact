###############################################################################

# Solar Data Data for solar generation is collected via the US Energy 
# Information Agency's Open Data Application Programming Interface (API): 
# <https://www.eia.gov/opendata/>

# EIA APIv2 technical documentation: 
# <https://www.eia.gov/opendata/documentation.php>

###############################################################################

# Daily Generation by Balancing Authority

# EIA does not provide hourly or daily solar generation data at the individual 
# plan level. The EIA only provides hourly and daily solar generation numbers 
# collected by balancing authority. Some of these balancing authorities are 
# concentrated in a small-enough geographic area that they could be useful as a
# measured entity. The following loop collects daily generation data from those 
# balancing authorities, from June 30, 2018 (the oldest date available) to 
# Dec 31, 2021.

###############################################################################

# API call parameters ----

eia_key <- "iPjXaaJSiyfWNALV62yg1Onb0VTuUgVHa0eVHgbx"
start_date <- "2018-06-30"
end_date <- "2021-12-31"
frequency <- "daily"
data <- "value"
fuel_type <- "SUN"
ba_code <- c(
  "AVA",
  "AVRN",
  "AZPS",
  "BANC",
  "BPAT",
  "CISO",
  "EPE",
  "ERCO",
  "IID",
  "IPCO",
  "LDWP",
  "NEVP",
  "NWMT",
  "PACE",
  "PACW",
  "PGE",
  "PNM",
  "PSCO",
  "PSEI",
  "SRP",
  "SWPP",
  "TEPC",
  "WACM",
  "WALC"
)
sort_by <- "respondent"
n_iter <- 24

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
for (i in 1:24) {
  url <-
    paste(
      "https://api.eia.gov/v2/electricity/rto/daily-fuel-type-data/data?api_key=",
      eia_key,
      "&frequency=",
      frequency,
      "&start=",
      start_date,
      "&end=",
      end_date,
      "&data[]=",
      data,
      "&facets[respondent][]=",
      ba_code[i],
      "&facets[fueltype][]=",
      fuel_type,
      "&facets[timezone][]=Pacific",
      "&sort[0][column]=",
      sort_by,
      "&sort[0][direction]=asc",
      "&length=999999",
      sep = ""
    )

  response <- GET(url)

  ba_gen_json <- fromJSON(rawToChar(response$content))

  ba_gen_df <- data.frame(ba_gen_json$response$data)

  if (j == 0) {
    ba_gen <- ba_gen_df
  } else {
    ba_gen <- rbind(ba_gen, ba_gen_df)
  }

  setTxtProgressBar(pb, i)

  j <- j + 1
}

close(pb)

fwrite(
  ba_gen,
  "data/API/ba_daily_gen.csv"
)
