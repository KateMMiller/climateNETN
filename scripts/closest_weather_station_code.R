library(tidyverse)
library(sf)
library(rgeoboundaries)
library(tmap)
# devtools::install_github("ropensci/rnoaa")
library(rnoaa)

netn_cent <- read.csv("../data/climate_temp/closest_ws_temp.csv") |>
  select(UnitCode, UnitName, SubUnit, park_long, park_lat)

# find nearest noaa weather station
# rnoaa was removed from CRAN last month. Install from github
#devtools::install_github("ropensci/rnoaa") # only use to find weather stations
stns <- ghcnd_stations()
lat_lon_df <- netn_cent |>
  mutate(id = ifelse(!is.na(SubUnit), paste0(UnitCode, "_", SubUnit), UnitCode)) |>
  select(id, latitude = park_lat, longitude = park_long)
#lat_lon_df <- getSites(active = F) |> select(id = SiteCode, latitude = SiteLatitude, longitude = SiteLongitude)
nearby_stations <- meteo_nearby_stations(lat_lon_df = lat_lon_df, station_data = stns, radius = 50)
# Vetted period of record via: https://xmacis.rcc-acis.org/
# Couldn't find a relevant WS for IAH

# closest_station <-
closest_WS1 <- map_dfr(seq_along(nearby_stations),
                 function(x){
                   site = names(nearby_stations)[x]
                   dat1 <- data.frame(site,
                                      nearby_stations[[x]][, 'id'],
                                      nearby_stations[[x]][, 'latitude'],
                                      nearby_stations[[x]][, 'longitude'],
                                      nearby_stations[[x]][, "distance"])
                 return(dat1)
                 })
# MORR USC00281592 por 2000 - 2013
# MORR US1NJMS0003 was close but small por
head(closest_WS1)
colnames(closest_WS1) <- c("IDCode", "id", "ws_lat", "ws_long", "ws_dist_km")
closest_WS1$UnitCode <- ifelse(grepl("^AC", closest_WS1$IDCode), "ACAD",
                               substr(closest_WS1$IDCode, 1, 4))
closest_WS1$SubUnit <- substr(closest_WS1$IDCode, 6, 8)

# Revise manually to include nearest WS with most complete record from 2006-2024, including
# data from 2024. Used scacis.rcc-acis.org to find best ws per park.
# Couldn't find a better wstn for IAH or SCH than MARS
best_ws <- data.frame(UnitCode = c("ACAD", "BOHA", "MABI", "MIMA", "MORR", "ROVA",
                                   "SAGA", "SAIR", "SARA", "WEFA"),
                      id = c("USR0000MMCF", "USW00014739", "USC00439984", "USW00014702", "USC00281335", "USW00064756",
                             "USC00435768", "USC00196783", "USC00301068", "USW00054734"))
closest_WS <- left_join(best_ws, closest_WS1, by = c("UnitCode", "id"))
head(closest_WS)

# Finclosest_WS# Find the county for each water monitoring location, and add to closest_WS
us_states <- geoboundaries("USA", "adm1")
us_county <- geoboundaries("USA", "adm2")
#st_crs(us_county)
ws_sf <- st_as_sf(closest_WS, coords = c("ws_long", "ws_lat"), crs = 4326)
st_crs(us_county) == st_crs(ws_sf) # T
parks <- st_as_sf(netn_cent, coords = c("park_long", "park_lat"), crs = 4326)

# Look at data to check overlap
tm_shape(us_county, bbox = parks) + tm_borders("grey") +
  tm_shape(us_states, bbox = ws_sf) + tm_borders("black") +
  tm_shape(parks) + tm_dots("blue", size = 0.4) +
  tm_shape(ws_sf) + tm_dots("red", size = 0.5)

# relate weather station to state and county
ws_cty <- st_join(ws_sf, us_county)
ws_cty_final <- left_join(closest_WS,
                          ws_cty |> select(IDCode, WStnCounty = shapeName),
                          by = "IDCode") |>
                as.data.frame() |> select(-geometry)

table(ws_cty_final$UnitCode, ws_cty_final$WStnCounty)

ws_state <- st_join(ws_sf, us_states)
ws_comb <- left_join(ws_cty_final, ws_state |> select(IDCode, WStnState = shapeName), by = "IDCode") |>
  select(-geometry) #|>

table(ws_comb$UnitCode, ws_comb$WStnState)
head(ws_comb)
# relate site to state and county
park_state <- st_join(parks, us_states) |> select(UnitCode, SubUnit, ParkState = shapeName) |>
  as.data.frame() |> select(-geometry)

park_cty <- st_join(parks, us_county) |> select(UnitCode, SubUnit, ParkCounty = shapeName) |>
  as.data.frame() |> select(-geometry)

park_comb <- left_join(park_cty, park_state |> select(UnitCode, SubUnit, ParkState), by = c("UnitCode", "SubUnit"))

# Combine site and ws counties
head(ws_comb)
head(park_comb)

comb_all <- left_join(park_comb |> select(UnitCode, SubUnit, ParkCounty, ParkState),
                      ws_comb |> select(-IDCode),
                      by = c("UnitCode", "SubUnit"))
head(comb_all)
# Add County and State FIPS code
library(tidycensus)
data("fips_codes")
head(fips_codes)
fips_codes <- fips_codes |> mutate(county1 = word(county, 1))

comb_wsfips <- left_join(comb_all, fips_codes, by = c("WStnState" = "state_name",
                                                      "WStnCounty" = "county1")) |>
  mutate(WStnFIPS = paste0(state_code, county_code)) |>
  select(-state, -county, -state_code, -county_code)

comb_parkfips <- left_join(comb_all, fips_codes, by = c("ParkState" = "state_name",
                                                        "ParkCounty" = "county1")) |>
  mutate(ParkFIPS = paste0(state_code, county_code)) |>
  select(UnitCode, SubUnit, ParkFIPS)

comb2 <- full_join(comb_wsfips, comb_parkfips, by = c("UnitCode", "SubUnit"))
closest_WS <- left_join(comb2, netn_cent, by = c("UnitCode", "SubUnit")) |>
  select(UnitCode, SubUnit, UnitName, park_long, park_lat, id, ws_long, ws_lat, ws_dist_km,
         ParkCounty, ParkState, ParkFIPS, WStnCounty, WStnState, WStnFIPS)

# Add to data folder associated with package
usethis::use_data(closest_WS, overwrite = T)



