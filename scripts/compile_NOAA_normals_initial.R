# NCEI NOAA Gridded normals
# NCEI.NOAA.GOV gridded normals

#library(terra)
library(sf)
library(tidyverse)
#importData()
path = "C:/NETN/R_Dev/data/"

# Note I had issues with sf generating centroids, so I created them in ArcPro from NPS admin boundaries shapefile downloaded from IRMA.

# NETN bounding box
NETN_bbox <- data.frame(lat = c(47.38, 44.80, 38.71, 43.40, 39.994187),
                        long = c(-68.71, -66.67, -74.84, -75.54, -80.521832)) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> st_bbox()
# last point is farthest south and west, so region doesn't look as weird.

netn_cent <- read.csv("../data/NPS_NETN_centroids_wgs84.txt") |>
  select(UnitCode = UNIT_CODE, UnitName = UNIT_NAME, long = POINT_X, lat = POINT_Y) |>
  arrange(UnitCode)

netn_cent$UnitCode[netn_cent$UnitCode == "HOFR"] <- "ROVA"
netn_cent$UnitName[netn_cent$UnitCode == "ROVA"] <- "Roosevelt-Vanderbilt National Historic Sites"
netn_cent$long[netn_cent$UnitCode == "ACAD"] <- -68.260190 # had to move b/c landed in water
netn_cent$lat[netn_cent$UnitCode == "ACAD"] <- 44.376559 # had to move b/c landed in water
netn_cent$long[netn_cent$UnitCode == "BOHA"] <- -71.0215867 # had to move b/c landed in water
netn_cent$lat[netn_cent$UnitCode == "BOHA"] <- 42.2709746 # had to move b/c landed in water
write.csv(netn_cent, paste0(path, "NETN_park_centroids_wgs84.csv"), row.names = F)
NETN_centroids <- netn_cent
#NETN_cent_sf <- netn_cent |> st_as_sf(coords = c("long", "lat"), crs = 4326)
usethis::use_data(NETN_centroids, overwrite = T)

# Normals were manually downloaded from:
  # "https://www.ncei.noaa.gov/products/land-based-station/us-climate-normals"

# Extract 1991-2020 climate data for NETN park centroids
ppt30norm <- rast("../data/NRCC/prcp-1991_2020-monthly-normals-v1.0.nc")

# wind <- extent(-77, -65, 38, 48)
# raster::plot(wind)
# plot(ppt30norm[[1]], add = T)
# plot(NETN_centroids[1], add = T, pch = 20)

ppt30 <- terra::extract(ppt30norm, netn_cent[,c("long", "lat")])
ppt30$UnitCode = netn_cent$UnitCode
ppt30_long <- ppt30 |> select(UnitCode, mlyprcp_norm_1:mlyprcp_norm_12, mlyprcp_std_1:mlyprcp_std_12) |>
  pivot_longer(-UnitCode, names_to = "parameter", values_to = "mlyprcp") |>
  mutate(month = as.numeric(gsub("\\D", "", parameter)),
         stat = ifelse(grepl("norm", parameter), "norm", "std"),
         param = paste0("precip_", stat),
         norm = "norm1991-2020") |>
  select(-parameter, -stat) |>
  pivot_wider(names_from = param, values_from = mlyprcp)

tmax30norm <- rast("../data/NRCC/tmax-1991_2020-monthly-normals-v1.0.nc")
tmax30 <- terra::extract(tmax30norm, netn_cent[,c("long", "lat")])
tmax30$UnitCode = netn_cent$UnitCode
tmax30_long <- tmax30 |> select(UnitCode, mlytmax_norm_1:mlytmax_norm_12, mlytmax_std_1:mlytmax_std_12) |>
  pivot_longer(-UnitCode, names_to = "parameter", values_to = "mlytmax") |>
  mutate(month = as.numeric(gsub("\\D", "", parameter)),
         stat = ifelse(grepl("norm", parameter), "norm", "std"),
         param = paste0("tmax_", stat),
         norm = "norm1991-2020") |>
  select(-parameter, -stat) |>
  pivot_wider(names_from = param, values_from = mlytmax)
head(tmax30_long)

tmin30norm <- rast("../data/NRCC/tmin-1991_2020-monthly-normals-v1.0.nc")
tmin30 <- terra::extract(tmin30norm, netn_cent[,c("long", "lat")])
tmin30$UnitCode = netn_cent$UnitCode
tmin30_long <- tmin30 |> select(UnitCode, mlytmin_norm_1:mlytmin_norm_12, mlytmin_std_1:mlytmin_std_12) |>
  pivot_longer(-UnitCode, names_to = "parameter", values_to = "mlytmin") |>
  mutate(month = as.numeric(gsub("\\D", "", parameter)),
         stat = ifelse(grepl("norm", parameter), "norm", "std"),
         param = paste0("tmin_", stat),
         norm = "norm1991-2020") |>
  select(-parameter, -stat) |>
  pivot_wider(names_from = param, values_from = mlytmin)
head(tmin30_long)

tavg30norm <- rast("../data/NRCC/tavg-1991_2020-monthly-normals-v1.0.nc")
tavg30 <- terra::extract(tavg30norm, netn_cent[,c("long", "lat")])
tavg30$UnitCode = netn_cent$UnitCode
tavg30_long <- tavg30 |> select(UnitCode, mlytavg_norm_1:mlytavg_norm_12, mlytavg_std_1:mlytavg_std_12) |>
  pivot_longer(-UnitCode, names_to = "parameter", values_to = "mlytavg") |>
  mutate(month = as.numeric(gsub("\\D", "", parameter)),
         stat = ifelse(grepl("norm", parameter), "norm", "std"),
         param = paste0("tavg_", stat),
         norm = "norm1991-2020") |>
  select(-parameter, -stat) |>
  pivot_wider(names_from = param, values_from = mlytavg)
head(ppt30_long)
head(tmax30_long)
norm30 <- list(ppt30_long, tmax30_long, tmin30_long, tavg30_long)
comb30 <- reduce(norm30, full_join, by = c("UnitCode", "month", "norm"))
head(comb30)

# Extract 1901-2000 climate data for NETN park centroids
ppt19norm <- rast("../data/NRCC/prcp-1901_2000-monthly-normals-v1.0.nc")
st_crs(ppt19norm)

ppt <- terra::extract(ppt19norm, netn_cent[,c("long", "lat")])
ppt$UnitCode = netn_cent$UnitCode
ppt_long <- ppt |> select(UnitCode, mlyprcp_norm_1:mlyprcp_norm_12, mlyprcp_std_1:mlyprcp_std_12) |>
  pivot_longer(-UnitCode, names_to = "parameter", values_to = "mlyprcp") |>
  mutate(month = as.numeric(gsub("\\D", "", parameter)),
         stat = ifelse(grepl("norm", parameter), "norm", "std"),
         param = paste0("precip_", stat),
         norm = "norm1901-2000") |>
  select(-parameter, -stat) |>
  pivot_wider(names_from = param, values_from = mlyprcp)


tmax19norm <- rast("../data/NRCC/tmax-1901_2000-monthly-normals-v1.0.nc")
tmax <- terra::extract(tmax19norm, netn_cent[,c("long", "lat")])
tmax$UnitCode = netn_cent$UnitCode
tmax_long <- tmax |> select(UnitCode, mlytmax_norm_1:mlytmax_norm_12, mlytmax_std_1:mlytmax_std_12) |>
  pivot_longer(-UnitCode, names_to = "parameter", values_to = "mlytmax") |>
  mutate(month = as.numeric(gsub("\\D", "", parameter)),
         stat = ifelse(grepl("norm", parameter), "norm", "std"),
         param = paste0("tmax_", stat),
         norm = "norm1901-2000") |>
  select(-parameter, -stat) |>
  pivot_wider(names_from = param, values_from = mlytmax)
head(tmax_long)

tmin19norm <- rast("../data/NRCC/tmin-1901_2000-monthly-normals-v1.0.nc")
tmin <- terra::extract(tmin19norm, netn_cent[,c("long", "lat")])
tmin$UnitCode = netn_cent$UnitCode
tmin_long <- tmin |> select(UnitCode, mlytmin_norm_1:mlytmin_norm_12, mlytmin_std_1:mlytmin_std_12) |>
  pivot_longer(-UnitCode, names_to = "parameter", values_to = "mlytmin") |>
  mutate(month = as.numeric(gsub("\\D", "", parameter)),
         stat = ifelse(grepl("norm", parameter), "norm", "std"),
         param = paste0("tmin_", stat),
         norm = "norm1901-2000") |>
  select(-parameter, -stat) |>
  pivot_wider(names_from = param, values_from = mlytmin)
head(tmin_long)

tavg19norm <- rast("../data/NRCC/tavg-1901_2000-monthly-normals-v1.0.nc")
tavg <- terra::extract(tavg19norm, netn_cent[,c("long", "lat")])
tavg$UnitCode = netn_cent$UnitCode
tavg_long <- tavg |> select(UnitCode, mlytavg_norm_1:mlytavg_norm_12, mlytavg_std_1:mlytavg_std_12) |>
  pivot_longer(-UnitCode, names_to = "parameter", values_to = "mlytavg") |>
  mutate(month = as.numeric(gsub("\\D", "", parameter)),
         stat = ifelse(grepl("norm", parameter), "norm", "std"),
         param = paste0("tavg_", stat),
         norm = "norm1901-2000") |>
  select(-parameter, -stat) |>
  pivot_wider(names_from = param, values_from = mlytavg)


norm19 <- list(ppt_long, tmax_long, tmin_long, tavg_long)
comb19 <- reduce(norm19, full_join, by = c("UnitCode", "month", "norm"))
head(comb19)
head(comb30)

comb_norms <- full_join(comb19 |> select(-norm), comb30 |> select(-norm),
                        by = c("UnitCode", "month"),
                        suffix = c("_1901_2000", "_1991_2020"))
head(comb_norms)

NETN_clim_norms <- left_join(netn_cent, comb_norms, by = "UnitCode")

#NETN_clim_norms <- read.csv("../data/NETN_clim_norms.csv")
#NETN_clim_norms <- NETN_clim_norms |> rename(UnitCode = UNIT_CODE)

# data("NETN_clim_norms")
# NETN_clim_norms <- NETN_clim_norms |> rename(UnitCode = UNIT_CODE)
write.csv(NETN_clim_norms, "../data/NETN_clim_norms.csv")
usethis::use_data(NETN_clim_norms, overwrite = T)

# Add temps as F and precip as "in"
data("NETN_clim_norms")
c_to_f <- function(x){
  (x * 9/5) + 32
}

mm_to_in <- function(x){
  (x / 25.4)
}

NETN_clim_norms <- NETN_clim_norms |> mutate(across(.cols = starts_with("t"),
                                                    .fns = ~c_to_f(.x),
                                                    .names = "{.col}_F"))

NETN_clim_norms <- NETN_clim_norms |> mutate(across(.cols = starts_with("precip"),
                                                    .fns = ~mm_to_in(.x),
                                                    .names = "{.col}_in"))

usethis::use_data(NETN_clim_norms, overwrite = T)
