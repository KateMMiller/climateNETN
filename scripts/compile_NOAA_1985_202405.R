#---------------------
# Script compiled monthly gridded climate data for each park NETN_centroidsroid in NETN 1895 through 2024-05
#--------------------
# iterate through years and months through 12/31/2005 to add historical context
# To save time, I downloaded the full files from
#   https://www.ncei.noaa.gov/thredds/catalog/data-in-development/nclimgrid/catalog.html
# via their https option, then extracted the data below.

library(tidyverse)
#library(waterNETN)
library(sf)
library(raster)

NETN_bbox <- data.frame(lat = c(47.38, 44.80, 38.71, 43.40, 39.994187),
                        long = c(-68.71, -66.67, -74.84, -75.54, -80.521832)) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> st_bbox()

data("NETN_centroids")

compnoaa <- function(yr, mon){
  bnd <- (yr - 1895) * 12 + mon
  prcp <- raster("../data/ncei/nclimgrid_prcp.nc", band = bnd)
  prcp_crop <- raster::crop(prcp, NETN_bbox)
  netn_prcp <- cbind(NETN_centroids, prcp = raster::extract(prcp_crop, NETN_centroids[,c("long", "lat")]))

  tmax <- raster("../data/ncei/nclimgrid_tmax.nc", band = bnd)
  tmax_crop <- raster::crop(tmax, NETN_bbox)
  netn_tmax <- cbind(NETN_centroids, tmax = raster::extract(tmax_crop, NETN_centroids[,c("long", "lat")]))

  tmin <- raster("../data/ncei/nclimgrid_tmin.nc", band = bnd)
  tmin_crop <- raster::crop(tmin, NETN_bbox)
  netn_tmin <- cbind(NETN_centroids, tmin = raster::extract(tmin_crop, NETN_centroids[,c("long", "lat")]))

  tavg <- raster("../data/ncei/nclimgrid_tavg.nc", band = bnd)
  tavg_crop <- raster::crop(tavg, NETN_bbox)
  netn_tavg <- cbind(NETN_centroids, tavg = raster::extract(tavg_crop, NETN_centroids[,c("long", "lat")]))

  clim_list <- list(netn_prcp, netn_tmax, netn_tmin, netn_tavg)
  netn_comb <- reduce(clim_list, full_join, by = c("UnitCode", "UnitName", "long", "lat"))

  netn_comb$year = yr
  netn_comb$month = as.numeric(mon)

  return(data.frame(netn_comb))
}

years <- 1895:2023
months <- 1:12
yrmon <- expand.grid(year = years, mon =  months)

netn_hist <- map2(yrmon$year, yrmon$mon, function(x, y){
    compnoaa(yr = x, mon = y)}, .progress = T) |> list_rbind()

mon24 = 1:5
netn_2024 <- map(mon24, function(x){
  compnoaa(yr = 2024, mon = x)}, .progress = T) |> list_rbind()

netn_final <- rbind(netn_hist, netn_2024)

NETN_clim_annual <- netn_final
usethis::use_data(NETN_clim_annual, overwrite = T)

# Add temps as F and precip as "in"
data("NETN_clim_annual")
c_to_f <- function(x){
  (x * 9/5) + 32
}

mm_to_in <- function(x){
  (x / 25.4)
}
NETN_clim_annual <- NETN_clim_annual |> mutate(across(.cols = starts_with("t"),
                                                    .fns = ~c_to_f(.x),
                                                    .names = "{.col}_F"))

NETN_clim_annual <- NETN_clim_annual |> mutate(across(.cols = starts_with("prcp"),
                                                      .fns = ~mm_to_in(.x),
                                                      .names = "{.col}_in"))
usethis::use_data(NETN_clim_annual, overwrite = T)

