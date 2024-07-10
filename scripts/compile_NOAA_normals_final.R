#------------------------------------------------------------------------------------------
# Code to compile NOAA gridded climate normals for NETN parks based on each park's
# centroid. Note that ACAD and BOHA park centroids had to be adjusted to land in
# NOAA grids. Grids were downloaded to local machine to speed up the process.
#  # Normals were manually downloaded from:
#     "https://www.ncei.noaa.gov/products/land-based-station/us-climate-normals"
#  # Historic grids were downloaded from:
#     "https://www.ncei.noaa.gov/thredds/catalog/data-in-development/nclimgrid/catalog.html
#------------------------------------------------------------------------------------------

library(terra) # for normals
library(sf)
library(tidyverse)
path = "C:/NETN/R_Dev/climateNETN/data/"

data("NETN_centroids")
netn_sf <- st_as_sf(NETN_centroids, coords = c("long", "lat"), crs = 4326)
netn_bbox <- netn_sf |> st_buffer(10000) |> st_bbox() #10km buffer so ACAD not cutoff

#---- Extract NOAA Climate Normals for NETN parks ----
#------ 1991-2020 Normals ------
ppt30norm <- rast("../data/NRCC/prcp-1991_2020-monthly-normals-v1.0.nc")
ppt30 <- terra::extract(ppt30norm, NETN_centroids[,c("long", "lat")])
ppt30$UnitCode = NETN_centroids$UnitCode
ppt30_long <- ppt30 |> dplyr::select(UnitCode, mlyprcp_norm_1:mlyprcp_norm_12, mlyprcp_std_1:mlyprcp_std_12) |>
  pivot_longer(-UnitCode, names_to = "parameter", values_to = "mlyprcp") |>
  mutate(month = as.numeric(gsub("\\D", "", parameter)),
         stat = ifelse(grepl("norm", parameter), "norm", "std"),
         param = paste0("precip_", stat),
         norm = "norm1991-2020") |>
  dplyr::select(-parameter, -stat) |>
  pivot_wider(names_from = param, values_from = mlyprcp)

tmax30norm <- rast("../data/NRCC/tmax-1991_2020-monthly-normals-v1.0.nc")
tmax30 <- terra::extract(tmax30norm, NETN_centroids[,c("long", "lat")])
tmax30$UnitCode = NETN_centroids$UnitCode
tmax30_long <- tmax30 |> dplyr::select(UnitCode, mlytmax_norm_1:mlytmax_norm_12, mlytmax_std_1:mlytmax_std_12) |>
  pivot_longer(-UnitCode, names_to = "parameter", values_to = "mlytmax") |>
  mutate(month = as.numeric(gsub("\\D", "", parameter)),
         stat = ifelse(grepl("norm", parameter), "norm", "std"),
         param = paste0("tmax_", stat),
         norm = "norm1991-2020") |>
  dplyr::select(-parameter, -stat) |>
  pivot_wider(names_from = param, values_from = mlytmax)

tmin30norm <- rast("../data/NRCC/tmin-1991_2020-monthly-normals-v1.0.nc")
tmin30 <- terra::extract(tmin30norm, NETN_centroids[,c("long", "lat")])
tmin30$UnitCode = NETN_centroids$UnitCode
tmin30_long <- tmin30 |> dplyr::select(UnitCode, mlytmin_norm_1:mlytmin_norm_12, mlytmin_std_1:mlytmin_std_12) |>
  pivot_longer(-UnitCode, names_to = "parameter", values_to = "mlytmin") |>
  mutate(month = as.numeric(gsub("\\D", "", parameter)),
         stat = ifelse(grepl("norm", parameter), "norm", "std"),
         param = paste0("tmin_", stat),
         norm = "norm1991-2020") |>
  dplyr::select(-parameter, -stat) |>
  pivot_wider(names_from = param, values_from = mlytmin)

tavg30norm <- rast("../data/NRCC/tavg-1991_2020-monthly-normals-v1.0.nc")
tavg30 <- terra::extract(tavg30norm, NETN_centroids[,c("long", "lat")])
tavg30$UnitCode = NETN_centroids$UnitCode
tavg30_long <- tavg30 |> dplyr::select(UnitCode, mlytavg_norm_1:mlytavg_norm_12, mlytavg_std_1:mlytavg_std_12) |>
  pivot_longer(-UnitCode, names_to = "parameter", values_to = "mlytavg") |>
  mutate(month = as.numeric(gsub("\\D", "", parameter)),
         stat = ifelse(grepl("norm", parameter), "norm", "std"),
         param = paste0("tavg_", stat),
         norm = "norm1991-2020") |>
  dplyr::select(-parameter, -stat) |>
  pivot_wider(names_from = param, values_from = mlytavg)

norm30 <- list(ppt30_long, tmax30_long, tmin30_long, tavg30_long)
comb30 <- reduce(norm30, full_join, by = c("UnitCode", "month", "norm"))
head(comb30)

#------ 1901-2000 Normals ------
ppt19norm <- rast("../data/NRCC/prcp-1901_2000-monthly-normals-v1.0.nc")

ppt <- terra::extract(ppt19norm, NETN_centroids[,c("long", "lat")])
ppt$UnitCode = NETN_centroids$UnitCode
ppt_long <- ppt |> dplyr::select(UnitCode, mlyprcp_norm_1:mlyprcp_norm_12, mlyprcp_std_1:mlyprcp_std_12) |>
  pivot_longer(-UnitCode, names_to = "parameter", values_to = "mlyprcp") |>
  mutate(month = as.numeric(gsub("\\D", "", parameter)),
         stat = ifelse(grepl("norm", parameter), "norm", "std"),
         param = paste0("precip_", stat),
         norm = "norm1901-2000") |>
  dplyr::select(-parameter, -stat) |>
  pivot_wider(names_from = param, values_from = mlyprcp)

tmax19norm <- rast("../data/NRCC/tmax-1901_2000-monthly-normals-v1.0.nc")
tmax <- terra::extract(tmax19norm, NETN_centroids[,c("long", "lat")])
tmax$UnitCode = NETN_centroids$UnitCode
tmax_long <- tmax |> dplyr::select(UnitCode, mlytmax_norm_1:mlytmax_norm_12, mlytmax_std_1:mlytmax_std_12) |>
  pivot_longer(-UnitCode, names_to = "parameter", values_to = "mlytmax") |>
  mutate(month = as.numeric(gsub("\\D", "", parameter)),
         stat = ifelse(grepl("norm", parameter), "norm", "std"),
         param = paste0("tmax_", stat),
         norm = "norm1901-2000") |>
  dplyr::select(-parameter, -stat) |>
  pivot_wider(names_from = param, values_from = mlytmax)

tmin19norm <- rast("../data/NRCC/tmin-1901_2000-monthly-normals-v1.0.nc")
tmin <- terra::extract(tmin19norm, NETN_centroids[,c("long", "lat")])
tmin$UnitCode = NETN_centroids$UnitCode
tmin_long <- tmin |> dplyr::select(UnitCode, mlytmin_norm_1:mlytmin_norm_12, mlytmin_std_1:mlytmin_std_12) |>
  pivot_longer(-UnitCode, names_to = "parameter", values_to = "mlytmin") |>
  mutate(month = as.numeric(gsub("\\D", "", parameter)),
         stat = ifelse(grepl("norm", parameter), "norm", "std"),
         param = paste0("tmin_", stat),
         norm = "norm1901-2000") |>
  dplyr::select(-parameter, -stat) |>
  pivot_wider(names_from = param, values_from = mlytmin)

tavg19norm <- rast("../data/NRCC/tavg-1901_2000-monthly-normals-v1.0.nc")
tavg <- terra::extract(tavg19norm, NETN_centroids[,c("long", "lat")])
tavg$UnitCode = NETN_centroids$UnitCode
tavg_long <- tavg |> dplyr::select(UnitCode, mlytavg_norm_1:mlytavg_norm_12, mlytavg_std_1:mlytavg_std_12) |>
  pivot_longer(-UnitCode, names_to = "parameter", values_to = "mlytavg") |>
  mutate(month = as.numeric(gsub("\\D", "", parameter)),
         stat = ifelse(grepl("norm", parameter), "norm", "std"),
         param = paste0("tavg_", stat),
         norm = "norm1901-2000") |>
  dplyr::select(-parameter, -stat) |>
  pivot_wider(names_from = param, values_from = mlytavg)

norm19 <- list(ppt_long, tmax_long, tmin_long, tavg_long)
comb19 <- reduce(norm19, full_join, by = c("UnitCode", "month", "norm"))

comb_norms <- full_join(comb19 |> dplyr::select(-norm), comb30 |> dplyr::select(-norm),
                        by = c("UnitCode", "month"),
                        suffix = c("_1901_2000", "_1991_2020"))

NETN_clim_norms <- left_join(NETN_centroids, comb_norms, by = "UnitCode")
head(NETN_clim_norms)
names(NETN_clim_norms) <- gsub("precip", "ppt", names(NETN_clim_norms))
names(NETN_clim_norms) <- gsub("tavg", "tmean", names(NETN_clim_norms))

write.csv(NETN_clim_norms, "../data/NETN_clim_norms.csv")
#usethis::use_data(NETN_clim_norms, overwrite = T)

head(NETN_clim_norms)
#---- Compile error around normals ----
#library(raster) # for historic grids

compnoaa <- function(yr, mon){
  bnd <- (yr - 1895) * 12 + mon
  prcp <- raster::raster("../data/ncei/nclimgrid_prcp.nc", band = bnd)
  prcp_crop <- raster::crop(prcp, netn_bbox)
  netn_prcp <- cbind(NETN_centroids, prcp = raster::extract(prcp_crop, NETN_centroids[,c("long", "lat")]))

  tmax <- raster::raster("../data/ncei/nclimgrid_tmax.nc", band = bnd)
  tmax_crop <- raster::crop(tmax, netn_bbox)
  netn_tmax <- cbind(NETN_centroids, tmax = raster::extract(tmax_crop, NETN_centroids[,c("long", "lat")]))

  tmin <- raster::raster("../data/ncei/nclimgrid_tmin.nc", band = bnd)
  tmin_crop <- raster::crop(tmin, netn_bbox)
  netn_tmin <- cbind(NETN_centroids, tmin = raster::extract(tmin_crop, NETN_centroids[,c("long", "lat")]))

  tavg <- raster::raster("../data/ncei/nclimgrid_tavg.nc", band = bnd)
  tavg_crop <- raster::crop(tavg, netn_bbox)
  netn_tavg <- cbind(NETN_centroids, tavg = raster::extract(tavg_crop, NETN_centroids[,c("long", "lat")]))

  clim_list <- list(netn_prcp, netn_tmax, netn_tmin, netn_tavg)
  netn_comb <- purrr::reduce(clim_list, full_join, by = c("UnitCode", "UnitName", "long", "lat"))

  netn_comb$year = yr
  netn_comb$month = as.numeric(mon)

  return(data.frame(netn_comb))
}

#------ 1991-2020 Error------
years <- 1991:2020
months <- 1:12
yrmon <- expand.grid(year = years, mon =  months)

netn_annual <- purrr::map2(yrmon$year, yrmon$mon, function(x, y){
  compnoaa(yr = x, mon = y)}, .progress = T) |> list_rbind()

names(netn_annual) <- gsub("prcp", "ppt", names(netn_annual))
names(netn_annual) <- gsub("tavg", "tmean", names(netn_annual))
head(netn_annual)

netn_sum_1991_2020 <- netn_annual |> group_by(UnitCode, month) |>
  summarize(ppt_min_1991_2020 = min(ppt),
            ppt_max_1991_2020 = max(ppt),
            ppt_u95_1991_2020 = quantile(ppt, 0.975),
            ppt_l95_1991_2020 = quantile(ppt, 0.025),
            ppt_u50_1991_2020 = quantile(ppt, 0.75),
            ppt_l50_1991_2020 = quantile(ppt, 0.25),
            tmean_min_1991_2020 = min(tmean),
            tmean_max_1991_2020 = max(tmean),
            tmean_u95_1991_2020 = quantile(tmean, 0.975),
            tmean_l95_1991_2020 = quantile(tmean, 0.025),
            tmean_u50_1991_2020 = quantile(tmean, 0.75),
            tmean_l50_1991_2020 = quantile(tmean, 0.25),
            tmin_min_1991_2020 = min(tmin),
            tmin_max_1991_2020 = max(tmin),
            tmin_u95_1991_2020 = quantile(tmin, 0.975),
            tmin_l95_1991_2020 = quantile(tmin, 0.025),
            tmin_u50_1991_2020 = quantile(tmin, 0.75),
            tmin_l50_1991_2020 = quantile(tmin, 0.25),
            tmax_min_1991_2020 = min(tmax),
            tmax_max_1991_2020 = max(tmax),
            tmax_u95_1991_2020 = quantile(tmax, 0.975),
            tmax_l95_1991_2020 = quantile(tmax, 0.025),
            tmax_u50_1991_2020 = quantile(tmax, 0.75),
            tmax_l50_1991_2020 = quantile(tmax, 0.25),
            .groups = 'drop')

head(netn_sum_1991_2020)
#------ 1901-2000 Error------
years <- 1901:2000
months <- 1:12
yrmon <- expand.grid(year = years, mon =  months)

netn_annual <- purrr::map2(yrmon$year, yrmon$mon, function(x, y){
  compnoaa(yr = x, mon = y)}, .progress = T) |> list_rbind()

names(netn_annual) <- gsub("prcp", "ppt", names(netn_annual))
names(netn_annual) <- gsub("tavg", "tmean", names(netn_annual))
head(netn_annual)

netn_sum_1901_2000 <- netn_annual |> group_by(UnitCode, month) |>
  summarize(ppt_min_1901_2000 = min(ppt),
            ppt_max_1901_2000 = max(ppt),
            ppt_u95_1901_2000 = quantile(ppt, 0.975),
            ppt_l95_1901_2000 = quantile(ppt, 0.025),
            ppt_u50_1901_2000 = quantile(ppt, 0.75),
            ppt_l50_1901_2000 = quantile(ppt, 0.25),
            tmean_min_1901_2000 = min(tmean),
            tmean_max_1901_2000 = max(tmean),
            tmean_u95_1901_2000 = quantile(tmean, 0.975),
            tmean_l95_1901_2000 = quantile(tmean, 0.025),
            tmean_u50_1901_2000 = quantile(tmean, 0.75),
            tmean_l50_1901_2000 = quantile(tmean, 0.25),
            tmin_min_1901_2000 = min(tmin),
            tmin_max_1901_2000 = max(tmin),
            tmin_u95_1901_2000 = quantile(tmin, 0.975),
            tmin_l95_1901_2000 = quantile(tmin, 0.025),
            tmin_u50_1901_2000 = quantile(tmin, 0.75),
            tmin_l50_1901_2000 = quantile(tmin, 0.25),
            tmax_min_1901_2000 = min(tmax),
            tmax_max_1901_2000 = max(tmax),
            tmax_u95_1901_2000 = quantile(tmax, 0.975),
            tmax_l95_1901_2000 = quantile(tmax, 0.025),
            tmax_u50_1901_2000 = quantile(tmax, 0.75),
            tmax_l50_1901_2000 = quantile(tmax, 0.25),
            .groups = 'drop')

df_list <- list(NETN_clim_norms, netn_sum_1901_2000, netn_sum_1991_2020)
NETN_clim_comb1 <- purrr::reduce(df_list, left_join, by = c("UnitCode", "month"))

# dropping the std columns for now, though code above extracted them.
names(NETN_clim_comb)
NETN_clim_norms <- NETN_clim_comb1[,-grep("std", colnames(NETN_clim_comb1))]
usethis::use_data(NETN_clim_norms, overwrite = T)
