#' @title getClimNOAA: Download monthly gridded NOAA climate data
#'
#' @description This function downloads monthly gridded climate data directly from NOAA's National Centers
#' for Environmental Information (www.ncei.noaa.gov) for the centroid of each park in NETN. The returned
#' data frame includes total precipitation (prcp in mm/day), Maximum air temperature (tmax in C) averaged
#' over the month, minimum air temperature (tmin in C) averaged over the month, and average temperature
#' (tavg in C). Note that occasionally you're unable to connect to the server, and will receive an error message
#' when that happens. This function was used to compile historic weather data for each park from 1895 up
#' through May 2024, which is stored as a package dataset called NETN_clim_annual (accessed via
#' \code{data("NETN_clim_annual")}). You should only need this function to download newer months of data
#' as they become available.
#'
#' @importFrom dplyr full_join
#' @importFrom purrr list_rbind map reduce
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"LNETN"}{Includes all parks but ACAD monitored for water quality.}
#' \item{"ACAD"}{Acadia NP only}
#' \item{"BOHA"}{Boston Harbor only}
#' \item{"MABI"}{Marsh-Billings-Rockefeller NHP only}
#' \item{"MIMA"}{Minute Man NHP only}
#' \item{"MORR"}{Morristown NHP only}
#' \item{"ROVA"}{Roosevelt-Vanderbilt NHS only}
#' \item{"SAGA"}{Saint-Gaudens NHP only}
#' \item{"SAIR"}{Saugus Iron Works NHS only}
#' \item{"SARA"}{Saratoga NHP only}
#' \item{"WEFA"}{Weir Farm NHP only}}
#'
#' @param year 4-digit year to query. Earliest available year is 1895, and latest is 2024. Currently can only
#' handle 1 year at a time.
#'
#' @param months Vector of numeric months to query. Typically there's about a 6 week delay in monthly data
#' availability.
#'
#' @return Data frame of Daymet daily climate data for each specified site.
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(climateNETN)
#'
#' # get weather data for January - May 2024 for all NETN (will take a bit to download)
#' getClimNOAA(months = 1:5, years = 2024)
#'
#' # get weather data for May 2024 for ROVA
#' getClimNOAA(park = "ROVA", months = 5)
#'
#' # get weather data for all of 2023 in LNETN
#' getClimNOAA(park = "LNETN", year = 2023, months = 1:12)
#'
#'}
#'
#' @export

getClimNOAA <- function(park = 'all', year = as.integer(format(Sys.Date(), "%Y")), months = 5){
  #--- error handling ---
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  stopifnot(class(year) %in% c("numeric", "integer"), year >= 1895)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))

  if(!requireNamespace("raster", quietly = TRUE)){
    stop("Package 'raster' needed to download NOAA gridded climate data. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("sf", quietly = TRUE)){
    stop("Package 'sf' needed to download NOAA gridded climate data. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("ncdf4", quietly = TRUE)){
    stop("Package 'ncdf4' needed to download NOAA gridded climate data. Please install it.", call. = FALSE)
  }

  #mos <- sprintf("%02d", months)

  #--- compile data ---
  # Create list of lat/longs to generate
  data("NETN_centroids")

  cent <- if(any(park == "all")){NETN_centroids
    } else {NETN_centroids[NETN_centroids$UnitCode %in% park,]}

  # bounding box to crop before extract to speed up function
  NETN_bbox <- data.frame(lat = c(47.38, 44.80, 38.71, 43.40),#, 39.994187),
                          long = c(-68.71, -66.67, -74.84, -75.54)) |> #, -80.521832)) |>
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |> sf::st_bbox()

  #"https://noaa-nclimgrid-monthly-pds.s3.amazonaws.com/202404.prcp.conus.pnt"

  getnoaa <- function(yr, mon){
    url_base <- "https://www.ncei.noaa.gov/thredds/dodsC/data-in-development/nclimgrid/nclimgrid_"
    url_prcp <- paste0(url_base, "prcp", ".nc")
    url_tavg <- paste0(url_base, "tavg", ".nc")
    url_tmax <- paste0(url_base, "tmax", ".nc")
    url_tmin <- paste0(url_base, "tmin", ".nc")

    band <- (yr - 1895)*12 + mon

    prcp <- tryCatch(raster::raster(url_prcp, band = band),
                     error = function(e){stop(paste0(e))})
    prcp_crop <- raster::crop(prcp, NETN_bbox)
    netn_prcp <- cbind(cent, prcp = raster::extract(prcp_crop, cent[,c("long", "lat")]))

    tmax <- raster::raster(url_tmax, band = band)
    tmax_crop <- raster::crop(tmax, NETN_bbox)
    netn_tmax <- cbind(cent, tmax = raster::extract(tmax_crop, cent[,c("long", "lat")]))

    tmin <- raster::raster(url_tmin, band = band)
    tmin_crop <- raster::crop(tmin, NETN_bbox)
    netn_tmin <- cbind(cent, tmin = raster::extract(tmin_crop, cent[,c("long", "lat")]))

    tavg <- raster::raster(url_tavg, band = band)
    tavg_crop <- raster::crop(tavg, NETN_bbox)
    netn_tavg <- cbind(cent, tavg = raster::extract(tavg_crop, cent[,c("long", "lat")]))

    clim_list <- list(netn_prcp, netn_tmax, netn_tmin, netn_tavg)
    netn_comb <- reduce(clim_list, full_join, by = c("UnitCode", "UnitName", "long", "lat"))

    netn_comb$year = yr
    netn_comb$month = as.numeric(mon)

    data.frame(netn_comb)
    }

  netn_final <- #if(length(months) > 1){
    purrr::map(months, function(x){
      tryCatch(getnoaa(yr = year, mon = x),
               error = function(e){NULL})}) |> list_rbind()
  # } else {tryCatch(getnoaa(yr = year, mon = months),
  #                    error = function(e){NULL})}

  if(nrow(netn_final) > 0){return(netn_final)} else {return(NULL)}
}
