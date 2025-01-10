#' @title getClimDaymet: Download daily Daymet gridded climate data directly from REST
#'
#' @description This function downloads daily gridded climate data directly from Daymet REST for
#' each NETN park centroid based on its lat/long coordinates, and binds results into a single data frame.
#' If downloading for all parks and multiple years, function may be slow. The returned data frame
#' includes Day length (dayl in s/day), Precipitation (prcp in mm/day), Shortwave radiation
#' (srad in W/m2), Snow water equivalent (swe in kg/m2), Maximum air temperature (tmax in C),
#' Minimum air temperature (tmin in C), and Water vapor pressure (vp in Pascals). More details on
#' metrics can be found online: https://daymet.ornl.gov/overview.html > Parameters, Parameter
#' abbreviations, Units and Descriptions. Note that occasionally you're unable to connect to the
#' server, and will receive an error message when that happens.
#'
#' @importFrom purrr list_rbind pmap
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
#' @param years Vector of years to download Daymet data. Earliest available year is 1980. Latest is
#' currently 12/31/2023.
#'
#' @return Data frame of Daymet daily climate data for each specified park.
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(climateNETN)
#'
#' # download for MORR 2023 only
#' morr <- getClimDaymet(park = "MORR", years = 2023)
#'
#' # download for ACAD from 1980:2023
#' acad <- getClimDaymet(park = "ACAD", years = 1980:2023)
#'
#'}
#'
#' @export

getClimDaymet <- function(park = "all",
                              years = c(2006:2023)){
  #--- error handling ---
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "BOHA", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  if(any(park == "all")){park = c("ACAD", "BOHA", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")}
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 1980)

  # Check that suggested package required for this function are installed
  if(!requireNamespace("httr", quietly = TRUE)){
    stop("Package 'httr' needed to download weather station data. Please install it.", call. = FALSE)
  }

  #--- compile data ---
  # Create list of lat/longs to generate
  data("closest_WS")
  sites1 <- subset(closest_WS, !SubUnit %in% c("IAH", "SCH", "NJB"))
  sites <- sites1[, c("UnitCode", "park_lat", "park_long")]
  colnames(sites) <- c("site", "lat", "long")
  # dropping smaller subunits for now

  test <- httr::POST("https://daymet.ornl.gov/single-pixel/api/data?")$status_code
  if(test %in% c(503, 404)){stop("Unable to reach Daymet Server.")}

  # Function to download one site
  getdaym <- function(unitcode, lat, long, years){
    # set up url
    year_range <- paste0(years, collapse = ",")
    daym_server <- "https://daymet.ornl.gov/single-pixel/api/data?"
    daym_url <- paste0(daym_server,
                       "lat=", lat,
                       "&lon=", long,
                       #"&vars=tmax,tmin,dayl,prcp,srad,swe,vp",
                       "&years=", year_range)
    # Download from server
    dat <- httr::GET(daym_url)
    # Turn into dataframe
    dat2 <- httr::content(dat, as = "text", encoding = "UTF-8")
    header_start <- unlist(gregexpr("year,yday", dat2))
    tile_pos <- unlist(gregexpr("Tile: ", dat2)) + 5
    tile <- as.numeric(substr(dat2, tile_pos, tile_pos + 6)) #12116
    alt_pos <- unlist(gregexpr("Elevation: " , dat2)) + 11
    altitude <- as.numeric(gsub("[[:alpha:]]|", "", (substr(dat2, alt_pos, alt_pos + 5))))
    dat3 <- substr(dat2, header_start, nchar(dat2))
    dat4 <- read.table(textConnection(dat3), sep = ",", header = T)
    colnames(dat4) <- gsub("\\.\\.", "_", names(dat4))
    colnames(dat4) <- gsub("\\.", "", names(dat4))
    dat4$Latitude = lat
    dat4$Longitude = long
    dat4$UnitCode = unitcode
    dat4$dm_tile = tile
    dat4$altitude = altitude
    dat4$Date <- as.Date(dat4$yday, origin = paste0(dat4$year, "-01-01"), format = "%Y-%m-%d")
    dat4 <- dat4[, c("UnitCode", "dm_tile", "Latitude", "Longitude", "altitude",
                     "Date", "year", "yday",
                     "dayl_s", "prcp_mmday", "srad_Wm2", "swe_kgm2", "tmax_degc",
                     "tmin_degc", "vp_Pa")]

    newnames <- c(names(dat4[, 1:8]),
                  paste0("dm_", names(dat4[, 9:ncol(dat4)])))
    colnames(dat4) <- newnames
    dat4
  }

  # Iterate to download all sites
  site_list <- list(sites$site, sites$lat, sites$long)
  yrs = years

  comb_daym <- pmap(site_list, function(unitcode, lat, long, yrs){
    getdaym(unitcode, lat, long, years)
  }) |> list_rbind()
  return(comb_daym)

}
