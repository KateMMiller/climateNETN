#' @title getClimWStat: Download daily data from nearest weather station
#'
#' @description This function downloads daily weather data from data.rcc-acis.org from the nearest
#' weather station with the longest period of record dating through 2024 for each selected NETN park
#' based on its lat/long coordinates. Data include daily total precipitation (mm),max. temperature (C),
#' min temperature (C) and binds each site's data into a single data frame. Final data frame
#' can also be written to disk (export = T). If downloading for multiple parks and multiple years, function
#' may be slow, particularly ACAD. Note that the McFarland Hill weather station precip. data is not available
#' through typical weather station sources. Instead, hourly precip. data from NADP are downloaded and summarized
#' to daily value. ACAD NADP Precip data only go back to 2008.
#'
#' @importFrom dplyr arrange filter full_join left_join select
#' @importFrom purrr map list_rbind reduce
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
#' @param years Vector of years to download weather station data for, will start with 01/01/year and end with 12/31/year.
#' Note that not all weather stations have complete a complete period of record from 2006 to current.
#'
#' @param export Logical. If TRUE, will export a CSV of the compiled weather data with a date stamp. Must supply
#' a filepath to write output to. If FALSE (Default), will only return results to R environment.
#'
#' @param filepath Quoted path to save files to. If not specified, will save to working directory.
#'
#' @return Data frame of weather station daily climate data for each specified park.
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(climateNETN)
#' importData()
#'
#' # get weather data for ROVA from 2020-2023
#' rova <- getClimWStat(park = "ROVA", years = 2020:2023)
#'
#' # get weather data for 2023 LNETN parks
#' lnetn <- getClimWStat(park = "LNETN", years = 2023)
#'
#'}
#'
#' @export

getClimWStat <- function(park = "all",
                         years = c(2006:2023),
                         filepath = NA,
                         export = FALSE){

  #--- error handling ---
  # Check that suggested package required for this function are installed
  if(!requireNamespace("jsonlite", quietly = TRUE)){
    stop("Package 'jsonlite' needed to download weather station data. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("httr", quietly = TRUE)){
    stop("Package 'httr' needed to download weather station data. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("XML", quietly = TRUE)){
    stop("Package 'XML' needed to download weather station data. Please install it.", call. = FALSE)
  }

  stopifnot(class(years) %in% c("numeric", "integer"))
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "all")){park = c("ACAD", "BOHA", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  stopifnot(class(years) %in% c("numeric", "integer"))

  if(export == TRUE){
    if(is.na(filepath)){stop(paste0("Must specify a filepath to the database when export = TRUE"))
    } else if(!file.exists(filepath)){
      stop("Specified file path does not exist.")}

    if(!grepl("/$", filepath)){filepath <- paste0(filepath, "/")} # add / to end of filepath if doesn't exist
  }

  # Test connection to API
  if(httr::http_error("http://data.rcc-acis.org")){
    stop("Unable to connect to data.rcc-acis.org to download weather station data.")}

  #--- compile data ---
  # Combine sites with nearest weather station
  # sites <- force(getSites(park = park, site = site, site_type = site_type, active = active)) |>
  #   select(SiteCode, SiteName, SiteLatitude, SiteLongitude, UnitCode)
  #
  # site_list <- unique(sites$SiteCode)

  data("closest_WS")

  # Drop smaller subunits and filter only selectd parks.
  parks_ws1 <- closest_WS[!closest_WS$SubUnit %in% c("IAH", "SCH", "NJB"),]
  parks_ws <- unique(parks_ws1[parks_ws1$UnitCode %in% park,])

  startdate <- paste0(min(years), "-01-01")
  enddate <- paste0(max(years), "-12-31")

  # function to downoad precip and temp weather station data from rcc-acis
  get_wdat <- function(field, stn, reduce = c("sum", "mean"), park){#, site){

  urlbase <- "http://data.rcc-acis.org/StnData?params=%s"
    elem_template <- function(field) {
      list(name = field,
           interval = "dly",
           duration = "dly")}

    params <-
      list(sid   = stn,
           sdate = startdate, #"por", # change to follow years
           edate = enddate,#"por", # change to follow years
           elems = lapply(field, elem_template))

    query <- sprintf(urlbase, URLencode(jsonlite::toJSON(params, auto_unbox = TRUE)))

    response <- httr::GET(query) |> httr::content(as = "text", encoding = "UTF-8") |> jsonlite::fromJSON()

    wdat <-
      response$data |> as.data.frame(stringsAsFactors = FALSE)

    names(wdat) <- c("Date", field)
    wdat[,field] <- gsub("A", "", wdat[,field])
    wdat[,field][wdat[,field] == "T"] <- 0
    wdat[,field][wdat[,field] %in% c("M", "S")] <- NA_real_ # replace M with NA for missing data
    wdat[,field] <- as.numeric(wdat[,field])

    UnitCode <- rep(park, nrow(wdat))
    wdat2 <- cbind(UnitCode, wdat)

    return(wdat2)
  }

  park_list <- parks_ws$UnitCode

  if(length(park_list) > 1){
  ws_comb <- purrr::map(park_list,

    function(x){
      station = parks_ws$id[parks_ws$UnitCode == x]
      parkc = x

      if(!parkc == "ACAD"){ # no precip data for MCCF
      pcp_in <- get_wdat('pcpn',
                         stn = station,
                         park = parkc)
      tmax_f <- get_wdat('maxt',
                         stn = station,
                         park = parkc)
      tmin_f <- get_wdat('mint',
                         stn = station,
                         park = parkc)

    ws_ls <- list(pcp_in, tmax_f, tmin_f)
    ws_comb1 <- reduce(ws_ls, full_join, by = c("UnitCode", "Date"))
    # print(head(data.frame(ws_comb1)))
    # print(dim(ws_comb1))
    return(data.frame(ws_comb1))

    } else if(parkc == "ACAD"){
    tmax_f <- get_wdat('maxt',
                       stn = station,
                       park = parkc)
    tmin_f <- get_wdat('mint',
                       stn = station,
                       park = parkc)

    ws_ls <- list(tmax_f, tmin_f)
    ws_comb1 <- purrr::reduce(ws_ls, full_join, by = c("UnitCode", "Date"))
    ws_comb1$pcp_in <- NA_real_
    return(ws_comb1)
    }
  }) |> list_rbind()
  } else {
    station = parks_ws$id
    parkc = parks_ws$UnitCode
    if(any(!parkc %in% "ACAD")){ # no precip data for MCCF
      pcp_in <- get_wdat('pcpn',
                         stn = station,
                         park = parkc) # not available for SARA's nearest WS
      tmax_f <- get_wdat('maxt',
                         stn = station,
                         park = parkc)
      tmin_f <- get_wdat('mint',
                         stn = station,
                         park = parkc)

      ws_ls <- list(pcp_in, tmax_f, tmin_f)
      ws_comb <- reduce(ws_ls, full_join, by = c("UnitCode", "Date"))
    } else {
      tmax_f <- get_wdat('maxt',
                         stn = station,
                         park = parkc)
      tmin_f <- get_wdat('mint',
                         stn = station,
                         park = parkc)

      ws_ls <- list(tmax_f, tmin_f)
      ws_comb <- reduce(ws_ls, full_join, by = c("UnitCode", "Date"))
      ws_comb$pcp_in <- NA_real_
    }
  }

  # ACAD McFarland WS doesn't include precip. on any site but NADP. Downloading from NADP
  # park_list <- sort(unique(sites_ws$UnitCode))

  if(any(park_list %in% "ACAD")){
  year_start <- ifelse(min(years) < 2008, 2008, min(years)) # dataset doesn't start until 2008
  start_date <- paste0("01/01/", year_start)
  end_date <- paste0("12/31/", max(years))
  stationID <- "ME98"

  # daily precip
  PrecipURL <- paste0("http://nadp2.slh.wisc.edu/siteOps/ppt/Data.aspx?id=", stationID, "&stdate=", start_date,
                      "T14:05&endate=", end_date, "T14:00&plot_sel=1000110&data_sel1=D&data_sel2=110&sel2_count=2&offset_txt=-5")

  precip_tbl <- XML::readHTMLTable(PrecipURL, header=T, as.data.frame = T,
                                   stringsAsFactors = F)$GridView_data[,c('Date', 'Precip (in)')]
  precip_tbl$precip_in <- as.numeric(precip_tbl$`Precip (in)`)

  precip_tbl$precip_mm <- precip_tbl$precip_in * 25.4
  precip_tbl$UnitCode <- "ACAD"
  precip_ACAD <- precip_tbl[,c("UnitCode", "Date", "precip_mm")]
  }

  ws_comb2 <-
  if(any(park_list %in% "ACAD")){
    left_join(ws_comb, precip_ACAD, by = c("UnitCode", "Date"))
  } else {ws_comb}

  ws_comb2$ws_pcpmm <- ifelse(ws_comb2$UnitCode %in% "ACAD",
                              round(ws_comb2$precip_mm, 3), round(ws_comb2$pcpn * 25.4, 3))
  ws_comb2$ws_tmaxc <- round((ws_comb2$maxt - 32) * (5/9), 3)
  ws_comb2$ws_tminc <- round((ws_comb2$mint - 32) * (5/9), 3)
  ws_comb2$year <- as.numeric(substr(ws_comb2$Date, 1, 4))
  ws_comb2$month <- as.numeric(substr(ws_comb2$Date, 6, 7))
  ws_comb2$doy <- as.numeric(strftime(ws_comb2$Date, format = "%j"))
  ws_comb3 <- ws_comb2 |> filter(year %in% years) |>
    select(UnitCode, Date, year, month, doy, ws_tmaxc, ws_tminc, ws_pcpmm)

  ws_final <- left_join(ws_comb3, parks_ws, by = "UnitCode") |> #, relationship = "many-to-many") |>
    select(UnitCode, UnitName, Date, year, month, doy, ws_id = id,
           ws_lat, ws_long, ws_dist_km, ws_tmaxc, ws_tminc, ws_pcpmm) |>
    arrange(UnitCode, Date)

  ws_final$ws_dist_km <- round(ws_final$ws_dist_km, 3)

  if(export == TRUE){write.csv(ws_final,
                               paste0(filepath, "Weather_station_data_", min(years), "-", max(years), ".csv"),
                               row.names = F)}
  return(data.frame(ws_final))

}
