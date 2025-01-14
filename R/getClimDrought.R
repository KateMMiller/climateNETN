#' @title getClimDrought: Download weekly county-level drought index
#'
#' @description This function downloads weekly drought index at the county level based on the U.S. Drought Monitor
#' for each specified park. If downloading for multiple parks and multiple weeks/years, function may be slow.
#' Returned data frame includes percent of county area in 5 levels of drought, with D0 = Abnormally Dry, D1 =
#' Moderate Drought, D2 = Severe Drought, D3 = Extreme Drought, and D4 = Exceptional Drought. Also returned is the
#' Drought Severity and Coverage Index, which ranges from 0 to 500, and is a weighted sum of area within each of
#' the drought categories. A score of 500 indicates the entire area is in exceptional drought (D4). Where multiple
#' counties occur in a park, data can be faceted by county if dom_county = FALSE. To only plot predominant county,
#' specify dom_county = TRUE (default). Otherwise, only park-level data are returned, as the results won't vary
#' by site within a park. More info on the data are at #' https://droughtmonitor.unl.edu/CurrentMap.aspx.
#' Note that Drought Monitor reports data such that, when drought a level moves into a higher category (e.g. D1 = 10),
#' the previous level (e.g. D0) is listed as 100. The columns with pct (e.g. D0pct) correct for that, so that
#' D0 + ... + D05 + None = 100.
#'
#' @importFrom dplyr case_when filter full_join mutate
#' @importFrom purrr list_rbind map
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
#' @param years Vector of years to download drought index for, will start with 01/01/year and end with 12/31/year.
#'
#' @param week_start Quoted start of week formatted as "mm/dd/yyyy". If specified, will return drought index for
#' given week. If blank, will return drought index for all weeks available in specified years. Note that weekly
#' data are available every Tuesday. If the week_start specified is not a Tuesday, code will shift the date to the
#' Tuesday of the specified start week.
#'
#' @param end_date Quoted end date of query. If blank (Default), will return the most recent week of data available.
#'
#' @param dom_county Logical. If TRUE (Default) only plots predominant county if park covers multiple counties.
#' If FALSE, facets on county.
#'
#' @return Data frame of weekly county drought data for each park.
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(waterNETN)
#' importData()
#'
#' # Get drought info for MABI and SAGA sites for first week of May
#' mabisaga <- getClimDrought(park = c("MABI", "SAGA"), week_start = "05/01/2024")
#'
#' # Get drought info for MABI from 2020:2024
#' mabi5 <- getClimDrought(park = "MABI", years = 2020:2024)
#'
#' # Get drought info for both counties in MORR in 2023
#' morr <- getClimDrought(park = "MORR", years = 2023, dom_county = TRUE)
#'
#'}
#'
#' @export

getClimDrought <- function(park = "all",
                           years = c(2006:2023), week_start = NA, end_date = NA,
                           dom_county = TRUE){

  #--- error handling ---
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "BOHA", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "all")){park = c("ACAD", "BOHA", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")}
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 1980)
  stopifnot(class(dom_county) == "logical")
  if(!is.na(week_start)){
    date_check <- as.Date(week_start, format = "%m/%d/%Y")
    if(is.na(date_check)){stop("Wrong week_start format specified. Must be formatted as 'mm/dd/yyyy'.")}
  }
  if(!is.na(end_date)){
    date2_check <- as.Date(end_date, format = "%m/%d/%Y")
    if(is.na(date2_check)){stop("Wrong end_date format specified. Must be formatted as 'mm/dd/yyyy'.")}
  }

  # Check that suggested package required for this function are installed
  if(!requireNamespace("httr", quietly = TRUE)){
    stop("Package 'httr' needed to download weather station data. Please install it.", call. = FALSE)
  }

  # Check that suggested package required for this function are installed
  if(!requireNamespace("jsonlite", quietly = TRUE)){
    stop("Package 'jsonlite' needed to download weather station data. Please install it.", call. = FALSE)
  }

  data("closest_WS", package = "climateNETN")

  # Set up start and end dates and FIPS based on specified arguments
  current_year <- format(Sys.Date(), "%Y")
  stopifnot(max(years) <= current_year) # bug handling

  start_day <-
    if(is.na(week_start)){
      format(as.Date(paste0("01/01/", min(years)), format = "%m/%d/%Y"), "%m/%d/%Y")
    } else if(!is.na(week_start)){
        format(as.Date(week_start, format = "%m/%d/%Y"), "%m/%d/%Y")
    }

  dow_shift <- as.POSIXlt(start_day, format = "%m/%d/%Y")$wday

  start_tues <- if(dow_shift > 3){
    format(as.Date(start_day, format = "%m/%d/%Y") - dow_shift + 2, "%m/%d/%Y")
  } else if(dow_shift < 3){
    format(as.Date(start_day, format = "%m/%d/%Y") + dow_shift, "%m/%d/%Y")
    } else {format(as.Date(start_day, format = "%m/%d/%Y"), "%m/%d/%Y")}

  end_day <-
    if(is.na(end_date)){
    if(max(years) == current_year & is.na(week_start)){
      format(Sys.Date(), "%m/%d/%Y")
    } else if(max(years) < current_year & is.na(week_start)){
      format(as.Date(paste0("12/31/", max(years)), format = "%m/%d/%Y"), "%m/%d/%Y")
    } else if(!is.na(week_start)){format(as.Date(start_tues, format = "%m/%d/%Y") + 6, "%m/%d/%Y")}
    } else {
      format(as.Date(end_date, format = "%m/%d/%Y"), format = "%m/%d/%Y")
    }

  # Set up aoi and filters for iterations
  fips <- "ParkFIPS"
  aoi1 <- closest_WS |> filter(UnitCode %in% park) |>
                        mutate(#UnitCode = substr(SiteCode, 1, 4),
                               dom_county =
                                 case_when(UnitCode == "ACAD" & ParkCounty == "Knox County" ~ FALSE,
                                           UnitCode == "MORR" & ParkCounty == "Somerset County" ~ FALSE,
                                           TRUE ~ TRUE)) |>
                        unique()

  aoi2 <- if(dom_county == TRUE){filter(aoi1, dom_county == TRUE)} else {aoi1}
  aoi = unique(aoi2[,fips])
  area = "CountyStatistics"

  getDSCI <- function(aoi){
    park <- unique(closest_WS$UnitCode[closest_WS[,fips] == aoi])
    url_dsci <- paste0("https://usdmdataservices.unl.edu/api/CountyStatistics",
                       "/GetDSCI?aoi=", aoi,
                       "&startdate=", start_tues, "&enddate=", end_day,
                       "&statisticsType=1")

    dsci1 <- httr::GET(url_dsci)
    dsci <- suppressMessages(
      httr::content(dsci1, as = "parsed") |> as.data.frame()
    )
    #dsci <- read.table(textConnection(dsci2), sep = ",", header = T)
    #dsci <- jsonlite::fromJSON(dsci2) # DSCI ranges from 0 to 500 with 500 being most extreme drought
    dsci$UnitCode <- park
    return(dsci)
  }

  getDrought <- function(aoi){
    park <- unique(closest_WS$UnitCode[closest_WS[,fips] == aoi])
    url_drght <- paste0("https://usdmdataservices.unl.edu/api/CountyStatistics",
                        "/GetDroughtSeverityStatisticsByAreaPercent?aoi=", aoi,
                        "&startdate=", start_tues, "&enddate=", end_day,
                        "&statisticsType=1")

    drgt1 <- httr::GET(url_drght)
    drgt <- suppressMessages(
      httr::content(drgt1, as = "parsed") |> as.data.frame()
    )
    #drgt <- jsonlite::fromJSON(drgt2)
    #drgt <- read.table(textConnection(drgt2), sep = ",", header = T)
    drgt$UnitCode <- park
    return(drgt)
  }

  # Iterate if multiple sites
  dsci_full <- if(length(aoi) > 1){
    purrr::map(aoi, \(x) getDSCI(x)) |> purrr::list_rbind()
    } else {getDSCI(aoi)}

  drgt_full <- if(length(aoi) > 1){
    map(aoi, \(x) getDrought(x)) |> list_rbind()
  } else {getDrought(aoi)}

  drgt_comb <- full_join(drgt_full, dsci_full,
                         by = c("MapDate", "FIPS", "County", "State", "UnitCode")) |>
    data.frame()

  num_cols <- c("None", "D0", "D1", "D2", "D3", "D4", "DSCI")
  drgt_comb[,num_cols] <- as.numeric(unlist(drgt_comb[, num_cols]))

  drgt_comb$D0pct <- ifelse(drgt_comb$D1 > 0, drgt_comb$D0 - drgt_comb$D1, drgt_comb$D0)
  drgt_comb$D1pct <- ifelse(drgt_comb$D2 > 0, drgt_comb$D1 - drgt_comb$D2, drgt_comb$D1)
  drgt_comb$D2pct <- ifelse(drgt_comb$D3 > 0, drgt_comb$D2 - drgt_comb$D3, drgt_comb$D2)
  drgt_comb$D3pct <- ifelse(drgt_comb$D4 > 0, drgt_comb$D3 - drgt_comb$D4, drgt_comb$D3)
  drgt_comb$D4pct <- drgt_comb$D4

  return(drgt_comb)
  }
