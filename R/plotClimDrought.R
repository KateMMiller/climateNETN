#' @include getClimDrought.R
#' @include theme_NETN.R
#'
#' @title plotClimDrought: Plots weekly drought based on county-level drought index
#'
#' @description This function plots weekly drought index at the county level for each specified park.
#' Resulting plot shows the percent of county area in 5 levels of drought, with D0 = Abnormally Dry,
#' D1 = Moderate Drought, D2 = Severe Drought, D3 = Extreme Drought, and D4 = Exceptional Drought.
#' Drought designations and color palette come from the U.S. Drought Monitor. If multiple parks are
#' specified, results will be faceted with a separate plot for each park. Where multiple counties
#' occur in a park, data can be faceted by county if dom_county = FALSE. To only plot predominant
#' county, specify dom_county = TRUE (default).
#'
#' @importFrom dplyr case_when filter left_join mutate select
#' @importFrom tidyr pivot_longer
#' @import ggplot2
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
#' @param years Vector of years to download drought index for, will start with the first Tuesday of the year
#' and end with the last Tuesday of the year specified. Defaults to current year.
#'
#' @param months Vector of months to download drought index for. Default = 1:12.
#'
#' @param dom_county Logical. If TRUE (Default) only plots predominant county if park covers multiple counties.
#' If FALSE, facets on county.
#'
#' @param end_date Quoted end date of query. If blank (Default), will return the most recent week of data available.
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position = "none". Other
#' options are "top", "bottom", "left", "right" (default).
#'
#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")
#'
#' @param x_pad Vector of 2 values. Specifies how much padding to add to x-axis on either side of axis.
#' Default is c(0, 0)
#'
#' @param plot_title Logical. If TRUE (default) prints site name at top of figure. If FALSE,
#' does not print site name. Only enabled when one site is selected.
#'
#' @param legend_row Integer. Specify number of rows to plot the legend. Default is 1.
#'
#' @return Returns a ggplot object of specified drought trends
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(waterNETN)
#' importData()
#'
#' # Plot drought info for MABI and SAGA for 2020 to 2024
#' plotClimDrought(park = c("MABI", "SAGA"), years = 2020:2024)
#'
#' # Plot drought info for both MORR counties for 2023
#' plotClimDrought(park = "MORR", years = 2023, dom_county = FALSE)
#'
#' # Plot drought info for ACAD counties in 2024
#' plotClimDrought(park = "ACAD", years = 2024, dom_county = FALSE)
#'}
#'
#' @export

plotClimDrought <- function(park = "all",
                            years = format(Sys.Date(), format = "%Y"),
                            months = 1:12, dom_county = TRUE, end_date = NA,
                            legend_position = 'right', plot_title = TRUE,
                            gridlines = 'none', x_pad = c(0, 0), legend_row = 1){

  #--- error handling ---
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "BOHA", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "all")){park = c("ACAD", "BOHA", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")}
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 1980)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(plot_title) == "logical")
  stopifnot(class(dom_county) == "logical")
  gridlines <- match.arg(gridlines, c("none", "grid_y", "grid_x", "both"))
  stopifnot(class(legend_row) %in% c("numeric", "integer"), legend_row > 0)
  stopifnot(length(x_pad) == 2)
  if(!is.na(end_date)){
    date_check <- as.Date(end_date, format = "%m/%d/%Y")
    if(is.na(date_check)){stop("Wrong end_date format specified. Must be formatted as 'mm/dd/yyyy'.")}
  }

  # Need to include park to get fips code
  ddata <- getClimDrought(park = park, years = years, dom_county = dom_county, end_date = end_date) |>
    mutate(dom_county = case_when(UnitCode == "ACAD" & County == "Knox County" ~ FALSE,
                                  UnitCode == "MORR" & County == "Somerset County" ~ FALSE,
                                  TRUE ~ TRUE)) |> unique() |>
    dplyr::select(-D0, -D1, -D2, -D3, -D4)

  # Take only dominant county if specified
  ddata2 <- if(dom_county == TRUE){filter(ddata, dom_county == TRUE)} else {ddata}
  ddata2$year <- format(as.Date(ddata2$ValidStart, "%Y-%m-%d"), "%Y")

  # Add column that's a proportion of county area

  ddata_long <- ddata2 |>
    filter(year %in% years) |>
    pivot_longer(cols = c(D0pct, D1pct, D2pct, D3pct, D4pct),
                 names_to = "Drought_Level", values_to = "Pct_Area")

  ddata_long$Date <- as.Date(ddata_long$ValidStart, format = "%Y-%m-%d")
  ddata_long$year <- format(as.Date(ddata_long$Date, format = "%Y-%m-%d"), "%Y")
  ddata_long$month <- as.numeric(format(as.Date(ddata_long$Date, format = "%Y-%m-%d"), "%m"))

  ddata3 <- ddata_long |>
    filter(month %in% months) |>
    mutate(drought_legend = case_when(Drought_Level == "D0pct" ~ "D0: Abnormally Dry",
                                      Drought_Level == "D1pct" ~ "D1: Moderate",
                                      Drought_Level == "D2pct" ~ "D2: Severe",
                                      Drought_Level == "D3pct" ~ "D3: Extreme",
                                      Drought_Level == "D4pct" ~ "D4: Exceptional"))

  year_len <- length(unique(ddata3$year))
  mon_len <- length(unique(ddata3$month))

  break_len <- if(year_len == 1){"1 month"
  } else if(year_len  %in% c(2, 3, 4) & mon_len <= 6){"2 months"
  } else if(year_len == 2 & mon_len > 6){"3 months"
    #} else if(year_len > 4 & mon_len <= 6){"6 months"
  } else if(year_len %in% c(4, 5, 6)){"4 months"
  } else if(year_len >= 6 & year_len < 30){"1 year"
  } else if(year_len >= 30){"4 years"
  } else {"6 months"}

  date_format <- ifelse(break_len %in% c("1 year", "2 years", "4 years"), "%Y",
                        ifelse(break_len %in% c("2 months", "3 months", "4 months"), "%b-%Y",
                               "%b"))

  if(length(years) == 1 & length(months) == 12){
    max_date <- as.Date(paste0(years, "-12-31"), format = "%Y-%m-%d")
    #datebreaks <- seq(min(ddata3$Date), max_date, by = break_len)
    datebreaks <- seq(as.Date(paste0(years, "-01-01"), format = "%Y-%m-%d"),
                      max_date, by = break_len)
  } else {
    datebreaks <- unique(c(seq(min(ddata3$Date), max(ddata3$Date) + 30, by = break_len),
                           paste0(as.numeric(max(ddata3$year)) + 1, "01-01")))
  }

  # if(break_len == "1 year"){
  #   num_uniq <- length(unique(format(datebreaks, "%Y")))
  #   datebreaks <- datebreaks[1:num_uniq]
  # }

  num_parks <- length(unique(ddata3$UnitCode))
  num_county <- length(unique(ddata3$County))

  facet_park <- if(num_parks > 1 & num_parks == num_county){TRUE} else {FALSE}
  facet_county <- if(num_parks == 1 & num_parks < num_county){TRUE} else {FALSE}
  facet_park_county <- if(num_parks > 1 & num_county > num_parks){TRUE} else {FALSE}

  x_lab <- NULL #ifelse(year_len == 1, paste0("Year: ", years), "Date")
  y_lab <- if(dom_county == TRUE){paste0("% of ", unique(ddata3$County), " in Drought")
  } else {"% of Park Counties in Drought"}

  dplot <-
    ggplot(ddata3, aes(x = Date, y = Pct_Area, fill = drought_legend, color = drought_legend)) +
    # layers
    geom_area() +
    # axis format
    scale_x_date(breaks = unique(datebreaks), limits = c(min(datebreaks), max(datebreaks)),
                 #labels = scales::label_date(date_format), expand = x_pad) +
                 labels = scales::label_date(date_format), expand = x_pad) +
    # layer formatting
    scale_fill_manual(values = c("#FFF000", "#FCD37F", "#FFAA00", "#E60000", "#730000"), name = "Drought Level") +
    scale_color_manual(values = c("#F0E100", "#E7C274", "#E19600", "#D10000", "#680000"), name = "Drought Level") +
    # theme and labels
    theme_NETN() +
    theme(legend.position = legend_position,
          panel.border = element_rect(color = "#696969", fill = NA),
          panel.background = element_rect(fill = NA, color = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 9)) + #,
    {if(legend_position == "bottom"){guides(fill = guide_legend(nrow = 1, byrow = T),
                                            color = guide_legend(nrow = 1, byrow = T))}} +
    {if(any(gridlines %in% c("grid_y", "both"))){
      theme(panel.grid.major.y = element_line(color = 'grey'))}} + #,
    {if(break_len == "4 years" & gridlines %in% c("grid_y", "both")){
      theme(panel.grid.minor.y = element_line(color = 'grey'))}} +
    {if(any(gridlines %in% c("grid_x", "both"))){
      theme(panel.grid.major.x = element_line(color = 'grey'))}} +#,
    {if(break_len == "4 years" & gridlines %in% c("grid_x", "both")){
      theme(panel.grid.minor.x = element_line(color = 'grey'))}} +
    #panel.grid.minor.x = element_line(color = 'grey'))}} +
    labs(y = y_lab, x = x_lab) +
    scale_y_continuous(n.breaks = 5, expand = c(0,0), limits = c(0, 100)) +
     # facets
    {if(facet_park){facet_wrap(~UnitCode)}} + #change to county
    {if(facet_county){facet_wrap(~County)}} +
    {if(facet_park_county){facet_wrap(~UnitCode + County)}} +
    guides(color = guide_legend(nrow = legend_row),
           fill = guide_legend(nrow = legend_row))

  return(dplot)

}
