#' @include theme_NETN.R
#' @include getClimNOAA.R
#'
#' @title plotClimCumPrecip: Plot cumulative precip relative to historic
#'
#' @importFrom dplyr arrange filter group_by left_join mutate select
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map list_rbind
#' @import ggplot2
#'
#' @description This function plots cumulative monthly precipitation for a given year compared
#' to either the 19th century or 30-year normal. If multiple parks or years are specified, resulting
#' plots will be faceted on those variables.
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
#' @param years Numeric. Years to plot separately. Accepted values start at 2006.If multiple years
#' specified, will facet results on year. Default is current year.
#'
#' @param units Specify if you want Scientific or English units. Acceptable values are "sci" (default) and "eng".
#' If "sci" precipitation units are mm; if "eng", precipitation units are in inches.
#'
#' @param normal Specify normals to plot. By default, the 20th century normal (1901-2000) plots.
#' Other options include:
#' \describe{
#' \item{"norm20cent"}{Plots the 20th century normal (1901 - 2000)}
#' \item{"norm1990"}{Plots the 30-year norm from 1991 - 2020}
#' }
#'
#' @param palette Specify 2 colors, one for the bar plot and one for the line. Default is light blue and grey.
#'
#' @param plot_title Logical. If TRUE (default) prints park name at top of figure. If FALSE,
#' does not print site name. Only enabled when one park is selected.
#'
#' @param title_type Specify whether to label facets with 4-letter UnitCode (default) or full UnitName.
#' Options are c("UnitCode", "UnitName").
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position =
#' "none" (Default). Other options are "top", "bottom", "left", "right".
#'
#' @param title_type Specify whether to label facets with 4-letter UnitCode (default) or full UnitName.
#' Options are c("UnitCode", "UnitName").
#'
#' @param numcol Specify number of columns in the facet wrap, which is only enabled when either multiple years
#' are specified or multiple parks. Default is 3.
#'
#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")
#'
#' @examples
#' \dontrun{
#'
#' # Plot ACAD cumulative precipitation for 2020 through 2023 with gridlines on y axis and precip in inches
#' plotClimCumPrecip(park = "ACAD", years = 2020:2023, legend_position = 'bottom',
#'                  gridlines = "grid_y", units = 'eng')
#'
#' # Plot ACAD cumulative precipitation for 2013 through 2023 for April - October only
#' plotClimCumPrecip(park = "ACAD", years = 2014:2024, legend_position = 'bottom', months = 4:10)
#'
#' # Plot all but SAIR cumulative precipitation for 2023 with 4 columns
#' parks <- c("ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA")
#' plotClimCumPrecip(park = parks, years = 2023, legend_position = 'bottom', numcol = 4)
#'
#'
#'}
#'
#' @return Returns a ggplot object of specified climate trends
#'
#' @export
#'
plotClimCumPrecip <- function(park = "all",
                        years = format(Sys.Date(), "%Y"),
                        months = 1:12,
                        normal = "norm20cent",
                        units = "sci",
                        palette = c("#7FC2F2", "#565656"),
                        plot_title = TRUE,
                        title_type = "UnitCode",
                        legend_position = 'right',
                        numcol = 3,
                        gridlines = 'none'){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "BOHA", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "all")){park = c("ACAD", "BOHA", "MABI", "MIMA", "MORR",
                                  "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  #if(all(!palette %in% c("viridis")) & length(years) > 1){stopifnot(length(palette) > 1)}
  stopifnot(class(plot_title) == "logical")
  normal <- match.arg(normal, c("norm20cent", "norm1990"))
  stopifnot(class(numcol) %in% c("numeric", "integer"))
  title_type <- match.arg(title_type, c("UnitCode", "UnitName"))
  gridlines <- match.arg(gridlines, c("none", "grid_y", "grid_x", "both"))
  units <- match.arg(units, c("sci", "eng"))
  stopifnot(length(palette) == 2)

  #-- Compile data for plotting --
  # Clim data as annual monthly normal
  data("NETN_clim_annual")
  data("NETN_clim_norms")

  clim_dat1 <- NETN_clim_annual |> filter(UnitCode %in% park) |> dplyr::select(UnitCode, UnitName, ppt, year, month)
  clim_dat2 <- clim_dat1 |> filter(year %in% years) |> filter(month %in% months)
  clim_dat2$date <- as.Date(paste0(clim_dat2$year, "-", clim_dat2$month, "-", 15), format = "%Y-%m-%d")

  clim_dat_long <-
    clim_dat2 |> pivot_longer(cols = -c(UnitCode, UnitName, year, month, date),
                              names_to = "param", values_to = "value") |>
    arrange(UnitCode, month, param)

  # Update clim data if requesting a year x month combination that is not currently in
  # the saved NETN_clim_annual.rda but only for complete months
  date_range_data <- sort(unique(clim_dat_long$date))
  date_range_fxn <- paste0(rep(years, length(months)),"-", rep(sprintf("%02d", months), length(years)), "-", 15)
  new_dates1 <- date_range_fxn[!date_range_fxn %in% date_range_data]

  # latest date of complete month
  mon_curr <- as.numeric(format(Sys.Date(), "%m"))
  mon_next_day <- as.numeric(format(Sys.Date() + 1, "%m"))
  mon_comp <- ifelse(mon_next_day > mon_curr, sprintf("%02d", mon_curr), sprintf("%02d", mon_curr - 1))
  latest_date_comp <- as.Date(paste0(format(Sys.Date(), "%Y"), "-", mon_comp, "-", 15))
  latest_date_data <- as.Date(max(date_range_data), format = "%Y-%m-%d")

  new_dates <- as.Date(new_dates1[new_dates1 <= latest_date_comp], format = "%Y-%m-%d")
  #new_dates <- as.Date(c("2024-05-15", "2024-04-15"), format = "%Y-%m-%d")

  clim_dat_long1 <-
    if(length(new_dates) == 0){clim_dat_long
    } else {
      new_months <- as.numeric(format(new_dates, "%m"))
      new_years <- as.numeric(format(new_dates, "%Y"))
      new_clim <- purrr::map(new_years, function(y){
        getClimNOAA(park = park, year = y, months = new_months)}
      ) |> list_rbind()
      if(nrow(new_clim) > 0){
        new_clim_long <- new_clim |> pivot_longer(cols = -c(UnitCode, UnitName, long, lat, year, month),
                                                  names_to = "param", values_to = "value")
        new_clim_long$date <- as.Date(paste0(
          new_clim_long$year, "-", new_clim_long$month, "-", 15), format = "%Y-%m-%d")
        comb_clim <- rbind(clim_dat_long, new_clim_long)
      } else {clim_dat_long}
    }

  # Clim data in decadal and 30-year norms
  avg_dat <- NETN_clim_norms |> filter(UnitCode %in% park) |> #|> filter(month %in% months)
    select(UnitCode, UnitName, month, ppt_norm_1901_2000, ppt_norm_1991_2020)

  avg_dat_long <- avg_dat |> pivot_longer(cols = -c(UnitCode, UnitName, month),
                                          names_to = "param_full", values_to = "value") |>
    mutate(param = sub("_.*", "", param_full),
           stat = ifelse(grepl("norm", param_full), "avg", "std"),
           norm = ifelse(grepl(1901, param_full), "norm20cent", "norm1990")) |>
    arrange(UnitCode, param, month)

  # filter on norm
  col1 <- ifelse(normal == "norm20cent", "ppt_norm_1901_2000", "ppt_norm_1991_2020")

  avg_dat_long2 <- avg_dat_long |> filter(param_full %in% col1) |>
    filter(stat == "avg") |> filter(norm == normal)

  # Combine the annual and norm data so can calculate difference from normal using
  # a generic parameter label
  # clim_dat_long1$param <- gsub("prcp", "ppt", clim_dat_long1$param)
  # avg_dat_long2$param <- gsub("precip", "ppt", avg_dat_long2$param)

  clim_comb <- left_join(clim_dat_long1, avg_dat_long2,
                         by = c("UnitCode", "UnitName", "month", "param"),
                         suffix = c("_curr", "_norm")) |>
    filter(!is.na(value_norm)) |>
    group_by(UnitCode, UnitName, year) |>
    mutate(cum_ppt_curr = cumsum(value_curr),
           cum_ppt_hist = cumsum(value_norm))

  # set up plotting features
  ptitle <- if(length(unique(clim_comb$UnitCode)) == 1 & plot_title == TRUE){
    unique(clim_comb$UnitName)} else {NULL}

  units_ppt <- if(units == "sci"){"mm"} else {"in"}
  ylabel = paste0("Cumulative Monthly Precip. (", units_ppt, ")")

  clim_comb$mon <- factor(clim_comb$month,
                          levels = unique(clim_comb$month),
                          labels = unique(month.abb[clim_comb$month]), ordered = T)

  clim_comb$park_facet <- if(title_type == "UnitCode"){clim_comb$UnitCode} else {clim_comb$UnitName}

  facet_park <- ifelse(length(park) > 1, TRUE, FALSE)
  facet_year <- ifelse(length(years) > 1, TRUE, FALSE)

  xaxis_breaks <- month.abb[months]

  avg_name <- ifelse(normal == "norm20cent", "20th Century Baseline", "30 year Baseline")

  clim_comb1 <- if(units == "sci"){clim_comb
  } else {
      clim_comb |> mutate(cum_ppt_curr = cum_ppt_curr/25.4,
                          cum_ppt_hist = cum_ppt_hist/25.4)
    }

  pptplot <-
    ggplot(clim_comb1, aes(x = mon)) + theme_NETN() +
      geom_bar(stat = 'identity', aes(y = cum_ppt_curr, fill = "Curr", color = "Curr"), alpha = 0.8) +
      geom_line(aes(y = cum_ppt_hist, group = stat, color = "Hist"), lwd = 2) +
      scale_color_manual(
        values = c("Curr" = palette[1], "Hist" = palette[2]),
        labels = c("Curr" = "Current", "Hist" = avg_name),
        aesthetics = c("color", "fill")) +
      # facets
      {if(facet_year == FALSE & facet_park == TRUE){facet_wrap(~park_facet, ncol = numcol)}} +
      {if(facet_year == TRUE & facet_park == FALSE){facet_wrap(~year, ncol = numcol)}} +
      {if(facet_year == TRUE & facet_park == TRUE){facet_wrap(~park_facet + year, ncol = numcol)}} +
      # labels/themes
      labs(x = NULL, y = ylabel, group = NULL, color = NULL, fill = NULL) +
      scale_x_discrete(breaks = xaxis_breaks, drop = F) +
      scale_y_continuous(n.breaks = 8) +
    {if(any(gridlines %in% c("grid_y", "both"))){
      theme(
        panel.grid.major.y = element_line(color = 'grey'))}} +
    {if(any(gridlines %in% c("grid_x", "both"))){
      theme(
        panel.grid.major.x = element_line(color = 'grey'))}} +
      theme(legend.position = legend_position,
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10))

  return(#suppressWarnings(
    pptplot
  )
  #)
}


