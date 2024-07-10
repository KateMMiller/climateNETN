#' @include getClimNOAA.R
#' @include theme_NETN.R
#'
#' @title plotClimAnom: Plot climate anomalies from baseline
#'
#' @importFrom dplyr arrange filter left_join mutate select
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom purrr map list_rbind
#' @import ggplot2
#'
#' @description This function produces a trend plot filtered on park, year, month, and
#' climate parameter and shows anomalies from chosen baseline (1901 - 2000 or 1991 - 2020)
#' as red for above and blue for below average using gridded NOAA NClim data. If a year x month
#' combination is specified that doesn't occur yet in NETN_clim_annual dataset, it will be
#' downloaded if available. New months are typically available within a few weeks of the month end.
#' If multiple parks are specified, they can either be plotted on the same figure or separate
#' figures. If multiple parameters are specified, they will be plotted on separate figures.
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
#' @param years Numeric. Years to query. Accepted values start at 1895.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12.
#' If specifying new months not yet included in NETN_clim_annual dataset, will
#' download months that are available from NOAA.
#'
#' @param parameter Specify the monthly averaged parameter to plot. Acceptable values are
#' \describe{
#' \item{"tmean"}{Plot mean temperature comparisons.}
#' \item{"tmax"}{Plot max temperature comparisons.}
#' \item{"tmin"}{Plot min temperature comparisons.}
#' \item{"ppt"}{Plot total precipitation comparisons in raw units.}
#' \item{"ppt_pct"}{Plot relative percent difference in precipitation.}
#' }
#'
#' @param units Specify if you want Scientific or English units. Acceptable values are "sci" (default) and "eng".
#' If "sci", temperature units are in C and precipitation units are in mm. If "eng", temperature units are in F,
#' and precipitation units are in inches.
#'
#' @param normal Specify normal to plot. By default, the 20th century normal (1901-2000) plots.
#' Other options include:
#' \describe{
#' \item{"norm20cent"}{Plots the 20th century normal (1901 - 2000)}
#' \item{"norm1990"}{Plots the 30-year norm from 1991 - 2020}
#' }
#'
#' @param palette Specify 2 colors for above and below average bars. Default is red for above average and blue
#' for below average, each with transparent fill.
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position = "none" (Default).
#' Other options are "top", "bottom", "left", "right".
#'
#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")
#'
#' @param title_type Specify whether to label facets with 4-letter UnitCode (default) or full UnitName.
#' Options are c("UnitCode", "UnitName").
#'
#' @examples
#' \dontrun{
#' # Plot ACAD average temperature anomalies from 1895 to 2024 compared with 20th century normals with gridlines on y axis.
#' plotClimAnom(park = "ACAD", years = 1895:2024, parameter = "tmean", legend_position = 'right',
#' gridlines = "grid_y")
#'
#' # Plot MABI and SAGA average temperature anomalies from 2006 to 2024 compared with 20th century normals and english units.
#' plotClimAnom(park = c("MABI", "SAGA"), years = 2006:2024, parameter = "tmean", units = "eng", legend_position = 'right')
#'
#' # Plot WEFA percent precipitation anomalies in 2023 compared with 20th century normals.
#' plotClimAnom(park = "WEFA", years = 2023, parameter = "ppt_pct", legend_position = 'right')
#'
#'}
#'
#' @return Returns a ggplot object of specified climate trends
#'
#' @export
#'
plotClimAnom <- function(park = "all",
                          years = 1895:format(Sys.Date(), "%Y"),
                          months = 1:12,
                          parameter = 'tmean', units = "sci",
                          normal = "norm20cent",
                          palette = c("red", "blue"),
                          legend_position = 'none',
                          title_type = "UnitCode",
                          gridlines = 'none'){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "BOHA", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  if(any(park == "all")){park = c("ACAD", "BOHA", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")
  } else {park}
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 1895)

  parameter <- match.arg(parameter, c("tmean", "tmax", "tmin", "ppt", "ppt_pct"), several.ok = TRUE)
  normal <- match.arg(normal, c("norm20cent", "norm1990"))
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  gridlines <- match.arg(gridlines, c("none", "grid_y", "grid_x", "both"))
  units <- match.arg(units, c("sci", "eng"))
  stopifnot(length(palette) == 2)

  #-- Compile data for plotting --
  # Clim data as annual monthly normal
  data("NETN_clim_annual")
  data("NETN_clim_norms")

  clim_dat <- NETN_clim_annual |> filter(UnitCode %in% park)
  clim_dat2 <- clim_dat |> filter(year %in% years) |> filter(month %in% months)
  clim_dat2$date <- as.Date(paste0(clim_dat2$year, "-", clim_dat2$month, "-", 15), format = "%Y-%m-%d")

  clim_dat_long <-
    clim_dat2 |>
      dplyr::select(UnitCode:month, date) |>
      pivot_longer(cols = -c(UnitCode, UnitName, year, month, date, lat, long),
                   names_to = "param", values_to = "value") |>
    arrange(UnitCode, month, param)

  avg_dat <- NETN_clim_norms |> filter(UnitCode %in% park) #|> filter(month %in% months)


  # Only need norms for this plotting function, so dropping error cols
  avg_dat_cols <- c("UnitCode", "UnitName", "month", names(avg_dat[,grep("norm", names(avg_dat))]))
  avg_dat1 <- avg_dat[,avg_dat_cols]

  avg_dat_long <- avg_dat1 |>
    pivot_longer(cols = -c(UnitCode, UnitName, month),
                names_to = "param_full", values_to = "avg") |>
    mutate(param = sub("_.*", "", param_full),
           norm = ifelse(grepl(1901, param_full), "norm20cent", "norm1990")) |>
    arrange(UnitCode, param, month)

  avg_dat_long2 <- avg_dat_long |> filter(norm == normal)

  # Update clim data if requesting a year x month combination that is not currently in
  # the saved NETN_clim_2006_2024.rda but only for complete months
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

  clim_dat <-
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

  if(nrow(clim_dat) == 0){stop("Specified arguments returned a data frame with 0 records.")}

  # clim_dat$param[clim_dat$param == "prcp"] <- "ppt"
  # clim_dat$param[clim_dat$param == "tavg"] <- "tmean"
  # avg_dat_long2$param[avg_dat_long2$param == "precip"] <- "ppt"
  # avg_dat_long2$param[avg_dat_long2$param == "tavg"] <- "tmean"

  # head(avg_dat_long2)
  # avg_dat3 <- avg_dat_long2 |> dplyr::select(-param_full) |> pivot_wider(names_from = stat, values_from = value)

  clim_comb <- left_join(clim_dat, avg_dat_long2,
                         by = c("UnitCode", "UnitName", "month", "param"))

  clim_comb$anom <- clim_comb$value - clim_comb$avg
  clim_comb$pct_diff <- ((clim_comb$value - clim_comb$avg)/clim_comb$avg)*100
  clim_comb$anom_type <- ifelse(clim_comb$anom > 0, "above", "below")

  clim_comb1 <-
    if(units == "sci"){clim_comb
    } else if(units == "eng"){
        clim_comb |> mutate(value = ifelse(param == "ppt", value/25.4, (value * 9/5) + 32),
                            avg = ifelse(param == "ppt", avg/25.4, (avg * 9/5) + 32),
                            anom = value - avg)
      }

  param <- if(any(parameter == "all")){c("ppt", "tmean", "tmax", "tmin")} else {parameter}

  #-- Set up plotting features --
  ylab <- ifelse(length(unique(clim_comb1$param)) == 1, unique(clim_comb1$param), "Monthly Value")

  facetpark <- ifelse(length(unique(clim_comb1$UnitCode)) > 1, TRUE, FALSE)
  facetparam <- ifelse(length(parameter) > 1, TRUE, FALSE)
  facet_y <- if(length(parameter) > 1 & any(parameter %in% c("ppt", "ppt_pct"))){"free_y"} else {"fixed"}

  clim_comb1$park_facet <- if(title_type == "UnitCode"){clim_comb1$UnitCode} else {clim_comb1$UnitName}

  pars <- c("ppt", "tmax", "tmin", "tmean", "ppt_pct")

  units_temp <- if(units == "sci"){"C"} else {"F"}
  units_ppt <- if(units == "sci"){"mm"} else {"in"}

  plabs <- c(paste0("Total Precip. (", units_ppt, ")"),
             paste0("Avg. Max. Temp. (", units_temp, ")"),
             paste0("Avg. Min. Temp. (", units_temp, ")"),
             paste0("Average Temp. (", units_temp, ")"),
             paste0("% of Total Precip."))

  param_labels <- data.frame(param = pars, param_label = plabs)

  clim_comb2 <- left_join(clim_comb1, param_labels, by = 'param')

  ylab <- ifelse(length(parameter) > 1, "Monthly Value", param_labels$param_label[param_labels$param == parameter])

  clim_comb2$date2 <- as.Date(clim_comb2$date, format = c("%Y-%m-%d"))

  clim_comb3 <- if(any(parameter %in% "ppt_pct")){
    ppt_pct <- clim_comb2 |> filter(param %in% "ppt") |> mutate(anom = pct_diff, param = "ppt_pct")
    comb <- rbind(clim_comb2, ppt_pct)
    comb
  } else {clim_comb2}

  clim_comb4 <- clim_comb3 |> filter(param %in% parameter) |> filter(year %in% years)

  year_len <- length(unique(clim_comb4$year))
  mon_len <- length(unique(clim_comb4$month))

  break_len <- if(year_len == 1){"1 month"
  } else if(year_len  %in% c(2, 3, 4) & mon_len <= 6){"2 months"
  } else if(year_len == 2 & mon_len > 6){"3 months"
    #} else if(year_len > 4 & mon_len <= 6){"6 months"
  } else if(year_len %in% c(4, 5, 6)){"4 months"
  } else if(year_len >= 6 & year_len < 20){"2 years"
  } else if(year_len >= 20){"5 years"
  } else {"6 months"}

  date_format <- ifelse(break_len %in% c("1 year", "2 years", "5 years"), "%Y",
                        ifelse(break_len %in% c("2 months", "3 months", "4 months"), "%b-%Y",
                                                "%b"))
  datebreaks <- seq(min(clim_comb4$date2, na.rm = T), max(clim_comb4$date2, na.rm = T) + 30, by = break_len)

  ylabel <- if(length(parameter) > 1){"Deviation from Baseline"
    } else {
            switch(parameter,
                   "ppt" = paste0("Precipitation Anomaly (", units_ppt, ")"),
                   "tmin" = paste0("Temperature Anomaly (", units_temp, ")"),
                   "tmax" = paste0("Temperature Anomaly (", units_temp, ")"),
                   "tmean" = paste0("Temperature Anomaly (", units_temp, ")"),
                   "ppt_pct" = paste0("+/- % of Precipitation Anomaly"))
    }

  above_label <- if(year_len == 1){paste0("Above baseline for ", years)} else {"Above baseline"}
  below_label <- if(year_len == 1){paste0("Below baseline for ", years)} else {"Below baseline"}
  avglabel <- ifelse(normal == "norm20cent", "Baseline: 1901 - 2000", "Baseline: 1991 - 2020")


anomplot <-
  ggplot(clim_comb4, aes(x = date2, y = anom,
                         fill = factor(anom_type),
                         color = factor(anom_type),
                         group = factor(anom_type))) +
    geom_bar(stat = 'identity', alpha = 0.4, lwd = 0.05) +
    scale_color_manual(values = c("above" = palette[1], "below" =  palette[2]),
                      labels = c("above" = above_label,
                                 "below" = below_label),
                      aesthetics = c("color", "fill"),
                      name = NULL) +
    geom_hline(aes(yintercept = 0, linetype = "Baseline")) +
    scale_linetype_manual(values = c("Baseline" = "solid"),
                          labels = c("Baseline" = avglabel),
                          name = NULL) +
    # facets
    {if(facetparam == FALSE & facetpark == TRUE){facet_wrap(~park_facet)}} +
    {if(facetparam == TRUE & facetpark == FALSE){facet_wrap(~param_label, scales = facet_y)}} +
    {if(facetparam == TRUE & facetpark == TRUE){facet_wrap(~park_facet + param_label)}} +
    # labels/themes
    theme_NETN() +
    labs(x = NULL, y = ylabel) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          legend.position = legend_position,
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10)) +
    {if(any(gridlines %in% c("grid_y", "both"))){
      theme(
        panel.grid.major.y = element_line(color = 'grey'))}} +
    {if(any(gridlines %in% c("grid_x", "both"))){
      theme(
        panel.grid.major.x = element_line(color = 'grey'))}} +
    # Axes
    scale_x_date(breaks = datebreaks, labels = scales::label_date(date_format)) +
    scale_y_continuous(n.breaks = 8, limits = c(-max(clim_comb4$anom), max(clim_comb4$anom))) +
    # Legend order
    guides(linetype = guide_legend(order = 2),
           fill = guide_legend(order = 1),
           color = guide_legend(order = 1))

   return(#suppressWarnings(
    anomplot)#)
}


