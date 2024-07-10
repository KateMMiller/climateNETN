#' @include theme_NETN.R
#' @include getClimNOAA.R
#'
#' @title plotClimComps: Plot climate comparisons
#'
#' @importFrom dplyr arrange filter left_join mutate
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map list_rbind
#' @import ggplot2
#'
#' @description This function compares historic average monthly climate variables with
#' user-specified years and months to provide an idea of how extreme or normal a given month in a
#' year is. This function only works with NOAA gridded climate data, and plots park
#' centroids, not site-level values. Each month's data are typically available about
#' two weeks after the month ends. To save download time, the NETN_clim_annual dataset contains
#' monthly climate data from January 2006 through May 2024.
#'
#' @param park Specify park to plot. Currently can only plot 1 park at a time. Valid inputs:
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
#' @param years Numeric. Years to plot separately. Accepted values start at 2006.
#'
#' @param months Vector of numeric months to query. If specifying new months not yet included in
#' NETN_clim_annual dataset, will download months that are available from NOAA.
#'
#' @param parameter Specify the monthly averaged parameter to plot. Acceptable values are
#' \describe{
#' \item{"tmean"}{Plot mean temperature comparisons.}
#' \item{"tmax"}{Plot max temperature comparisons.}
#' \item{"tmin"}{Plot min temperature comparisons.}
#' \item{"ppt"}{Plot total precipitation comparisons.}
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
#' @param layers Options are "points", "lines", or both for Annual Values. By default, only lines will plot.
#'
#' @param palette Color palette for plots. Options currently are 'viridis' (yellow - green - blue),
#' magma (yellow, red, purple), plasma (brighter version of magma), turbo (rainbow), or create your
#' own by specifying 2 or more colors in quotes, like palette = c("red", "yellow", "blue"),
#' or palette = c("yellow", "blue"). Hexcodes work too. If only 1 year is specified, only need to
#' specify 1 color
#'
#' @param color_rev Reverse the order of the color pallet. Only enabled for palette = 'viridis'.
#'
#' @param plot_title Logical. If TRUE (default) prints site name at top of figure. If FALSE,
#' does not print site name. Only enabled when one site is selected.
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position =
#' "none" (Default). Other options are "top", "bottom", "left", "right".
#'
#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")
#'
#' @param ... Additional arguments relevant to \code{sumClimAvgs()} or \code{sumClimMonthly()}
#'
#' @examples
#' \dontrun{
#'
#' # Plot mean monthly temp for MABI for 2019:2023 and all months with red-blue color palette
#' plotClimComps(park = "MABI", years = 2019:2023, parameter = "tmean", palette = c('red', 'blue'))
#'
#' # Same as above, but with points too
#' plotClimComps(park = "MABI", years = 2019:2023, parameter = "tmean", palette = c('red', 'blue'),
#'   layers = c('points', 'lines'))
#'
#' # Plot max monthly temp ACAD for 2019:2023 and all months with 1991- 2020 normals with english units
#' plotClimComps(park = "ACAD", years = 2019:2023, parameter = "tmax", normal = "norm1990",
#'   palette = c('red', 'blue'), layers = 'lines', units= 'eng')
#'
#' # Plot total monthly precip for ACAD for past 10 years using a blue color scheme
#' plotClimComps(park = "ACAD", years = 2013:2023, parameter = 'ppt',
#'   palette = c("#75C5FF", "#3563DD", "#323969"))
#'
#' # Plot total monthly precip for latest year in ACAD
#' plotClimComps(park = "ACAD", years = 2024, parameter = "ppt", months = 1:5)
#'
#'}
#'
#' @return Returns a ggplot object of specified climate trends
#'
#' @export
#'
plotClimComps <- function(park = "ACAD",
                          years = 2006:format(Sys.Date(), "%Y"),
                          months = 1:12,
                          layers = "lines",
                          normal = "norm20cent",
                          parameter = 'tmean', units = "sci",
                          plot_title = TRUE,
                          palette = "viridis", color_rev = FALSE,
                          legend_position = 'right',
                          gridlines = 'none', ...){

  #-- Error handling --
  park <- match.arg(park, several.ok = FALSE,
                    c(#"all", "LNETN",
                      "ACAD", "BOHA", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  #if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  parameter <- match.arg(parameter, c("tmean", "tmax", "tmin", "ppt"))
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  layers <- match.arg(layers, c("points", "lines"), several.ok = TRUE)
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  if(all(!palette %in% c("viridis", "magma", "plasma", "turbo")) & length(years) > 1){stopifnot(length(palette) > 1)}
  stopifnot(class(plot_title) == "logical")
  normal <- match.arg(normal, c("norm20cent", "norm1990"))
  gridlines <- match.arg(gridlines, c("none", "grid_y", "grid_x", "both"))
  units <- match.arg(units, c("sci", "eng"))

  #-- Compile data for plotting --
  # Clim data as annual monthly normal
  data("NETN_clim_annual")
  data("NETN_clim_norms")

  clim_dat <- NETN_clim_annual |> filter(UnitCode %in% park)
  clim_dat2 <- clim_dat |> filter(year %in% years) |> filter(month %in% months)
  clim_dat2$date <- as.Date(paste0(clim_dat2$year, "-", clim_dat2$month, "-", 15), format = "%Y-%m-%d")

  clim_dat_long <-
      clim_dat2 |>
        select(UnitCode:month, date) |>
        pivot_longer(cols = -c(UnitCode, UnitName, year, month, date, lat, long),
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

  clim_dat_final1 <-
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

  park_names <- unique(clim_dat[,c("UnitCode", "UnitName")])
  clim_dat_final2 <- left_join(clim_dat_final1, park_names, by = c("UnitCode", "UnitName"))

  # Clim data in century or 30-year norms
  avg_dat <- NETN_clim_norms |> filter(UnitCode %in% park) #|> filter(month %in% months)

  avg_dat_long <- avg_dat |> pivot_longer(cols = -c(UnitCode, UnitName, long, lat, month),
                                                  names_to = "param_full", values_to = "value") |>
    mutate(param = sub("_.*", "", param_full),
           stat = ifelse(grepl("norm", param_full), "avg", "std"),
           norm = ifelse(grepl(1901, param_full), "norm20cent", "norm1990")) |>
    arrange(UnitCode, param, month)

  #-- Set up plotting features --
  color_dir <- ifelse(color_rev == FALSE, -1, 1)

  # annual params
  units_temp <- if(units == "sci"){"C"} else {"F"}
  units_ppt <- if(units == "sci"){"mm"} else {"in"}

  param_labels_annual <-
    data.frame(param = c("ppt", "tmean", "tmax", "tmin"),
               param_label = c(paste0("Total Precip. (", units_ppt, ")"),
                               paste0("Avg. Temp. (", units_temp, ")"),
                               paste0("Max. Temp. (", units_temp, ")"),
                               paste0("Min. Temp. (", units_temp, ")")))

  clim_dat_final <- left_join(clim_dat_final2, param_labels_annual, by = 'param')

  clim_dat_final$mon <- factor(clim_dat_final$month, levels = unique(clim_dat_final$month),
                               labels = unique(month.abb[clim_dat_final$month]), ordered = T)
  clim_dat_final$param_label2 <- paste0(clim_dat_final$param_label, " (", clim_dat_final$year, ")")

  clim_dat_final <-
    if(units == "sci"){clim_dat_final
    } else if(units == "eng"){
      clim_dat_final |> mutate(value = ifelse(param == "ppt", value/25.4, (value * 9/5) + 32))
    }

  # norm params
  norms <- data.frame(norm = c("norm20cent", "norm1990"),
                      param_label = c("years 1901-2000", "years 1991-2020"))

  avg_dat_final <- left_join(avg_dat_long, norms, by = "norm")

  avg_dat_final$mon <- factor(avg_dat_final$month,
                              levels = unique(avg_dat_final$month),
                              labels = unique(month.abb[avg_dat_final$month]), ordered = T)

  avg_dat_final <-
    if(units == "sci"){avg_dat_final
    } else if(units == "eng"){
      avg_dat_final |> mutate(value = ifelse(param == "precip", value/25.4, (value * 9/5) + 32))
    }

  # set up filter and labelling on parameter
  if(parameter == "tmean"){
    ann_filt = c("tmean")
    norm_filt <- if(normal == "norm20cent"){"tmean_norm_1901_2000"} else {"tmean_norm_1991_2020"}
    y_label = paste0("Avg. Monthly Temp. (", units_temp, ")")
  } else if(parameter == "tmin"){
    ann_filt = c("tmin")
    norm_filt <- if(normal == "norm20cent"){"tmin_norm_1901_2000"} else {"tmin_norm_1991_2020"}
    y_label = paste0("Avg. Minimum Monthly Temp. (", units_temp, ")")
  } else if(parameter == "tmax"){
    ann_filt = c("tmax")
    norm_filt <- if(normal == "norm20cent"){"tmax_norm_1901_2000"} else {"tmax_norm_1991_2020"}
    y_label = paste0("Avg. Maximum Monthly Temp. (", units_temp, ")")
  } else if(parameter == "ppt"){
    ann_filt = c("ppt")
    norm_filt <- if(normal == "norm20cent"){"ppt_norm_1901_2000"} else {"ppt_norm_1991_2020"}
    y_label = paste0("Total Monthly Precip. (", units_ppt, ")")
  }

  year_breaks <- as.integer(years)
    # if(length(years) <= 5){years
    # } else {as.integer(c(quantile(clim_dat_final$year, probs = c(0, 0.25, 0.5, 0.75, 1), names = FALSE)))}

  vir_pal = unique(ifelse(palette %in% c("viridis", "magma", "plasma", "turbo"), "viridis", "colbrew"))

  if(any(palette %in% c("viridis", "magma", "plasma", "turbo"))){
  vir_option <- switch(palette,
                       viridis = 'viridis',
                       magma = 'magma',
                       plasma = 'plasma',
                       turbo = 'turbo')}

  pal <-
    if(vir_pal == "colbrew"){
      if(length(palette) > 1){
        colorRampPalette(palette)(length(unique(clim_dat_final$year)))
      } else { # hack to allow gradient to work with 1 color
        colorRampPalette(c(palette, palette))(length(unique(clim_dat_final$year)))
      }
    }


  #leg_guide <- if(length(years) > 5){"colourbar"} else{"legend"}
  ptitle <- if(length(unique(clim_dat_final$UnitCode)) == 1 & plot_title == TRUE){
    unique(clim_dat_final$UnitCode)} else {NULL}

  avg_name <- ifelse(normal == "norm20cent", "20th Century Baseline", "30-year Baseline")

  clim_plot <-
  ggplot() + theme_NETN() +
  geom_line(data = avg_dat_final |> filter(param_full %in% norm_filt),
            aes(x = mon, y = value, group = param_label, linetype = param_label),
            linewidth = 0.8) + #} +
  # line type for normal
  scale_linetype_manual(values = c("longdash"), name = avg_name) + #}} +
  # layers for annual data
  {if(any(layers %in% "lines"))
      geom_line(data = clim_dat_final |> filter(param %in% ann_filt),
                aes(x = mon, y = value, group = as.integer(year), color = as.integer(year)))} +
  {if(any(layers %in% "points"))
      geom_point(data = clim_dat_final |> filter(param %in% ann_filt),
                 aes(x = mon, y = value, group = as.integer(year),
                     color = as.integer(year), fill = as.integer(year)))} +
  # color palettes for annual data
  {if(vir_pal == 'viridis') scale_fill_viridis_c(direction = color_dir, guide = "legend", option = vir_option,
                                               name = 'Annual Values', breaks = year_breaks)} +
  {if(vir_pal == 'viridis') scale_color_viridis_c(direction = color_dir, guide = "legend", option = vir_option,
                                               name = 'Annual Values', breaks = year_breaks)} +
  {if(vir_pal == "colbrew") scale_fill_gradientn(colors = pal, guide = "legend",
                                                         name = 'Annual Values', breaks = year_breaks)} +
  {if(vir_pal == "colbrew") scale_color_gradientn(colors = pal, guide = "legend",
                                                          name = 'Annual Values', breaks = year_breaks)} +
  # # facets for multiple years
  # {if(length(unique(clim_dat_long$UnitCode)) > 1) facet_wrap(~UnitName)} +

  # labels/themes
  labs(x = NULL, y = y_label, title = ptitle,
       color = "Annual Values", linetype = avg_name, linewidth = avg_name) +
    scale_y_continuous(n.breaks = 8) +
  {if(any(gridlines %in% c("grid_y", "both"))){
    theme(
      panel.grid.major.y = element_line(color = 'grey'))}} + #,
      #panel.grid.minor.y = element_line(color = 'grey'))}} +
  {if(any(gridlines %in% c("grid_x", "both"))){
    theme(
      panel.grid.major.x = element_line(color = 'grey'))}} +#,
      #panel.grid.minor.x = element_line(color = 'grey'))}} +
  theme(legend.position = legend_position,
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

 return(#suppressWarnings(
   clim_plot
   )
 #)
}


