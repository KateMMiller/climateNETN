#' @include getClimNOAA.R
#' @include theme_NETN.R
#'
#' @title plotClimBar: Plot climate trends as bar plot
#'
#' @importFrom dplyr filter left_join mutate
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map list_rbind
#' @import ggplot2
#'
#' @description This function produces bar plot filtered on park, year, month, and climate parameter.
#' This function was developed to compare with monthly precip. with monthly discharge measurements for
#' water monitoring, and by default only plots May - October. If you're plotting all months, use plotClimTrend()
#' instead of this function. If multiple parks are specified, they can either be plotted on the same figure or
#' separate figures. If multiple parameters are specified, they will be plotted on separate figures.
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
#' Default is 5:10. If specifying new months not yet included in NETN_clim_annual dataset, will
#' download months that are available from NOAA.
#'
#' @param parameter Specify the parameter(s) to plot. Acceptable values are
#' \describe{
#' \item{"all"}{Plot all climate variables}
#' \item{"ppt"}{Monthly total precipitation.}
#' \item{"tmax"}{Monthly average maximum temperature.}
#' \item{"tmin}{Monthly average minimum temperature.}
#' \item{"tmean}{Monthly average temperature.}
#' }
#'
#' @param units Specify if you want Scientific or English units. Acceptable values are "sci" (default) and "eng".
#' If "sci", temperature units are in C and precipitation units are in mm. If "eng", temperature units are in F,
#' and precipitation units are in inches.
#'
#' @param facet_park Logical. If TRUE, plots sites on separate facets (ie figures). If FALSE (Default),
#' plots all sites on the same figure. This is only enabled if multiple sites are chosen.
#'
#' @param facet_param Logical. If TRUE, plots parameters on separate facets. If FALSE (Default), plots
#' all parameters on the same figure. Note that function will automatically facet if parameters with
#' different units (ie tmean and ppt) are selected.
#'
#' @param palette Specify a vector of colors. Default is light blue ('#1e90ff') If fewer colors than
#' parameters are specified, they will be ramped to generate enough colors.
#'
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position =
#' "none" (Default). Other options are "top", "bottom", "left", "right".
#'
#' @param numcol Specify number of columns in the facet wrap, which is only enabled when either multiple years
#' are specified or multiple parks. Default is 2.
#'
#' @param line_width Numeric. Specify the width of the line. Default is 1.
#'
#' @param x_pad Vector of 2 values. Specifies how much padding to add to x-axis on either side of axis.
#' Default is c(0.01, 0)
#'
#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default),
#' "grid_y", "grid_x", "both")
#'
#' @examples
#' \dontrun{
#'
#' # Plot total monthly precip for 2006:2023 for May - Oct in English units
#' plotClimBar(park = "MABI", years = 2006:2023, parameter = "ppt", units = 'eng')
#'
#'
#'}
#'
#' @return Returns a ggplot object of specified climate trends
#'
#' @export
#'
plotClimBar <- function(park = "all",
                          years = 2006:format(Sys.Date(), "%Y"),
                          months = 5:10,
                          parameter = NA, units = "sci",
                          facet_park = FALSE,
                          facet_param = FALSE,
                          palette = "#1e90ff",
                          numcol = 2, x_pad = c(0.01, 0),
                          legend_position = 'none', gridlines = 'none'){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "BOHA", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  if(any(park == "all")){park = c("ACAD", "BOHA", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")
    } else {park}
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 1895)

  parameter <- match.arg(parameter, c("all", "tmean", "tmin", "tmax", "ppt"), several.ok = TRUE)
  if(any(parameter == "all")){parameter = c("tmean", "tmin", "tmax", "ppt")}

  facet_y <- if(any(parameter == "all")){"free_y"} else {"fixed"}
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  gridlines <- match.arg(gridlines, c("none", "grid_y", "grid_x", "both"))
  units <- match.arg(units, c("sci", "eng"))
  stopifnot(class(numcol) %in% c("numeric", "integer"), numcol > 0)
  stopifnot(length(x_pad) == 2)

  #-- Compile data for plotting --
  # Clim data as annual monthly averages
  data("NETN_clim_annual")
  data("NETN_clim_norms")

  clim_dat <- NETN_clim_annual |> filter(UnitCode %in% park)
  clim_dat2 <- clim_dat |> filter(year %in% years) |> filter(month %in% months)
  clim_dat2$date <- as.Date(paste0(clim_dat2$year, "-", clim_dat2$month, "-", 15), format = "%Y-%m-%d")

  clim_dat_long <-
    clim_dat2 |> pivot_longer(cols = -c(UnitCode, UnitName, year, month, date, lat, long),
                              names_to = "param", values_to = "value") |>
    arrange(UnitCode, month, param)

  # Update clim data if requesting a year x month combination that is not currently in
  # the saved NETN_clim_2006_2024.rda but only for complete months
  date_range_data <- sort(unique(clim_dat_long$date))
  date_range_fxn <- paste0(rep(years, each = length(months)),"-",
                           rep(sprintf("%02d", months), length(years)), "-", 15)
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
    if(length(new_dates) == 0 || is.na(new_dates)){clim_dat_long
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

  param <- if(any(parameter == "all")){c("ppt", "tmean", "tmax", "tmin")} else {parameter}

  #-- Set up plotting features --
  ylab <- ifelse(length(unique(clim_dat$param)) == 1, unique(clim_dat$param),
                 paste0("Monthly Value"))

  facetpark <- ifelse(facet_park == TRUE & length(unique(clim_dat$UnitCode)) > 1, TRUE, FALSE)
  facetparam <- ifelse((facet_param == TRUE & length(unique(clim_dat$param)) > 1) |
                         any(parameter %in% c("ppt", "ppt_pct")) &
                         any(parameter %in% c("tmean", "tmax", "tmin")), TRUE, FALSE)

  pars <- c("ppt", "tmax", "tmin", "tmean", "ppt_pct",
            "ppt_ra", "tmax_ra", "tmin_ra", "tmean_ra", "ppt_pct_ra")

  units_temp <- if(units == "sci"){"C"} else {"F"}
  units_ppt <- if(units == "sci"){"mm"} else {"in"}

  plabs <- c(paste0("Total Precip. (", units_ppt, ")"),
             paste0("Avg. Max. Temp. (", units_temp, ")"),
             paste0("Avg. Min. Temp. (", units_temp, ")"),
             paste0("Average Temp. (", units_temp, ")"),
             paste0("% of Total Precip."))

  param_labels <- data.frame(param = pars, param_label = plabs)

  clim_dat1 <- left_join(clim_dat, param_labels, by = 'param')

  ylab <- if(length(parameter) > 1 &
             any(parameter %in% c("ppt", "ppt_pct")) &
             any(parameter %in% c("tmean", "tmax", "tmin"))){"Monthly Value"
  } else if(length(parameter) > 1 & all(parameter %in% c("tmean", "tmax", "tmin"))){
    paste0("Monthly Temperature (", units_temp, ")\n")
    } else {param_labels$param_label[param_labels$param == parameter]}

  clim_dat1$date2 <- as.Date(clim_dat1$date, format = c("%Y-%m-%d"))

  clim_dat2 <- clim_dat1 |> filter(param %in% parameter)

  year_len <- length(unique(clim_dat2$year))
  mon_len <- length(unique(clim_dat2$month))

  clim_dat2$date_chr <- as.character(clim_dat2$date2)

  seq_int <- if(any(parameter == "ppt")){20} else {2}

  clim_dat_final <- if(units == "sci"){clim_dat2
  } else if(units == "eng"){
      clim_dat2 |> mutate(value = ifelse(param == "ppt", value/25.4, (value * 9/5) + 32))
    }

  pal <-
      if(length(palette) > 1){
        rep(colorRampPalette(palette)(length(unique(param))), times = length(parameter))
      } else { # hack to allow gradient to work with 1 color
        rep(colorRampPalette(c(palette, palette))(length(unique(param))), times = length(parameter))
      }

  num_parks <- length(unique(clim_dat_final$UnitCode))
  num_params <- length(unique(clim_dat_final$param_label))

  every_nth = function(n) {
    return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
  }

  # head(clim_dat_final)
  # table(clim_dat_final$date)
  #--- Create plot --
  climbarplot <-
    ggplot(clim_dat_final, aes(x = date_chr, y = value,
                               group = if(num_parks > 1 & num_params == 1){UnitCode
                               } else if(num_parks == 1 & num_params > 1){param_label
                               } else if(num_parks > 1 & num_params > 1){interaction(param_label, UnitCode)
                               } else {param_label},
                               color = if(num_parks > 1 & num_params == 1){UnitCode
                               } else if(num_parks == 1 & num_params > 1){param_label
                               } else if(num_parks > 1 & num_params > 1){interaction(param_label, UnitCode)
                               } else {param_label},
                               fill = if(num_parks > 1 & num_params == 1){UnitCode
                               } else if(num_parks == 1 & num_params > 1){param_label
                               } else if(num_parks > 1 & num_params > 1){interaction(param_label, UnitCode)
                               } else {param_label})) +
      # layers
      geom_bar(stat = 'identity', alpha = 0.6) +
      # themes
      theme_NETN() +
      theme(legend.position = legend_position,
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      {if(any(gridlines %in% c("grid_y", "both"))){
        theme(
          panel.grid.major.y = element_line(color = 'grey'))}} + #,
      {if(any(gridlines %in% c("grid_x", "both"))){
        theme(
          panel.grid.major.x = element_line(color = 'grey'))}} +#,
      # facets
      {if(facetparam == FALSE & facetpark == TRUE){facet_wrap(~UnitName, ncol = numcol)}}+
      {if(facetparam == TRUE & facetpark == FALSE){facet_wrap(~param_label, scales = facet_y, ncol = numcol)}}+
      {if(facetparam == TRUE & facetpark == TRUE){facet_wrap(~UnitName + param_label, ncol = numcol)}}+
      # palettes
      scale_fill_manual(values = pal) +
      scale_color_manual(values = pal) +
      # axis format
      scale_x_discrete(breaks = every_nth(n = 6), labels = clim_dat_final$year, expand = x_pad) +
      # scale_x_date(breaks = datebreaks, labels = scales::label_date(date_format),
      #              expand = x_pad, limits = datelims) +
      scale_y_continuous(n.breaks = 8) +
      # labels/themes
      labs(x = NULL, y = ylab)

  return(#suppressWarnings(
   climbarplot
   )#)
}


