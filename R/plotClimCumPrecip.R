#' @include theme_NETN.R
#' @include getClimNOAA.R
#'
#' @title plotClimCumPrecip: Plot cumulative precip relative to historic
#'
#' @importFrom dplyr arrange filter group_by left_join mutate select
#' @importFrom tidyr pivot_longer pivot_wider
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
#' @param layers Options are "points", "lines", "bars", or "area" for Annual Values.
#' By default, only lines and points will plot. Bars or area should be selected on their own.
#'
#' @param palette Color palette for years. If only 1 color is specified and multiple years
#' are specified, all will be the same color.
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
#' @param include_error Logical. If TRUE (default), plots min/max, 95% and 50% error bands around the normal.
#' If FALSE, only plots the normal line.
#'
#' @param legend_row Integer. Specify number of rows to plot the legend. Default is 1.
#'
#' @examples
#' \dontrun{
#'
#' # Plot ACAD cumulative precipitation for 2020 through 2023 with gridlines on y axis and precip in inches
#' plotClimCumPrecip(park = "ACAD", years = 2020:2023, legend_position = 'bottom',
#'                  gridlines = "grid_y", units = 'eng')
#'
#' # Plot ACAD cumulative precipitation for 2014 through 2024 with legend 2 rows and points and lines
#' plotClimCumPrecip(park = "ACAD", years = 2014:2024, legend_position = 'bottom', legend_row = 2,
#' layers = c("points", "lines"))
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
                        layers = "lines",
                        normal = "norm20cent",
                        units = "sci",
                        palette = "#1e90ff",
                        plot_title = TRUE,
                        title_type = "UnitCode",
                        legend_position = 'right',
                        numcol = 3,
                        gridlines = 'none',
                        include_error = TRUE,
                        legend_row = 1){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "BOHA", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "all")){park = c("ACAD", "BOHA", "MABI", "MIMA", "MORR",
                                  "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  stopifnot(class(plot_title) == "logical")
  normal <- match.arg(normal, c("norm20cent", "norm1990"))
  stopifnot(class(numcol) %in% c("numeric", "integer"))
  title_type <- match.arg(title_type, c("UnitCode", "UnitName"))
  gridlines <- match.arg(gridlines, c("none", "grid_y", "grid_x", "both"))
  units <- match.arg(units, c("sci", "eng"))
  stopifnot(class(include_error) == 'logical')
  layers <- match.arg(layers, c("points", "lines", "area", "bars"), several.ok = TRUE)
  stopifnot(class(legend_row) %in% c("numeric", "integer"), legend_row > 0)

  #-- Compile data for plotting --
  # Clim data as annual monthly normal
  data("NETN_clim_annual", package = "climateNETN")
  data("NETN_clim_norms", package = "climateNETN")

  months = 1:12 # Cum only really works when all months included
  clim_dat1 <- NETN_clim_annual |> filter(UnitCode %in% park) |>
    dplyr::select(UnitCode, UnitName, ppt, year, month)
  clim_dat2 <- clim_dat1 |> filter(year %in% years)
  clim_dat2$date <- as.Date(paste0(clim_dat2$year, "-", clim_dat2$month, "-", 15), format = "%Y-%m-%d")

  # Update clim data if requesting a year x month combination that is not currently in
  # the saved NETN_clim_annual.rda but only for complete months
  date_range_data <- sort(unique(clim_dat2$date))
  date_range_fxn <- paste0(rep(years, each = length(months)),"-",
                           rep(sprintf("%02d", months), length(years)), "-", 15)
  new_dates1 <- date_range_fxn[!date_range_fxn %in% date_range_data]

  # latest date of complete month
  mon_curr <- as.numeric(format(Sys.Date(), "%m"))
  mon_next_day <- as.numeric(format(Sys.Date() + 1, "%m"))
  mon_comp <- ifelse(mon_next_day > mon_curr, sprintf("%02d", mon_curr), sprintf("%02d", mon_curr - 1))
  latest_date_comp <- as.Date(paste0(format(Sys.Date(), "%Y"), "-", mon_comp, "-", 15), format = "%Y-%m-%d")
  latest_date_data <- as.Date(max(date_range_data), format = "%Y-%m-%d")

  new_dates <- as.Date(new_dates1[new_dates1 <= latest_date_comp], format = "%Y-%m-%d")
  #new_dates <- as.Date(c("2024-05-15", "2024-04-15"), format = "%Y-%m-%d")

  clim_dat3 <-
    if(length(new_dates) == 0 || all(is.na(new_dates))){clim_dat2
    } else {
      new_months <- as.numeric(format(new_dates, "%m"))
      new_years <- as.numeric(format(new_dates, "%Y"))
      new_clim <- purrr::map(new_years, function(y){
        getClimNOAA(park = park, year = y, months = new_months)}
      ) |> list_rbind()
      if(nrow(new_clim) > 0){
        new_clim$date <- as.Date(paste0(
          new_clim$year, "-", new_clim$month, "-", 15), format = "%Y-%m-%d")
        comb_clim <- rbind(clim_dat2, new_clim)
      } else {clim_dat2}
    }

  # Clim data in decadal and 30-year norms
  avg_dat1 <- NETN_clim_norms |> filter(UnitCode %in% park)
  ppt_cols <- names(avg_dat1)[grep("pptcum", colnames(avg_dat1))]
  norm_cols <- if(normal == "norm20cent"){ppt_cols[grep("1901", ppt_cols)]
    } else {ppt_cols[grep("1991", ppt_cols)]}
  avg_dat <- avg_dat1[,c("UnitCode", "UnitName", "month", norm_cols)]

  avg_dat_long <- avg_dat |> pivot_longer(cols = -c(UnitCode, UnitName, month),
                                          names_to = "param_full", values_to = "value") |>
    mutate(param = sub("_.*", "", param_full),
           norm = ifelse(grepl(1901, param_full), "norm20cent", "norm1990")) |>
    arrange(UnitCode, param, month)

  avg_dat_long$stat <- sapply(strsplit(avg_dat_long$param_full, "_"), function(x) x[2])

  metric_df <- data.frame(stat = c("norm", "min", "max", "l95", "u95", "l50", "u50"),
                          metric_type = c("Average", "lower", "upper", "lower", "upper", "lower", "upper"))

  avg_dat_long <- left_join(avg_dat_long, metric_df, by = "stat")

  # Finalize data shape and format
  clim_curr <- clim_dat3 |>
    arrange(UnitCode, year, month) |>
    group_by(UnitCode, UnitName, year) |>
    mutate(cum_ppt_curr = cumsum(ppt))

  clim_curr$mon <- factor(clim_curr$month,
                          levels = unique(clim_curr$month),
                          labels = unique(month.abb[clim_curr$month]), ordered = T)

  clim_curr$mon <- factor(format(clim_curr$date, "%b"), month.abb, ordered = TRUE)
  clim_curr$mon <- clim_curr$mon[,drop = T]

  clim_curr_final <- if(units == "sci"){clim_curr
  } else {
    clim_curr |> mutate(cum_ppt_curr = cum_ppt_curr/25.4)
  }

  avg_dat3 <- if(units == "sci"){avg_dat_long
  } else {
    avg_dat_long |> mutate(value = value/25.4)
  }

  avg_dat3$mon <- factor(avg_dat3$month,
                         levels = unique( avg_dat3$month),
                         labels = unique(month.abb[ avg_dat3$month]), ordered = T)

  # Split norm from distribs.
  avg_dat_norm <- avg_dat3 |> filter(stat == "norm")
  avg_dat_dist <- avg_dat3 |> filter(stat != "norm")

  # Widen to have columns for lower and upper for bands
  avg_dat_dist$distrib <- ifelse(grepl("min|max", avg_dat_dist$stat), "d100",
                                 paste0("d", gsub("\\D", "", avg_dat_dist$stat)) )

  avg_dat_dist_wide <- avg_dat_dist |> filter(norm == normal) |>
    select(-stat, -param_full) |>
    pivot_wider(names_from = metric_type, values_from = value)

  avg_dat_dist_wide$distrib <- factor(avg_dat_dist_wide$distrib, levels = c("d100", "d95", "d50"))

  # set up plotting features
  ptitle <- if(length(unique(clim_curr$UnitCode)) == 1 & plot_title == TRUE){
    unique(clim_curr$UnitName)} else {NULL}

  units_ppt <- if(units == "sci"){"mm"} else {"in"}
  ylabel = paste0("Cumulative Monthly Precip. (", units_ppt, ")")


  clim_curr_final$park_facet <- if(title_type == "UnitCode"){clim_curr_final$UnitCode
    } else {clim_curr_final$UnitName}

  facet_park <- ifelse(length(park) > 1, TRUE, FALSE)
  facet_year <- ifelse(length(years) > 1, TRUE, FALSE)

  xaxis_breaks <- month.abb[months]

  avg_name <- ifelse(normal == "norm20cent",
                     "Climate Baseline: 1901 - 2000",
                     "Climate Baseline: 1991 - 2020")

  year_breaks = unique(clim_curr_final$year)

  # palette for bands
  band_values <- c("d100" = "#EDEDED", "d95" = "#D3D3D3", "d50" = "#AFAFAF",
                   "Average" = "black")#, "Current Year" = palette)

  band_shapes <- c("d100" = 21, "d95" = 21, "d50" = 21,
                   "Average" = 21)#, "Current Year" = 21)

  band_labels <-
    c("d100" = "Min/Max",
      "d95" = "95% range",
      "d50" = "50% range",
      "Average" = "Average")#,
#      "Current Year" = "Current Year")

  clim_curr_final$grp <- "Current Year"
  avg_dat_dist_wide$grp <- avg_dat_dist_wide$distrib
  avg_dat_norm$grp <- "Average"

  pptplot <-
    ggplot(data = clim_curr_final, aes(group = grp)) + theme_NETN() +
    {if(all(layers == "bars")){
      geom_bar(data = clim_curr_final, stat = 'identity',
               aes(y = cum_ppt_curr, x = mon, fill = grp),
               alpha = 0.8, fill = palette)}} +
    {if(all(layers == "area")){
      geom_ribbon(data = clim_curr_final,
                  aes(ymin = 0, ymax = cum_ppt_curr, x = mon,
                      fill = grp), fill = palette)}} +
    # normal ribbons
    {if(include_error == TRUE){
      geom_ribbon(data = avg_dat_dist_wide,
                  aes(ymin = lower, ymax = upper, x = mon,
                      fill = grp))}} +
    # normal line
    geom_line(data = avg_dat_norm,
              aes(x = mon, y = value,
                  linetype = grp,
                  color = grp),
              linewidth = 1, linetype = "longdash") +
    # layers for annual data
    {if(any(layers %in% "lines")){
      geom_line(data = clim_curr_final,
                aes(y = cum_ppt_curr, x = mon, color = grp, linetype = grp),
                lwd = 1, color = palette)}} +
    {if(any(layers %in% "points")){
      geom_point(data = clim_curr_final, #shape = 21,
                aes(y = cum_ppt_curr, x = mon, color = grp, fill = grp, shape = grp),
                color = palette, fill = palette, size = 2.5)}} +
    # manual scaling
    scale_color_manual(values = band_values, labels = band_labels,
                       name = avg_name, guide = 'legend',
                       aesthetics = c("fill", "color")) +
    scale_shape_manual(values = 21, labels = "Current Year", name = NULL) +
    #scale_color_manual(values = band_values, labels = band_labels, name = NULL) +
    #scale_linetype_manual(values = c("longdash", "solid"), labels = waiver(), name = NULL) +
    # facets
    {if(facet_year == FALSE & facet_park == TRUE){facet_wrap(~park_facet, ncol = numcol)}} +
    {if(facet_year == TRUE & facet_park == FALSE){facet_wrap(~year, ncol = numcol)}} +
    {if(facet_year == TRUE & facet_park == TRUE){facet_wrap(~park_facet + year, ncol = numcol)}} +
    # labels/themes
    labs(x = NULL, y = ylabel, group = NULL,
         fill = avg_name, color = avg_name, linetype = NULL,) +
    scale_x_discrete(breaks = xaxis_breaks, drop = F) +
    scale_y_continuous(n.breaks = 8) +
    {if(any(gridlines %in% c("grid_y", "both"))){
      theme(panel.grid.major.y = element_line(color = 'grey'))}} +
    {if(any(gridlines %in% c("grid_x", "both"))){
      theme(panel.grid.major.x = element_line(color = 'grey'))}} +
      theme(legend.position = legend_position,
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            legend.text = element_text(size = 9),
            legend.title = element_text(size = 9),
            legend.margin = margin(0,0,0,0),
            legend.key.width = unit(0.85, "cm")
            ) +
    {if(any(layers %in% c("bars", "area"))){
      guides(color = guide_legend(order = 2),
             fill = guide_legend(order = 3),
             linetype = guide_legend(order = 1))}} +
    {if(any(layers %in% c("points", "lines"))){
    guides(
           fill = guide_legend(order = 0),
           color = guide_legend(order = 0),
           #shape = guide_legend(order = 4),
           linetype = guide_legend(order = 0))}} +
    guides(#linetype = guide_legend(keywidth = unit(5, "cm")),
           color = guide_legend(nrow = legend_row),
           fill = guide_legend(nrow = legend_row))

  return(#suppressWarnings(
    pptplot
  )
  #)
}


