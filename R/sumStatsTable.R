#' @include theme_NETN.R
#' @include getClimNOAA.R
#'
#' @title sumStatsTable: Summarize climate records
#'
#' @importFrom dplyr arrange filter group_by mutate select slice_max slice_min
#' @importFrom purrr list_rbind map
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @description This function summarizes climate records by month, including wettest, dryest,
#' warmest and coolest based on monthly values dating back to 1895 for a given park.
#'
#' @param park Specify park to plot. Currently can only plot 1 park at a time. Valid inputs:
#' \describe{
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
#' @param years Numeric. Years to consider for summary table. Accepted values start at 1895.
#'
#' @param normal Specify normal to plot. By default, the 20th century normal (1901-2000) plots.
#' Other options include:
#' \describe{
#' \item{"norm20cent"}{Plots the 20th century normal (1901 - 2000)}
#' \item{"norm1990"}{Plots the 30-year norm from 1991 - 2020}
#' }
#'
#' @param units Specify if you want Scientific or English units. Acceptable values are "sci" (default) and "eng".
#' If "sci", temperature units are in C and precipitation units are in mm. If "eng", temperature units are in F,
#' and precipitation units are in inches.
#'
#' @param top_n Number of rows to return per statistic. Default is 5.
#'
#' @param kable Logical. If TRUE (default) will return a kable with formatting. If FALSE, will return a data.frame.
#'
#' @examples
#' \dontrun{
#'
#' # Generate html table of top 5 records in ACAD for all years in English
#' sumStatsTable(park = "ACAD", units = "eng", top_n = 5, kable = T)
#'
#' # Return dataframe of top 3 records in WEFA for all years in scientific units
#' sumStatsTable(park = "WEFA", top_n = 3, kable = F)
#'
#' # Generate
#'}
#'
#' @return Returns a dataframe or kable of climate statistics
#'
#' @export
#'
sumStatsTable <- function(park = "ACAD",
                          years = 1895:format(Sys.Date(), "%Y"),
                          normal = "norm20cent",
                          units = "sci",
                          top_n = 5,
                          kable = T){

  #-- Error handling --
  park <- match.arg(park, c("ACAD", "BOHA", "MABI", "MIMA", "MORR",
                            "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))

  #if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 1895 & years <= format(Sys.Date(), "%Y"))
  normal <- match.arg(normal, c("norm20cent", "norm1990"))
  stopifnot(class(top_n) %in% c("numeric", "integer"), top_n >= 1)
  stopifnot(class(kable) == "logical")
  if(kable == TRUE){
    if(!requireNamespace("kableExtra", quietly = TRUE)){
      stop("Package 'kableExtra' needed if kable = TRUE. Please install it.",
           call. = FALSE)}

      if(!requireNamespace("knitr", quietly = TRUE)){
        stop("Package 'knitr' needed if kable = TRUE. Please install it.",
             call. = FALSE)}
  }
  #-- Compile data for plotting --
  # Clim data as annual monthly normal
  data("NETN_clim_annual")
  data("NETN_clim_norms")

  # Prepare annual data for summary
  clim_dat <- NETN_clim_annual |> dplyr::filter(UnitCode %in% park) |> dplyr::filter(year %in% years) |>
    dplyr::select(UnitCode, UnitName, year, month, ppt, tmax, tmin, tmean)

  clim_dat$date <- as.Date(paste0(clim_dat$year, "-", clim_dat$month, "-", 15), format = "%Y-%m-%d")

  avg_dat <- NETN_clim_norms |> dplyr::filter(UnitCode %in% park) |> arrange(month)
  avg_dat$mon <- factor(avg_dat$month, levels = sort(unique(avg_dat$month)),
                        labels = unique(month.abb[avg_dat$month]))


  norm_cols <- if(normal == "norm20cent"){c("mon", "ppt_norm_1901_2000", "tmean_norm_1901_2000")
  } else {c("mon", "ppt_norm_1991_2020", "tmean_norm_1991_2020")}

  avg_dat2 <- avg_dat[,norm_cols]
  colnames(avg_dat2) <- c("mon", "ppt", "tmean")

  if(units == "eng"){
    avg_dat2$ppt <- avg_dat2$ppt/25.4
    avg_dat2$tmean <- (avg_dat2$tmean * 9/5) + 32
  }

  avg_dat_wide <- rbind(
                    avg_dat2 |> select(-tmean) |>
                      pivot_wider(names_from = mon, values_from = ppt) |>
                      mutate(param = "ppt"),
                    avg_dat2 |> select(-ppt) |>
                      pivot_wider(names_from = mon, values_from = tmean) |>
                      mutate(param = "tmean")
                    )



  # Update clim data if requesting a year x month combination that is not currently in
  # the saved NETN_clim_annual.rda but only for complete months
  date_range_data <- sort(unique(clim_dat$date))
  months = 1:12
  date_range_fxn <- paste0(rep(years, each = length(months)),"-",
                           rep(sprintf("%02d", months), length(years)), "-", 15)
  new_dates1 <- date_range_fxn[!date_range_fxn %in% date_range_data]

  # latest date of complete month
  mon_curr <- as.numeric(format(Sys.Date(), "%m"))
  mon_next_day <- as.numeric(format(Sys.Date() + 1, "%m"))
  mon_comp <- ifelse(mon_next_day > mon_curr, sprintf("%02d", mon_curr),
                     sprintf("%02d", mon_curr - 1))
  latest_date_comp <- as.Date(paste0(format(Sys.Date(), "%Y"), "-", mon_comp, "-", 15), format = "%Y-%m-%d")
  latest_date_data <- as.Date(max(date_range_data), format = "%Y-%m-%d")

  new_dates <- as.Date(new_dates1[new_dates1 <= latest_date_comp], format = "%Y-%m-%d")
  #new_dates <- as.Date(c("2024-05-15", "2024-04-15"), format = "%Y-%m-%d")

  clim_dat2 <-
    if(length(new_dates) == 0 || is.na(new_dates)){clim_dat
    } else {
      new_months <- as.numeric(format(new_dates, "%m"))
      new_years <- as.numeric(format(new_dates, "%Y"))
      new_clim <- purrr::map(new_years, function(y){
        getClimNOAA(park = park, year = y, months = new_months)}
      ) |> list_rbind()
      if(nrow(new_clim) > 0){
        new_clim$date <- as.Date(paste0(
          new_clim$year, "-", new_clim$month, "-", 15), format = "%Y-%m-%d")
        comb_clim <- rbind(clim_dat, new_clim[,names(clim_dat)])
      } else {clim_dat}
    }

  clim_dat_long <- clim_dat2 |>
    pivot_longer(cols = ppt:tmean, names_to = "param", values_to = "value")

  if(units == "eng"){clim_dat_long$value <-
    ifelse(clim_dat_long$param == "ppt", clim_dat_long$value/25.4, (clim_dat_long$value * 9/5) + 32)}

  clim_max <- clim_dat_long |> group_by(UnitCode, UnitName, month, param) |>
    slice_max(value, n = top_n, with_ties = F) |>
    mutate(stat = paste0("max"),
           rank = order(param, decreasing = T)) |>
    ungroup()

  clim_min <- clim_dat_long |> group_by(UnitCode, UnitName, month, param) |>
    slice_min(value, n = top_n, with_ties = F) |>
    mutate(stat = paste0("min"),
           rank = order(param, decreasing = F)) |>
    ungroup()

  clim_sum <- rbind(clim_max, clim_min)

  clim_sum$mon <- factor(clim_sum$month, levels = sort(unique(clim_sum$month)),
                         labels = unique(month.abb[clim_sum$month]))

  clim_wide_yr <- clim_sum |> select(-date, -value, -month) |>
    pivot_wider(names_from = mon, values_from = year)

  clim_wide_val <- clim_sum |> select(-date, -year, -month) |>
    pivot_wider(names_from = mon, values_from = value)

  temp_units <- ifelse(units == "sci", "C", "F")
  ppt_units <- ifelse(units == "sci", "mm", "in")

  param_labels <- data.frame(label = c(paste0("Record High Total Precip. (", ppt_units, ")"),
                                       paste0("Record Low Total Precip. (", ppt_units, ")"),
                                       # paste0("Warmest Max. Temp (", temp_units, ")"),
                                       # paste0("Coldest Min. Temp (", temp_units, ")")),
                                       paste0("Warmest Avg. Temp (", temp_units, ")"),
                                       paste0("Coldest Avg. Temp (", temp_units, ")")),
                             param = c("ppt", "ppt", #"tmax", "tmin"),
                                       "tmean", "tmean"),
                             stat = c("max", "min", #"max", "min"),
                                      "max", "min"),
                             order = c(1, 2, 3, 4))#, 5, 6))

  clim_yr <- left_join(param_labels, clim_wide_yr, by = c("param", "stat"))

  clim_comb <- left_join(clim_yr, clim_wide_val, by = c("UnitCode", "UnitName", "param", "stat", "rank"),
                         suffix = c("_yr", "_val"))

  # Functions for kable conditional formatting and tooltips
  col_fxn <- function(col){
    ifelse(clim_comb2[,col] >= 2020, "red", "black")
  }

  tool_fxn <- function(col){
    units <- ifelse(clim_comb2$param == "ppt", ppt_units, temp_units)
    avg_mon <- substr(col, 1, 3)
    avg_df1 <- do.call("rbind", replicate(top_n * 2, avg_dat_wide, simplify = F)) |>
      arrange(param) |> data.frame()
    avg_df <- avg_df1[,avg_mon]

    paste0("Record: ", format(round(clim_comb2[,col], 1), nsmall = 1), " (", units, "); ",
           "\n\n",
           "Average: ", format(round(avg_df, 1), nsmall = 1), " (", units, ")",
           "\n")
  }

  clim_final <-
  if(kable == T){
    clim_comb2 <- clim_comb |> arrange(UnitName, order, rank)

    clim_kab <-
    knitr::kable(clim_comb2 |> select(label, rank, Jan_yr:Dec_yr),
                 format = "html",
                 col.names = c("Statistic", "Rank",
                               "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                 align = c("l", rep("c", ncol(clim_comb2)))) |>
    kableExtra::kable_styling(bootstrap_options = c("condensed", "bordered", "hover"),
                              full_width = FALSE) |>
    kableExtra::column_spec(3, color = col_fxn("Jan_yr"), tooltip = tool_fxn("Jan_val")) |>
    kableExtra::column_spec(4, color = col_fxn("Feb_yr"), tooltip = tool_fxn("Feb_val")) |>
    kableExtra::column_spec(5, color = col_fxn("Mar_yr"), tooltip = tool_fxn("Mar_val")) |>
    kableExtra::column_spec(6, color = col_fxn("Apr_yr"), tooltip = tool_fxn("Apr_val")) |>
    kableExtra::column_spec(7, color = col_fxn("May_yr"), tooltip = tool_fxn("May_val")) |>
    kableExtra::column_spec(8, color = col_fxn("Jun_yr"), tooltip = tool_fxn("Jun_val")) |>
    kableExtra::column_spec(9, color = col_fxn("Jul_yr"), tooltip = tool_fxn("Jul_val")) |>
    kableExtra::column_spec(10, color = col_fxn("Aug_yr"), tooltip = tool_fxn("Aug_val")) |>
    kableExtra::column_spec(11, color = col_fxn("Sep_yr"), tooltip = tool_fxn("Sep_val")) |>
    kableExtra::column_spec(12, color = col_fxn("Oct_yr"), tooltip = tool_fxn("Oct_val")) |>
    kableExtra::column_spec(13, color = col_fxn("Nov_yr"), tooltip = tool_fxn("Nov_val")) |>
    kableExtra::column_spec(14, color = col_fxn("Dec_yr"), tooltip = tool_fxn("Dec_val")) |>
    kableExtra::row_spec(c(0, 5, 10, 15, 20), extra_css = "border-bottom: 1px solid lightgrey;") |>
    kableExtra::collapse_rows(1, valign = "top")

  } else {clim_comb}

  return(clim_final)

  }



