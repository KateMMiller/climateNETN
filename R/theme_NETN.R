#' @title theme_NETN: custom ggplot2 theme for climateNETN
#'
#' @import ggplot2
#'
#' @description This function customizes a theme for plotting NETN climate data, including removing the
#' default panel grids from ggplot2 figures.
#'
#' @return This function must be used in conjunction with a ggplot object
#'
#' @export


theme_NETN <- function(){theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.background = element_rect(color = '#696969', fill = 'white', linewidth = 0.4),
                               plot.background = element_blank(),
                               strip.background = element_rect(color = '#696969', fill = 'grey90', linewidth = 0.4),
                               legend.key = element_blank(),
                               axis.line.x = element_line(color = "#696969", linewidth = 0.4),
                               axis.line.y = element_line(color = "#696969", linewidth = 0.4),
                               axis.ticks = element_line(color = "#696969", linewidth = 0.4)
)}
