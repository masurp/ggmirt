#' A quick summary of IRT analyses
#'
#' This function is essentially just a wrapper around several functions in this package and produces a summary of the most important aspects of an IRT model, including an item-person-map, item characteristic curves, test information curve, and conditional reliability.
#' 
#' @param model an object of class `SingleGroupClass` returned by the function `mirt()`. 
#' @param data the data frame used to estimate the IRT model.
#'
#' @return a plot grid as returned by `cowplot::ggdraw()`
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import mirt
#' @export
#'
#' @examples
#' library(mirt)
#' library(ggmirt)
#' data <- expand.table(LSAT7)
#' (mod <- mirt(data, 1))
#' 
#' summaryPlot(mod, data = data, theta_range = c(-4.5, 3.5), adj_factor = 3.5)
#'
summaryPlot <- function(model, data,
                        theta_range = c(-4, 4),
                        adj_factor = .05) {
  
  p1 <- itempersonMap(model, theta_range = theta_range, shape = 17, color = "red")
  p2 <- tracePlot(model, data = data, theta_range = theta_range, facet = FALSE)
  p3 <- testInfoPlot(model, theta_range = theta_range, adj_factor = adj_factor)
  p4 <- scaleCharPlot(model, theta_range = theta_range)
  
  
  p <- cowplot::ggdraw() +
    cowplot::draw_plot(p1, x = 0, y = 0.025, width = .50, height = 1) +
    cowplot::draw_plot(p2, x = .52, y = .66, width = .465, height = .33) +
    cowplot::draw_plot(p3, x = .52, y = .33, width = .48, height = 0.33) +
    cowplot::draw_plot(p4, x = .52, y = 0, width = .465, height = 0.32) 
  
  return(p)
  
}

