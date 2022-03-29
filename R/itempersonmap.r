#' Visualize item person map and scale properties based on Rasch model
#'
#' This function takes a fitted mirt-model and visualizes and plots item-person-map (also known as Kernel-Density Plots or Wright maps) on the left, and add a scale characteristic curve, scale information curve, and a marginal reliability curve on the right.
#' 
#' 
#' @param model an object of class `SingleGroupClass` returned by the function `mirt()`. 
#' @param limits range to be shown on the x-axis
#' @param title title for the plot (defaults to "Item-Person-Map")
#' @param margin margins around the top figure. Sometimes one might want to adjust this.
#' @param density logical value indicating whether a smoothed density curve or a standard histogram should be plotted.
#' @param color color of the geoms, defaults to "red".
#' @param shape can be used to change the shape of the geom, defaults to triangles (17).
#' @param size size of the geom, default to 3.  
#' @param theme any ggplot theme. 
#' @param ... any argument passed to `geom_point()`.
#'
#' @return a plot grid as returned by `cowplot::plot_grid()`
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
#' itempersonMap(mod)
#'
itempersonMap <- function(model,
                          theta_range = c(-4,4),
                          title = "Item Person Map",
                          margin = c(1,0,-1.5,0),
                          density = FALSE,
                          color = "red",
                          shape = 17,
                          size = 3,
                          theme = theme_minimal(),
                          ...) {
  
  p1 <- personDist(model, theta_range = theta_range, density = density) + 
    theme + 
    theme(plot.margin = unit(margin,"cm")) + 
    labs(title = title)
  p2 <- itemDist(model, theta_range = theta_range, shape = shape, color = color, size = size, ...) + 
    theme
  
  
  p <- cowplot::plot_grid(p1, p2,
                          nrow = 2,
                          rel_heights = c(1.5,2.5),
                          align = "hv",
                          axis = "tlbr")
  
  return(p)
}

