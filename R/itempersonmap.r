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
#' itempersonMap(mod, 
#'               limits = c(-3, 3), 
#'               color = "red", 
#'               theme = theme_bw(), 
#'               shape = 17)
#'
itempersonMap <- function(model,
                          limits = c(-4,4),
                          title = "Item Person Map",
                          margin = c(1,0,-1.5,0),
                          density = FALSE,
                          theme = theme_minimal(),
                          ...) {
  
  p1 <- personDist(model, density = density) + xlim(limits) + theme + theme(plot.margin = unit(margin,"cm")) + labs(title = title)
  p2 <- itemDist(model, ...) + xlim(limits) + theme
  
  
  p <- cowplot::plot_grid(p1, p2,
                          nrow = 2,
                          rel_heights = c(1.5,2.5),
                          align = "hv",
                          axis = "tlbr")
  
  return(p)
}

