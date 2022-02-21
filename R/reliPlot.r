#' Plotting conditional reliabilities across theta levels
#'
#' This function takes a fitted mirt-model plots reliability estimates across all levels of theta. 
#' 
#' 
#' @param model an object of class `SingleGroupClass` returned by the function `mirt()`. 
#' @param color what color should the curve be?
#' @param theta_range range to be shown on the x-axis
#' @param title title for the plot (defaults to "Item Characteristic Curves")
#'
#' @return a ggplot
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import mirt
#' @export
#'
reliPlot <- function(model, 
                     color = "blue",
                     theta_range = c(-4,4), 
                     title = "Conditional reliability") {
  
  theta_range = seq(theta_range[1], theta_range[2], by = .01)
  
  theta <- matrix(theta_range)
  information <- testinfo(mod, theta)
  SE <- 1/(sqrt(information))
  rel <- 1-SE^2
  d <- data.frame(theta, information, SE, rel)
  
  d %>%
    ggplot(aes(x = theta, y = rel)) +
    geom_line(color = color) +
    labs(x = expression(theta), y = expression(r_xx(theta)),
         title = title) +
    ylim(0, 1) +
    theme_minimal()
  
}
