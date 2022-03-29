#' Scale Characteristic Curve
#'
#' Once model-based theta score estimates are computed, it often is of interest to transform those estimates into the original scale metric. A scale characteristic function provides a means of transforming estimated theta scores to expected true scores in the original scale metric. This transformation back into the original scale metric provides a more familiar frame of reference for interpreting scores. This function provides a visualization for this transformation. 
#' 
#' @param model an object of class `SingleGroupClass` returned by the function `mirt()`. 
#' @param theta_range range to be shown on the x-axis
#' @param color color of the line
#' @param title title for the plot (defaults to "Person Infit and Outfit Statistics")
#'
#' @return a ggplot
#' 
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
#' scaleCharPlot(mod)
#' 
scaleCharPlot <- function(model,
                          theta_range = c(-4, 4),
                          color = "red",
                          title  = "Scale Characteristic Curve") {

  theta <- seq(theta_range[1], theta_range[2], by = .01)
  score <- expected.test(model, matrix(theta))
  n.items <- model@Data$nitems
  
  d <- data.frame(theta, score)
  p <- ggplot(d, aes(x = theta, y = score)) +
    geom_line(color = color) +
    theme_minimal() +
    labs(x = expression(theta), y = expression(T(theta)), 
         title = title)
  
  return(p)
}


