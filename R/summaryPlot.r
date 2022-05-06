#' A quick summary of IRT analyses
#'
#' This function is essentially just a wrapper around several functions in this package and produces a summary of the most important aspects of an IRT model, including an item-person-map,test information curve, scale characteristic curve, and conditional reliability.
#' 
#' @param model an object of class `SingleGroupClass` returned by the function `mirt()`. 
#' @param theta_range range to be shown on the x-axis
#' @param adj_factor adjustment factor for properly overlaying information and standard error.
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
#' 
#' # Simulate some data
#' data <- sim_irt(500, 10, seed = 123)
#' 
#' # Run IRT model with mirt
#' mod <- mirt(data, 1, itemtype = "2PL", verbose = FALSE)
#' 
#' summaryPlot(mod, theta_range = c(-4.5, 3.5), adj_factor = 1.5)
#'
summaryPlot <- function(model,
                        theta_range = c(-4, 4),
                        adj_factor = .05) {
  
  # Get number of items
  J <- model@Data$nitems
  
  # Person parameter distribution
  p1 <- personDist(model, theta_range = theta_range) +
    labs(title = "Item Person Map")
  
  # Item difficulty distribution
  p2 <- itemDist(model, theta_range = theta_range, shape = 17, color = "red") 
  
  # Change item labelling if no. items > 10
  if(J > 10) {
    p2 <- p2 +
    geom_text(aes(label = items), nudge_x = .75, color = "darkgrey", size = 2, check_overlap = T) +
    theme(axis.text.y = element_blank(),
          panel.grid.major.y = element_blank())
  }
  
  # Scale Characteristic Curve
  p3 <- scaleCharPlot(model, theta_range = theta_range)
  
  # Test information curve
  p4 <- testInfoPlot(model, theta_range = theta_range, adj_factor = adj_factor)
  
  # Conditional reliability curve
  p5 <- conRelPlot(model, theta_range = theta_range)
  
  # Bind together
  p <- ggpubr::ggarrange(ggpubr::ggarrange(p1, p2, 
                                           ncol = 1, 
                                           align = "hv", 
                                           heights = c(1,2)), 
                         ggpubr::ggarrange(p4, p3, p5, 
                                           ncol = 1, 
                                           align = "hv", 
                                           heights = c(1.25, 1, 1)), 
                         ncol = 2)
  
  
  return(p)
  
}

