#' Comparing test information curves of parallel tests
#'
#' This function takes two fitted mirt-model and visualizes test their test information curves on top of each other. This can be helpful for finding parallel tests.
#' 
#' 
#' @param model1 an object of class `SingleGroupClass` returned by the function `mirt()`. 
#' @param model2 an object of class `SingleGroupClass` 
#' @param title title for the plot
#' @param subtitle subtitle for the plot
#'
#' @return a ggplot
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
#' (mod1 <- mirt(data, 1))
#' (mod2 <- mirt(data[,1:4], 1))
#' 
#' testInfoCompare(mod1, mod2)
#'
testInfoCompare <- function(model1, model2, 
                          theta_range = c(-4,4), 
                          title = "Parallel Tests",
                          subtitle = "Test Information Curves") {
  
  theta_range = seq(theta_range[1], theta_range[2], by = .01)
  
  Theta <- matrix(theta_range)
  information1 <- testinfo(model1, Theta)
  information2 <- testinfo(model2, Theta)
  
  p <- data.frame(Theta, information1, information2) %>%
    gather(key, value, -Theta) %>%
    ggplot() +
    geom_line(aes(x = Theta, y = value, color = key), alpha = .75) +
    labs(x = expression(theta), y = expression(I(theta)),
         title = title, subtitle = subtitle) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    theme(legend.title=element_blank())
  
  return(p)
  
}
