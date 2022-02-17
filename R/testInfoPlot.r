#' Plotting test information curve
#'
#' This function takes a fitted mirt-model and visualizes test information curve.
#' 
#' 
#' @param model an object of class `SingleGroupClass` returned by the function `mirt()`. 
#' @param theta_range range to be shown on the x-axis
#' @param adj_factor adjustment factor for properly overlaying information and standard error.
#' @param title title for the plot (defaults to "Item Characteristic Curves")
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
#' (mod <- mirt(data, 1))
#' 
#' testInfoPlot(mod)
#'
testInfoPlot <- function(model, 
                         theta_range = seq(-4,4,.01), 
                         adj_factor = 3.5,
                         title = "Test Information Curve") {

Theta <- matrix(theta_range)
information <- testinfo(model, Theta)
SE <- 1/(sqrt(information))

p <- data.frame(Theta, information, SE) %>%
  mutate(SE = SE/adj_factor) %>%
  gather(key, value, -Theta) %>%
  ggplot() +
  geom_line(aes(x = Theta, y = value, color = key, linetype = key)) +
  scale_linetype_manual(values=c("dashed", "solid"))+
  scale_y_continuous(sec.axis = sec_axis(~.*adj_factor, name = expression(SE(theta)))) +
  labs(x = expression(theta), y = expression(I(theta)),
       title = title) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank())

return(p)

}
