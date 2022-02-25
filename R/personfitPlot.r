#' Plotting personfit estimates 
#'
#' This function takes a fitted mirt-model and visualizes person infit and outfit estimates. The function builds on `mirt::itemfit()`. The basic idea is to visualize how many individuals in the sample do not show a response pattern that aligns with the suggested model. At best, the number of non-fitting response patterns is low (e.g., < 5%).
#' 
#' 
#' @param model an object of class `SingleGroupClass` returned by the function `mirt()`. 
#' @param std logical value indicating whether standardized or non-standardized infit or outfit estimates should be used (leads to different cut-off values).
#' @param title title for the plot (defaults to "Person Infit and Outfit Statistics")
#'
#' @return a ggplot
#' 
#' @references \itemize{
#'  \item Linacre JM. (2002). What do Infit and Outfit, Mean-square and Standardized mean? Rasch Measurement Transactions, 16(2), p.878. https://www.rasch.org/rmt/rmt162f.htm
#' }
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
#' personfitPlot(mod, std = F)
#'
personfitPlot <- function(model,
                          std = TRUE,
                          title = "Person Infit and Outfit Statistics"){
  
   
  if(isTRUE(std)) {
    
     fit <- mirt::personfit(model) %>%
       dplyr::select(z.infit, z.outfit) %>%
       gather(key, value) %>%
       mutate(color_diff = ifelse(value < -1.96, "red",
                                  ifelse(value > 1.96, "red", "grey")))
     
     limits <- c(-1.96, 0, 1.96)
  } else {
       
     fit <- mirt::personfit(model) %>%
       dplyr::select(infit, outfit) %>%
       gather(key, value) %>%
       mutate(color_diff = ifelse(value < .5, "red",
                                  ifelse(value > 1.5, "red", "grey")))
     limits <- c(.5, 1, 1.5)
  }
  
  fit %>%
    ggplot(aes(x = value, fill = color_diff)) +
    geom_histogram(color = "white") +
    geom_vline(xintercept = limits[3], color = "darkgrey", linetype = "dashed") +
    geom_vline(xintercept = limits[2], color = "darkgrey") +
    geom_vline(xintercept = limits[1], color = "darkgrey", linetype = "dashed") +
    facet_wrap(~key) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(x = "",
         y = "",
         title = title)
}
