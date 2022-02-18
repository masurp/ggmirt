#' Plotting itemfit estimates 
#'
#' This function takes a fitted mirt-model and visualizes item infit and outfit estimates. The function builds on `mirt::itemfit()`. Currently only supported `fact_stats = "infit"`. 
#' 
#' 
#' @param model an object of class `SingleGroupClass` returned by the function `mirt()`. 
#' @param fit_stats a character vector indicating which fit statistics should be computed. See `mirt::infit()` for supported inputs. 
#' @param color color of the item points.
#' @param shape shape of the item points
#' @param title title for the plot (defaults to "Item Infit and Outfit Statistics")
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
#' itemfitPlot(mod, fit_stats = "infit")
#'
itemfitPlot <- function(model,
                        fit_stats = "infit",
                        color = "red",
                        shape = 17,
                        title = "Item Infit and Outfit Statistics",
                        ...) {
    
  
    fit <- mirt::itemfit(model, fit_stats = fit_stats, ...)
    
    if("infit" %in% names(fit)) {
      fit %>%
        select(item, infit, outfit) %>%
        gather(key, value, -item) %>%
        ggplot(aes(x = item, y = value)) +
        geom_point(size = 3, color = color, shape = shape) +
        geom_line() +
        geom_hline(yintercept = .5, color = "darkgrey", linetype = "dashed") +
        geom_hline(yintercept = 1, color = "darkgrey") +
        geom_hline(yintercept = 1.5, color = "darkgrey", linetype = "dashed") +
        scale_y_continuous(breaks = c(.5, 1, 1.5), limits = c(0, 2)) +
        facet_grid(~key) +
        coord_flip() +
        theme_minimal() +
        labs(y = "", x = "", caption = "Note: Items with values within 0.5 and 1.5 are considered to be productive for measurement.",
             title = title)
    }
}



