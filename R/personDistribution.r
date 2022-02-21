#' Person parameter distribution
#'
#' This function requires a fitted mirt-model of class `SingleGroupClass` to visualize a person parameter distribution (theta levels in the studied population). The resulting ggplot can be further customized (e.g., with regard to theme, labels, etc.). It works with both uni- and multidimensional models.
#' 
#' @param model an object of class `SingleGroupClass` returned by the function `mirt()`. 
#' @param density logical value indicating whether a smoothed density curve or a standard histogram should be plotted.
#' @param bins number of bins to be plotted in the histogram
#'
#' @return a ggplot object.
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import mirt
#' @export
#'
#' @examples
#' # Loading packages
#' library(mirt)
#' library(ggmirt)
#' 
#' # Getting data
#' data <- expand.table(LSAT7)
#' 
#' # Fitting a model 
#' (mod <- mirt(data, 1))
#' 
#' # Simple plot
#' personDist(mod)
#' personDist(mod, density = TRUE)
#' 
#' # Customized plot
#' personDist(mod, theta_range = c(-3, 3), bins = 10) +
#'   theme_classic()
personDist <- function(model, 
                       theta_range = c(-4, 4),
                       density = FALSE,
                       bins = 35) {
  
  person.params <- fscores(model, QMC = TRUE) %>%
    as.data.frame() 
  
  if(length(person.params) != 1) {
    p <- person.params %>%
      tidyr::pivot_longer(names(.), names_to = "dimension") %>%
      ggplot(aes(x = value, fill = dimension)) 
      
  } else {
  
  p <- person.params %>%
    pivot_longer(names(.), names_to = "dimension") %>%
    ggplot(aes(x = value, fill = dimension)) +
    guides(fill = F)
  }
  
  if(isTRUE(density)) {
    
    p <- p + geom_density()
    
  } else {
    
    p <- p + geom_histogram(bins = bins, color = "white")
  }

  
  p + xlim(theta_range) + theme_minimal() + labs(x = expression(theta))
  
}
