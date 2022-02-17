#' Person parameter distribution
#'
#' This function requires a fitted mirt-model of class `SingleGroupClass` to visualize a person parameter distribution (theta levels in the studied population). The resulting ggplot can be further customized (e.g., with regard to theme, labels, etc.). It works with both uni- and multidimensional models.
#' 
#' @param model an object of class `SingleGroupClass` returned by the function `mirt()`. 
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
#' personDens(mod)
#' 
#' # Customized plot
#' personDist(mod, 10) +
#'   xlim(-3, 3) +
#'   theme_classic()
personDist <- function(model, 
                     bins = 35) {
  
  person.params <- fscores(model, QMC = TRUE) %>%
    as.data.frame() 
  
  if(length(person.params) != 1) {
    p <- person.params %>%
      tidyr::pivot_longer(names(.), names_to = "dimension") %>%
      ggplot(aes(x = value, fill = dimension)) +
      geom_histogram(bins = bins, color = "white")
      
  } else {
  
  p <- person.params %>%
    pivot_longer(names(.), names_to = "dimension") %>%
    ggplot(aes(x = value, fill = dimension)) +
    geom_histogram(bins = bins, color = "white") +
    guides(fill = F)
  }
  
  p + theme_minimal() + labs(x = "θ")
  
}

personDens <- function(model) {
  
  person.params <- fscores(model, QMC = TRUE) %>%
    as.data.frame() 
  
  if(length(person.params) != 1) {
    p <- person.params %>%
      pivot_longer(names(.), names_to = "dimension") %>%
      ggplot(aes(x = value, fill = dimension)) +
      geom_density()
    
  }
  
  p <- person.params %>%
    pivot_longer(names(.), names_to = "dimension") %>%
    ggplot(aes(x = value)) +
    geom_density() +
    guides(fill = F)
  
  p + theme_minimal() + labs(x = "θ")
  
}

