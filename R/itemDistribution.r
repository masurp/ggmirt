#' Item difficulty distribution
#'
#' This function requires a fitted mirt-model of class `SingleGroupClass` to visualize item difficulty distribution. Currently only works for unidimensional models.
#' 
#' @param model an object of class `SingleGroupClass` returned by the function `mirt()`. 
#' @param ... any argument passed to `geom_point()`
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
#' itemDist(mod)
#' 
#' # Customized plot
#' itemDist(mod, size = 3, shape = 17, color = "blue")
#' 
itemDist <- function(model,
                     ...) {
  
  item.params <- mirt::coef(mod, IRTpars = TRUE, simplify = TRUE) %>%
    as.data.frame %>%
    tibble::rownames_to_column("items") 
  
  p <- item.params %>%
    mutate(items = forcats::fct_reorder(items, items.b)) %>%
    ggplot(aes(y = items, x = items.b)) + 
    geom_point(...)
  
  p + theme_minimal() + labs(x = expression(theta), y = "")
}

