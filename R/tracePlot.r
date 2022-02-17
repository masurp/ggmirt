#' Plotting item characteristics curves
#'
#' This function takes a fitted mirt-model and the underlying data and visualizes item characteristic curves.
#' 
#' 
#' @param model an object of class `SingleGroupClass` returned by the function `mirt()`. 
#' @param data the data frame used to estimate the IRT model.
#' @param theta_range range to be shown on the x-axis
#' @param title title for the plot (defaults to "Item Characteristic Curves")
#' @param facet Should all items be shown in one plot, or each item received its individal facet?
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
#' tracePlot(mod, data)
#' tracePlot(mod, data, theta_range = seq(-5,5, by = .1), facet = F)
#'
tracePlot <- function(model, data, 
                      theta_range = seq(-4,4, by = .1),
                      title = "Item Characteristics Curves",
                      facet = T) {
  
  trace <- NULL
  for(i in 1:length(data)){
  extr <- extract.item(model, i)
  theta <- matrix(theta_range)
  trace[[i]] <- probtrace(extr, theta)
  }
  
  names(trace) <- paste('item', 1:length(trace))
  trace_df <- do.call(rbind, trace)
  
  item <- rep(names(trace), each = length(theta))
  d <- cbind.data.frame(theta, item, trace_df)
  d$item <- as.factor(d$item)
  
  # final plot
  if(isFALSE(facet)) {
  ggplot(d, aes(theta, P.1, colour = item)) + 
  geom_line() + 
  labs(x = expression(theta), 
       y = expression(P(theta)), 
       title = title) +
  theme_minimal()
    
  } else {
    ggplot(d, aes(theta, P.1)) + 
      geom_line() + 
      facet_wrap(~item) +
      labs(x = expression(theta), 
           y = expression(P(theta)), 
           title = title) +
      theme_minimal()
  }
}
