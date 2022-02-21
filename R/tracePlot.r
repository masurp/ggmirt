#' Plotting item characteristics curves
#'
#' This function takes a fitted mirt-model and the underlying data and visualizes item characteristic curves.
#' 
#' 
#' @param model an object of class `SingleGroupClass` returned by the function `mirt()`. 
#' @param data the data frame used to estimate the IRT model.
#' @param theta_range range to be shown on the x-axis
#' @param title title for the plot (defaults to "Item Characteristic Curves")
#' @param facet Should all items be shown in one plot, or each item received its individual facet?
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
#' tracePlot(mod, data, theta_range = c(-5,5), facet = F, legend = T)
#'
tracePlot <- function(model, data, 
                      theta_range = c(-4,4),
                      title = "Item Characteristics Curves",
                      facet = TRUE,
                      legend = FALSE) {
  
  theta_range = seq(theta_range[1], theta_range[2], by = .01)
  
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
  p <- ggplot(d, aes(theta, P.1, colour = item)) + 
    geom_line() + 
    labs(x = expression(theta), 
         y = expression(P(theta)), 
         title = title) +
    theme_minimal()
  
  if(isFALSE(legend)) {
   p <- p + guides(color = FALSE)
  }
    
  } else {
   p <- ggplot(d, aes(theta, P.1)) + 
      geom_line() + 
      facet_wrap(~item) +
      labs(x = expression(theta), 
           y = expression(P(theta)), 
           title = title) +
      theme_minimal()
  }
  
  return(p)
}
