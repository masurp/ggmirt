#' Plotting item characteristics curves
#'
#' This function takes a fitted mirt-model and the underlying data and visualizes item characteristic curves.
#' 
#' 
#' @param model an object of class `SingleGroupClass` returned by the function `mirt()`. 
#' @param items numerical vector indicating which items to plot (currently does not yet work for graded response models).
#' @param theta_range range to be shown on the x-axis
#' @param n.answers In a graded response model, number of answer options (e.g., 5-point scale = 5)
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
#' tracePlot(mod)
#' tracePlot(mod, items = c(1,2,3), theta_range = c(-5,5), facet = F, legend = T)
#'
tracePlot <- function(model, 
                      items = NULL,
                      theta_range = c(-4,4),
                      title = "Item Characteristics Curves",
                      n.answers = 5,
                      facet = TRUE,
                      legend = FALSE) {
  
  data <- model@Data$data %>% as.data.frame
  
  # Set theta range as sequence
  theta_range = seq(theta_range[1], theta_range[2], by = .01)
  
  # Check model type
  type <- model@Model$itemtype
  
  # Graded response model
  if(type[1] == "graded") {
    
    trace <- probtrace(model, Theta = theta_range) %>%
      as_tibble %>%
      mutate(Theta = theta_range) %>%
      gather(key, value, -Theta) %>%
      separate(key, c("var", "response"), sep = ifelse(n.answers > 10, -4, -3))
    
    p <- ggplot(trace, aes(x = Theta, y =  value)) +
      geom_line(aes(color = response)) +
      facet_wrap(~var) +
      theme_minimal() +
      labs(x = expression(theta), 
           y = expression(P(theta)), 
           title = title) 
    
    # in cases where there are max 9 items: use a specified color palette
    if (length(unique(trace$var)) < 10) {
      p <- p +
        scale_color_brewer(palette = 7)
    }
    
    
  } else {
  
  trace <- NULL
  for(i in 1:length(data)){
  extr <- extract.item(model, i)
  theta <- matrix(theta_range)
  trace[[i]] <- probtrace(extr, theta)
  }
  
  if (!is.null(items)) {
    trace <- trace[items]
  }
  
  names(trace) <- paste('item', 1:length(trace))
  trace_df <- do.call(rbind, trace)
  
  item <- rep(names(trace), each = length(theta))
  d <- cbind.data.frame(theta, item, trace_df)
  d$item <- factor(d$item, levels = c(paste('item', 1:length(trace))))
  
  # final plot
  if(isFALSE(facet)) {
  p <- ggplot(d, aes(theta, P.1, colour = item)) + 
    geom_line() + 
    labs(x = expression(theta), 
         y = expression(P(theta)), 
         title = title) +
    theme_minimal() 
  
  # in cases where there are max 9 items: use a specified color palette
  if (length(unique(d$item)) < 10) {
    p <- p +
      scale_color_brewer(palette = 7)
  }
  
  if(isFALSE(legend)) {
   p <- p + guides(color = "none")
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
  
  }
  return(p)
}
