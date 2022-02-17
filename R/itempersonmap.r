#' Visualize item person map and scale properties based on Rasch model
#'
#' This function takes a fitted mirt-model and visualizes and plots item-person-map (also known as Kernel-Density Plots or Wright maps) on the left, and add a scale characteristic curve, scale information curve, and a marginal reliability curve on the right.
#' 
#' 
#' @param model an object of class `SingleGroupClass` returned by the function `mirt()`. 
#' @param bin_color color of the bins and points in the plot
#' @param no_bins number of bins to be displayed
#' @param title_left title of the item-person-map on the left. 
#' @param title_right title of the scales properties on the left (defaults to none).  
#' @param scale_prop logical value indicating whether scale properties should be plotted at all.
#'
#' @return a ggplot object.
#' 
#' @export
#'
#' @examples
#' library(mirt)
#' data <- expand.table(LSAT7)
#' (mod1 <- mirt(data, 1))
#' 
#' itemperson_map(mod1, no_bins = 10)
#'
itemperson_map <- function(model, 
                           bin_color = "#0b66ff", 
                           no_bins = 45,
                           title_left = "Item-Person-Map", 
                           title_right = "",
                           scale_prop = TRUE) {
  
  item.params <- coef(model, IRTpars = TRUE, simplify = TRUE) %>%
    as.data.frame %>%
    rownames_to_column("items") 
  person.params <- fscores(model, QMC = TRUE) %>%
    as.data.frame()
  
  
  p1a <- ggplot(person.params, aes(x = F1)) +
    geom_histogram(bins = no_bins, color = "white", fill = bin_color) +
    scale_x_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-4, 4)) +
    theme_minimal() +
    theme(plot.margin=unit(c(1,0,0,0),"cm"),
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.ticks.length = unit(2.3, "mm"),
          panel.grid = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    labs(x = "θ", y = "", title = "")
  
  p1b <- item.params %>%
    mutate(items = fct_reorder(items, items.b)) %>%
    ggplot(aes(y = items, x = items.b)) +
    geom_point(color = bin_color) +
    geom_text(aes(label = items), nudge_x = .75, color = "grey", size = 2, check_overlap = T) +
    scale_x_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-4, 4)) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(color = "black"),
          axis.ticks.x = element_line(color = "black"),
          axis.ticks.length.x = unit(2.3, "mm"),
          plot.margin=unit(c(-3,0,0,0),"cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    labs(x = "θ", y = "")
  
  
  
  p <- cowplot::plot_grid(p1a, p1b,
                          labels = c(title_left, title_right),
                          nrow = 2,
                          rel_heights = c(1.6,3.4),
                          align = "hv",
                          axis = "tlbr")
  
  if(isTRUE(scale_prop)) {
    
    p2a <- plot(model, type = 'score', theta_lim = c(-4.5, 4), main = "Scale Characteristic Curve")
    p2b <- plot(model, type = 'infoSE', theta_lim = c(-4.5, 4), main = "Scale information and conditional standard errors")
    p2c <- plot(model, type = 'rxx', theta_lim = c(-4.5, 4), main = "Marginal Reliability")
    
    p <- ggdraw() +
      draw_plot(p, x = 0, y = 0.025, width = .50, height = .96) +
      draw_plot(p2a, x = .52, y = .66, width = .465, height = .33) +
      draw_plot(p2b, x = .52, y = .33, width = .48, height = 0.33) +
      draw_plot(p2c, x = .52, y = 0, width = .465, height = 0.32) 
    
  }
  
  return(p)
}

