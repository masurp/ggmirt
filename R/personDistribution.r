#' Person parameter distribution
#'
#' Visualizes the distribution of estimated person ability (theta) parameters
#' from a fitted mirt model. Works with both unidimensional and multidimensional
#' models. For multidimensional models, dimensions are shown with separate fill
#' colors.
#'
#' @param model an object of class `SingleGroupClass` returned by `mirt()`.
#' @param theta_range numeric vector of length 2 specifying the x-axis range
#'   (default: `c(-4, 4)`).
#' @param density logical; if `TRUE`, a smoothed density curve is plotted
#'   instead of a histogram (default: `FALSE`).
#' @param bins integer; number of histogram bins (default: `35`). Ignored when
#'   `density = TRUE`.
#' @param title character string for the plot title.
#'
#' @return a ggplot object.
#' @importFrom ggplot2 ggplot aes geom_density geom_histogram guides xlim
#'   theme_minimal labs
#' @importFrom dplyr everything
#' @importFrom tidyr pivot_longer
#' @export
#'
#' @examples
#' library(mirt)
#' library(ggmirt)
#'
#' data <- expand.table(LSAT7)
#' mod <- mirt(data, 1)
#'
#' personDist(mod)
#' personDist(mod, density = TRUE)
#' personDist(mod, theta_range = c(-3, 3), bins = 10) +
#'   ggplot2::theme_classic()
#'
personDist <- function(model,
                       theta_range = c(-4, 4),
                       density = FALSE,
                       bins = 35,
                       title = "Person Parameter Distribution") {

  person_params_raw <- mirt::fscores(model, QMC = TRUE)
  multidim <- ncol(person_params_raw) > 1

  person_params <- as.data.frame(person_params_raw) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "dimension",
      values_to = "value"
    )

  p <- ggplot2::ggplot(person_params, ggplot2::aes(
    x = .data[["value"]], fill = .data[["dimension"]]
  ))

  if (!multidim) {
    p <- p + ggplot2::guides(fill = "none")
  }

  if (isTRUE(density)) {
    p <- p + ggplot2::geom_density()
  } else {
    p <- p + ggplot2::geom_histogram(bins = bins, color = "white")
  }

  p +
    ggplot2::xlim(theta_range) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = expression(theta), title = title)
}
