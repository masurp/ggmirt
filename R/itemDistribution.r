#' Item difficulty distribution
#'
#' Visualizes the distribution of item difficulty (b) parameters from a fitted
#' mirt model. For dichotomous models (1PL, 2PL, 3PL), a single difficulty
#' parameter per item is plotted. For graded response models (GRM), all
#' threshold parameters are shown as separate colored points.
#'
#' @param model an object of class `SingleGroupClass` returned by `mirt()`.
#' @param theta_range numeric vector of length 2 specifying the x-axis range
#'   (default: `c(-4, 4)`).
#' @param title character string for the plot title.
#' @param colors character vector of colors for threshold points in GRM models.
#'   Passed to `scale_color_manual()`. Ignored for dichotomous models, where
#'   point color is controlled via `color` in `...`.
#' @param ... additional arguments passed to `geom_point()`. For dichotomous
#'   models, `color` can be set here. For GRM models, `color` is ignored
#'   (use `colors` instead).
#'
#' @return a ggplot object.
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual xlim
#'   theme_minimal labs
#' @importFrom dplyr mutate all_of
#' @importFrom tidyr pivot_longer
#' @export
#'
#' @examples
#' library(mirt)
#' library(ggmirt)
#'
#' # Dichotomous model
#' data <- expand.table(LSAT7)
#' mod <- mirt(data, 1)
#' itemDist(mod)
#' itemDist(mod, size = 3, shape = 17, color = "blue")
#'
#' # Graded response model
#' mod_grm <- mirt(Science, 1, itemtype = "graded")
#' itemDist(mod_grm, size = 3, shape = 17)
#' itemDist(mod_grm, colors = c("b1" = "steelblue", "b2" = "orange",
#'                              "b3" = "forestgreen", "b4" = "tomato"))
#'
itemDist <- function(model,
                     theta_range = c(-4, 4),
                     title = "Item Difficulty Distribution",
                     colors = NULL,
                     ...) {

  params <- mirt::coef(model, IRTpars = TRUE, simplify = TRUE)
  item_params <- as.data.frame(params)
  item_params[["item"]] <- rownames(item_params)

  b_cols <- grep("^items\\.b", names(item_params), value = TRUE)

  if (length(b_cols) == 0) {
    stop(
      "No difficulty parameters found. ",
      "This function supports unidimensional dichotomous (1PL, 2PL, 3PL) ",
      "and graded response models (GRM)."
    )
  }

  if (length(b_cols) == 1) {
    # Dichotomous model (1PL, 2PL, 3PL): single b parameter per item
    p <- item_params |>
      dplyr::mutate(
        item = reorder(.data[["item"]], .data[[b_cols]])
      ) |>
      ggplot2::ggplot(ggplot2::aes(
        y = .data[["item"]], x = .data[[b_cols]]
      )) +
      ggplot2::geom_point(...)

  } else {
    # Graded response model (GRM): one b parameter per threshold.
    # Strip `color` from ... so it doesn't override the threshold color mapping.
    dots <- list(...)
    dots[["color"]] <- NULL

    p <- item_params |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(b_cols),
        names_to = "threshold",
        values_to = "b"
      ) |>
      dplyr::mutate(
        threshold = sub("^items\\.b", "b", .data[["threshold"]]),
        item = reorder(
          .data[["item"]], .data[["b"]],
          FUN = function(x) mean(x, na.rm = TRUE)
        )
      ) |>
      ggplot2::ggplot(ggplot2::aes(
        y = .data[["item"]], x = .data[["b"]], color = .data[["threshold"]]
      )) +
      do.call(ggplot2::geom_point, dots) +
      ggplot2::labs(color = "Threshold")

    if (!is.null(colors)) {
      p <- p + ggplot2::scale_color_manual(values = colors)
    }
  }

  p +
    ggplot2::xlim(theta_range) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = expression(theta), y = "", title = title)
}
