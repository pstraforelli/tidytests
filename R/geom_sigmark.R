#' Significance marker labels
#'
#' @description A wrapper for `ggtext::geom_richtext()` designed to pass labels and their significance markers
#' @param colour_fill Either "colour" or "fill", depending on the geometric object used
#' @param colours A vector of colours to be passed into `scale_*_manual()`
#' @param ... Arguments to pass into `ggtext::geom_richtext()`
#'
#' @note The `prep_sigmark()` function returns a data frame with a `labels` variable. This should be used for the `label` aesthetic of `geom_sigmark()`.
#' Note that the function also includes a `scale_*_manual` function call to ensure that the geom object is given the same colours as the significant markers.
#' Add `hjust = 0` for a horizontal bar chart or `vjust = 0` for a vertical bar chart.
#'
#' @return A ggplot2 layer that can be added to a plot created with `ggplot2::ggplot()`.
#' @importFrom ggtext geom_richtext
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_fill_manual
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' library(ggplot2)
#' mydf <- data.frame(smokers = c(rbinom(100, 1, .8),
#'                                rbinom(70, 1, 0.7),
#'                                rbinom(50, 1, 0.6)),
#'                    region = c(rep("A", 100), rep("B", 70), rep("C", 50)))
#' colour_vec <- c("red", "blue", "green", "orange")
#' names(colour_vec) <- LETTERS[1:4]
#'
#' summ_df <- mydf |>
#'   count(smokers, region) |>
#'   filter(smokers == 1) |>
#'   mutate(perc = n / sum(n))
#'
#' # Comparing subgroup to subgroup
#'
#' test_res <- mydf |>
#'   pairwise_prop_test(smokers, region) |>
#'   filter(p_value < 0.05, smokers == 1) |>
#'   select(higher_group, lower_group)
#'
#' summ_df |>
#'   prep_sigmark(test_res, region, perc, colour_vec, percent = TRUE, vs_rest = FALSE) |>
#'   ggplot(aes(x = perc, y = region, label = labels, fill = region)) +
#'   geom_col() +
#'   geom_sigmark("fill", colour_vec, hjust = 0) +
#'   ylim(c(0, 1))
#'
#' # Comparing each subgroup to rest of sample
#'
#' test_res2 <- mydf |>
#'   pairwise_prop_test(smokers, region, vs_rest = TRUE) |>
#'   filter(p_value < 0.05, smokers == 1) |>
#'   select(higher_group, lower_group)
#'
#' summ_df |>
#'   prep_sigmark(test_res2, region, perc, colour_vec, percent = TRUE, vs_rest = TRUE) |>
#'   ggplot(aes(x = perc, y = region, label = labels, fill = region)) +
#'   geom_col() +
#'   geom_sigmark(hjust = 0) +
#'   ylim(c(0, 1))

geom_sigmark <- function(colour_fill = NULL, colours, ...) {
  if (missing(colour_fill)) {
    geom_richtext(fill = NA, label.color = NA, ...)
  } else if (colour_fill == "colour") {
    list(
      geom_richtext(fill = NA, label.color = NA, ...),
      scale_colour_manual(values = colours)
    )
  } else if (colour_fill == "fill") {
    list(
      geom_richtext(fill = NA, label.color = NA, ...),
      scale_fill_manual(values = colours)
    )
  }
}
