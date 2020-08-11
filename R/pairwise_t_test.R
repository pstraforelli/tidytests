#' Pairwise T Tests
#'
#' @description A tidy calculation of pairwise comparisons between group levels with corrections for multiple testing
#' @param df A data frame or tibble of raw observations
#' @param outcome Response vector
#' @param subgroups Grouping vector
#' @param vs_rest Logical indicating whether to test each level of a subgroup to the rest of the data
#' @param ... Additional arguments passed on to pairwise.t.test and t.test
#'
#' @return A tibble with output from pairwise.t.test
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang :=
#' @importFrom dplyr pull
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom forcats fct_other
#' @importFrom purrr map_dfr
#' @export
#'
#' @examples
#' pairwise_t_test(iris, Sepal.Length, Species)

pairwise_t_test <- function(df, outcome, subgroups, vs_rest = FALSE, ...) {
  outcome <- enquo(outcome)
  subgroups <- enquo(subgroups)
  subgroups_name <- quo_name(subgroups)

  df <- mutate(df, !! subgroups := fct_drop(!! subgroups))

  if (vs_rest) {
    output <- df %>%
      pull(!! subgroups) %>%
      levels() %>%
      map_dfr(function(x) df %>%
                mutate(!! subgroups_name := fct_other(!! subgroups, keep = x)) %>%
                pairwise_t_test_int(!! outcome, !! subgroups))
  } else {
    output <- pairwise_t_test_int(df, !! outcome, !! subgroups)
  }

  output
}
