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
#' @importFrom rlang abort
#' @importFrom dplyr pull
#' @importFrom dplyr mutate
#' @importFrom forcats fct_other
#' @importFrom forcats fct_drop
#' @importFrom forcats fct_inorder
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @export
#'
#' @examples
#' pairwise_t_test(iris, Sepal.Length, Species)

pairwise_t_test <- function(df, outcome, subgroups, vs_rest = FALSE, ...) {
  outcome <- enquo(outcome)
  subgroups <- enquo(subgroups)
  subgroups_name <- quo_name(subgroups)

  if (!is.factor(df[[as_name(subgroups)]])) {
    df[[as_name(subgroups)]] <- df[[as_name(subgroups)]] |>
      as.character() |>
      fct_inorder()
  }

  df <- mutate(df, !! subgroups := fct_drop(!! subgroups))

  levels_check <- df |>
    pull(!! subgroups) |>
    nlevels()

  if (levels_check <= 1) {
    abort("Only one subgroup has data. Significance testing cannot be run.")
  }

  if (vs_rest) {
    df |>
      pull(!! subgroups) |>
      levels() |>
      map(function(x) df |>
                mutate(!! subgroups_name := fct_other(!! subgroups, keep = x)) |>
                pairwise_t_test_int(!! outcome, !! subgroups)) |>
      list_rbind()
  } else {
    pairwise_t_test_int(df, !! outcome, !! subgroups)
  }
}
