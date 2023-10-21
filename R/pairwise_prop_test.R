#' Pairwise Proportional Tests
#'
#' @description A tidy calculation of pairwise proportional comparisons between group levels with corrections for multiple testing
#' @param df A data frame or tibble of raw observations
#' @param outcome Response vector
#' @param subgroups Grouping vector
#' @param vs_rest Logical indicating whether to test each level of a subgroup to the rest of the data
#' @param ... Additional arguments passed on to pairwise.prop.test and prop.test
#'
#' @return A tibble with output from pairwise.prop.test
#' @import rlang
#' @importFrom forcats fct_drop
#' @importFrom forcats fct_other
#' @importFrom forcats fct_inorder
#' @importFrom dplyr pull
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @export
#'
#' @examples
#' mydf <- data.frame(smokers = c(rbinom(100, 1, 0.8),
#'                                rbinom(70, 1, 0.7),
#'                                rbinom(50, 1, 0.6)),
#'                    region = c(rep("A", 100), rep("B", 70), rep("C", 50)))
#' pairwise_prop_test(mydf, smokers, region)
#' pairwise_prop_test(mydf, smokers, region, vs_rest = TRUE)

pairwise_prop_test <- function(df, outcome, subgroups, vs_rest = FALSE, ...) {
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
    levs <- df |>
      pull(!! subgroups) |>
      levels()

    if ("Other" %in% levs) {
      other_lev <- "Other2"
    } else {
      other_lev <- "Other"
    }

    levs |>
      map(function(x) df |>
                mutate(!! subgroups_name := fct_other(!! subgroups, keep = x, other_level = other_lev)) |>
                pairwise_prop_test_int(!! outcome, !! subgroups)) |>
      list_rbind() |>
      arrange(!! outcome)
  } else {
    pairwise_prop_test_int(df, !! outcome, !! subgroups)
  }
}
