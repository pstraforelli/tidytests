#' Internal function for Pairwise Proportional Tests
#'
#' @description Internal function to be used in pairwise_prop_test
#' @param df A data frame
#' @param x A vector of "successes"
#' @param n A vector of "trials"
#' @param subgroups Optional, a vector of subgroup labels to append to the output
#' @param ... Additional arguments passed on to pairwise.prop.test and prop.test
#'
#' @return A tibble listing each pairwise comparison and its corresponding p-value
#' @importFrom rlang enquo
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr %>%
#' @importFrom broom tidy
#' @importFrom stats pairwise.prop.test

pairwise_prop_test_int <- function(df, x, n, subgroups = NULL, ...) {
  x <- enquo(x)
  n <- enquo(n)

  df_tested <- df %>%
    summarize(tidy(pairwise.prop.test(!! x, !! n, ...))) %>%
    mutate(group1 = as.numeric(group1), group2 = as.numeric(group2))

  if (missing(subgroups)) {
    df_tested
  } else {
    subgroups <- enquo(subgroups)
    subgroups <- pull(df, !! subgroups)

    mutate(df_tested, group1 = subgroups[group1], group2 = subgroups[group2])
  }
}
