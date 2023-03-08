#' @importFrom rlang enquo
#' @importFrom dplyr reframe
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom broom tidy
#' @importFrom stats pairwise.prop.test

pairwise_prop_test_zzz <- function(df, x, n, subgroups = NULL, ...) {
  x <- enquo(x)
  n <- enquo(n)

  df_tested <- df |>
    reframe(tidy(pairwise.prop.test(!! x, !! n, ...))) |>
    mutate(group1 = as.numeric(group1), group2 = as.numeric(group2))

  if (missing(subgroups)) {
    df_tested
  } else {
    subgroups <- enquo(subgroups)
    subgroups <- pull(df, !! subgroups)

    mutate(df_tested, group1 = subgroups[group1], group2 = subgroups[group2])
  }
}
