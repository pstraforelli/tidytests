#' Pairwise T Tests
#'
#' @description A tidy calculation of pairwise comparisons between group levels with corrections for multiple testing
#' @param df A data frame or tibble
#' @param x Response vector
#' @param subgroups Grouping vector
#' @param ... Additional arguments passed on to pairwise.t.test and t.test
#'
#' @return A tibble with output from pairwise.t.test
#' @export
#'
#' @examples
#' pairwise_t_test(iris, Sepal.Length, Species)

pairwise_t_test <- function(df, x, subgroups, ...) {
  x <- enquo(x)
  subgroups <- enquo(subgroups)

  summary_df <- df %>%
    group_by(!! subgroups) %>%
    summarize(mean = mean(!! x),
              sd = sd(!! x),
              n = n(), .groups = "drop_last")

  as_tibble(summarize(df, tidy(pairwise.t.test(!! x, !! subgroups, ...)))) %>%
    left_join(summary_df, by = c("group1" = rlang::quo_name(subgroups))) %>%
    left_join(summary_df, by = c("group2" = rlang::quo_name(subgroups)), suffix = c("_group1", "_group2")) %>%
    transmute(higher_group = if_else(mean_group1 >= mean_group2, group1, group2),
              lower_group = if_else(mean_group1 >= mean_group2, group2, group1),
              p_value = p.value,
              higher_mean = if_else(mean_group1 >= mean_group2, mean_group1, mean_group2),
              lower_mean = if_else(mean_group1 >= mean_group2, mean_group2, mean_group1),
              higher_sd = if_else(mean_group1 >= mean_group2, sd_group1, sd_group2),
              lower_sd = if_else(mean_group1 >= mean_group2, sd_group2, sd_group1),
              higher_n = if_else(mean_group1 >= mean_group2, n_group1, n_group2),
              lower_n = if_else(mean_group1 >= mean_group2, n_group2, n_group1))
}
