#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr reframe
#' @importFrom dplyr n
#' @importFrom dplyr as_tibble
#' @importFrom dplyr left_join
#' @importFrom dplyr transmute
#' @importFrom dplyr if_else
#' @importFrom broom tidy
#' @importFrom stats pairwise.t.test
#' @importFrom stats sd

pairwise_t_test_int <- function(df, x, subgroups, ...) {
  x <- enquo(x)
  subgroups <- enquo(subgroups)

  levels_subgroups <- df |>
    pull(!! subgroups) |>
    levels()

  summary_df <- df |>
    group_by(!! subgroups) |>
    summarize(mean = mean(!! x),
              sd = sd(!! x),
              n = n(), .groups = "drop_last")

  df |>
    reframe(tidy(pairwise.t.test(!! x, !! subgroups, ...))) |>
    as_tibble() |>
    left_join(summary_df, by = c("group1" = quo_name(subgroups))) |>
    left_join(summary_df, by = c("group2" = quo_name(subgroups)), suffix = c("_group1", "_group2")) |>
    transmute(higher_group = if_else(mean_group1 >= mean_group2, group1, group2),
              lower_group = if_else(mean_group1 >= mean_group2, group2, group1),
              p_value = p.value,
              higher_mean = if_else(mean_group1 >= mean_group2, mean_group1, mean_group2),
              lower_mean = if_else(mean_group1 >= mean_group2, mean_group2, mean_group1),
              higher_sd = if_else(mean_group1 >= mean_group2, sd_group1, sd_group2),
              lower_sd = if_else(mean_group1 >= mean_group2, sd_group2, sd_group1),
              higher_n = if_else(mean_group1 >= mean_group2, n_group1, n_group2),
              lower_n = if_else(mean_group1 >= mean_group2, n_group2, n_group1)) |>
    mutate(higher_group = factor(higher_group, levels = levels_subgroups),
           lower_group = factor(lower_group, levels = levels_subgroups))
}
