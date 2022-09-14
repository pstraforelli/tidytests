#' @importFrom rlang enquo
#' @importFrom rlang :=
#' @importFrom rlang quo_name
#' @importFrom dplyr mutate
#' @importFrom dplyr count
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_modify
#' @importFrom dplyr left_join
#' @importFrom dplyr transmute
#' @importFrom dplyr if_else
#' @importFrom tidyr complete

pairwise_prop_test_int <- function(df, x, subgroups, ...) {
  x <- enquo(x)
  subgroups <- enquo(subgroups)

  prop_df <- df |>
    count(!! subgroups, !! x, name = "counts", .drop = FALSE) |>
    complete(!! subgroups, !! x, fill = list(counts = 0)) |>
    group_by(!! subgroups) |>
    mutate(percentage = counts / sum(counts),
           n = sum(counts)) |>
    ungroup() |>
    select(-counts)

  df |>
    count(!! x, !! subgroups, name = "incidence", .drop = FALSE) |>
    complete(!! x, !! subgroups, fill = list(incidence = 0)) |>
    group_by(!! subgroups) |>
    mutate(n = sum(incidence)) |>
    ungroup() |>
    filter(n > 0) |>
    group_by(!! x) |>
    group_modify(~ pairwise_prop_test_zzz(., incidence, n, !! subgroups)) |>
    ungroup() |>
    left_join(prop_df, by = c("group1" = quo_name(subgroups), quo_name(x))) |>
    left_join(prop_df, by = c("group2" = quo_name(subgroups), quo_name(x)), suffix = c("_group1", "_group2")) |>
    transmute(!! x,
              higher_group = if_else(percentage_group1 >= percentage_group2, group1, group2),
              lower_group = if_else(percentage_group1 >= percentage_group2, group2, group1),
              p_value = p.value,
              higher_percentage = if_else(percentage_group1 >= percentage_group2, percentage_group1, percentage_group2),
              lower_percentage = if_else(percentage_group1 >= percentage_group2, percentage_group2, percentage_group1),
              higher_n = if_else(percentage_group1 >= percentage_group2, n_group1, n_group2),
              lower_n = if_else(percentage_group1 >= percentage_group2, n_group2, n_group1))
}
