#' Pairwise Proportional Tests
#'
#' @description A tidy calculation of pairwise proportional comparisons between group levels with corrections for multiple testing
#' @param df A data frame or tibble of raw observations
#' @param x Response vector
#' @param subgroups Grouping vector
#' @param vs_rest Logical - Compare a particular subgroup to the rest of sample?
#' @param ... Additional arguments passed on to pairwise.prop.test and prop.test
#'
#' @return A tibble with output from pairwise.prop.test
#' @export
#'
#' @examples
#' mydf <- data.frame(smokers = c(rbinom(100, 1, .8), rbinom(70, 1, 0.7), rbinom(50, 1, 0.6)), region = c(rep("A", 100), rep("B", 70), rep("C", 50)))
#' pairwise_prop_test(mydf, smokers, region)

pairwise_prop_test <- function(df, x, subgroups, vs_rest = FALSE, ...) {
  x <- enquo(x)
  subgroups <- enquo(subgroups)

  df <- mutate(df, !! subgroups := fct_drop(!! subgroups))

  prop_df <- df %>%
    count(!! subgroups, !! x, name = "counts", .drop = FALSE) %>%
    group_by(!! subgroups) %>%
    mutate(percentage = counts / sum(counts),
           n = sum(counts)) %>%
    ungroup() %>%
    select(-counts)

  output <- count(df, !! x, !! subgroups, name = "incidence", .drop = FALSE) %>%
    group_by(!! subgroups) %>%
    mutate(n = sum(incidence)) %>%
    ungroup() %>%
    filter(n > 0) %>%
    group_by(!! x) %>%
    group_modify(~ pairwise_prop_test_int(., incidence, n, !! subgroups)) %>%
    ungroup() %>%
    left_join(prop_df, by = c("group1" = rlang::quo_name(subgroups), rlang::quo_name(x))) %>%
    left_join(prop_df, by = c("group2" = rlang::quo_name(subgroups), rlang::quo_name(x)), suffix = c("_group1", "_group2")) %>%
    transmute(!! x,
              higher_group = if_else(percentage_group1 >= percentage_group2, group1, group2),
              lower_group = if_else(percentage_group1 >= percentage_group2, group2, group1),
              p_value = p.value,
              higher_percentage = if_else(percentage_group1 >= percentage_group2, percentage_group1, percentage_group2),
              lower_percentage = if_else(percentage_group1 >= percentage_group2, percentage_group2, percentage_group1),
              higher_n = if_else(percentage_group1 >= percentage_group2, n_group1, n_group2),
              lower_n = if_else(percentage_group1 >= percentage_group2, n_group2, n_group1))

  if (vs_rest) {
    rest_output <- map(levels(pull(df, !! subgroups)), function(x) {
      to_collapse <- levels(pull(df, !! subgroups))[levels(pull(df, !! subgroups)) != .x]

      prop_df <- df %>%
        mutate(!! subgroups := fct_collapse(!! subgroups, "Rest" = to_collapse)) %>%
        count(!! subgroups, !! x, name = "counts", .drop = FALSE) %>%
        group_by(!! subgroups) %>%
        mutate(percentage = counts / sum(counts),
               n = sum(counts)) %>%
        ungroup() %>%
        select(-counts)

      count(df, !! x, !! subgroups, name = "incidence", .drop = FALSE) %>%
        group_by(!! subgroups) %>%
        mutate(n = sum(incidence)) %>%
        ungroup() %>%
        filter(n > 0) %>%
        group_by(!! x) %>%
        group_modify(~ pairwise_prop_test_int(., incidence, n, !! subgroups)) %>%
        ungroup() %>%
        left_join(prop_df, by = c("group1" = rlang::quo_name(subgroups), rlang::quo_name(x))) %>%
        left_join(prop_df, by = c("group2" = rlang::quo_name(subgroups), rlang::quo_name(x)), suffix = c("_group1", "_group2")) %>%
        transmute(!! x,
                  higher_group = if_else(percentage_group1 >= percentage_group2, group1, group2),
                  lower_group = if_else(percentage_group1 >= percentage_group2, group2, group1),
                  p_value = p.value,
                  higher_percentage = if_else(percentage_group1 >= percentage_group2, percentage_group1, percentage_group2),
                  lower_percentage = if_else(percentage_group1 >= percentage_group2, percentage_group2, percentage_group1),
                  higher_n = if_else(percentage_group1 >= percentage_group2, n_group1, n_group2),
                  lower_n = if_else(percentage_group1 >= percentage_group2, n_group2, n_group1))
    })

    bind_rows(output, rest_output)
  } else {
    output
  }
}
