#' Prepare for geom_sigmark
#'
#' @description Helper function to prepare a summarized data frame for significance markers with geom_sigmark
#' @param df A data frame with the summarized data to be charted
#' @param test_df The output of either `pairwise_t_test` or `pairwise_prop_test`. Note that you need to filter out that output to only include those comparisons to be shown as significant.
#' @param subgroups Grouping vector
#' @param result Result vector (either proportions or mean values)
#' @param colours A vector of colours associated with the subgroup levels. Not needed if `vs_rest` argument is set to `TRUE`.
#' @param percent A logical indicating whether the results are proportions or not
#' @param vs_rest A logical indicating whether the vs_rest argument in `pairwise_*_test()` was set to TRUE or FALSE.
#'
#' @return A tibble ready for ggplot2 chart with a `geom_sigmark()` layer.
#' @import rlang
#' @import dplyr
#' @import glue
#' @importFrom tidyr pivot_longer
#' @importFrom tibble enframe
#' @importFrom scales percent
#' @importFrom stringr str_c
#' @importFrom forcats fct_inorder
#' @export
#'
#' @examples
#' mydf <- data.frame(smokers = c(rbinom(100, 1, .8),
#'                                rbinom(70, 1, 0.7),
#'                                rbinom(50, 1, 0.6)),
#'                    region = c(rep("A", 100), rep("B", 70), rep("C", 50)))
#' test_res <- pairwise_prop_test(mydf, smokers, region)
#'
#' colour_vec <- c("A" = "red", "B" = "blue", "C" = "green")
#'
#' library(dplyr, warn.conflicts = FALSE)
#' mydf |>
#'   count(smokers, region) |>
#'   filter(smokers == 1) |>
#'   mutate(perc = n / sum(n)) |>
#'   prep_sigmark(test_res, region, perc, colour_vec, percent = TRUE, vs_rest = FALSE)

prep_sigmark <- function(df, test_df, subgroups, result, colours = NULL, percent, vs_rest) {
  subgroups_en <- enquo(subgroups)

  if (!is.factor(df[[as_name(subgroups_en)]])) {
    df[[as_name(subgroups_en)]] <- df[[as_name(subgroups_en)]] |>
      as.character() |>
      fct_inorder()
  }

  if (vs_rest) {
    test_df <- pivot_longer(test_df, everything())
    key <- "value"
    names(key) <- as_name(subgroups_en)

    output <- df |>
      left_join(test_df, by = key) |>
      mutate(labels = case_when(
        name == "higher_group" ~ glue("<span style='color:black'>&#9650;</span>"),
        name == "lower_group" ~ glue("<span style='color:black'>&#9660;</span>"),
        TRUE ~ as_glue(NA_character_)))
  } else {
    key <- "higher_group"
    names(key) <- as_name(subgroups_en)

    output <- df |>
      left_join(test_df, by = key) |>
      left_join(enframe(colours), by = c("lower_group" = "name")) |>
      mutate(labels = if_else(is.na(lower_group), as_glue(NA_character_), glue("<span style='color:{value}'>&#9650;</span>"))) |>
      group_by(!! subgroups_en) |>
      summarize("{{ result }}" := mean({{ result }}), labels = glue_collapse(labels), .groups = "drop_last")
  }

  if (percent) {
    output |>
      mutate(temp = percent({{ result }}, accuracy = 1),
             labels = ifelse(is.na(labels), temp, str_c(temp, labels))) |>
      select(-temp)
  } else {
    mutate(output, labels = ifelse(is.na(labels), {{ result }}, str_c({{ result }}, labels)))
  }
}
