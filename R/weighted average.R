### Weighted Average Function ###

#' This function allows users to calculate means while incorportating survey weights.
#' The function differs from weighted_freq because it doesn't create a frequency table. Instead, it produces a mean.

#' This function was developed by Alexander Bills.
#' @export
weighted_average <- function(data, var, group, weight) {
  # Capture the variable names as quosures
  var <- enquo(var)
  group <- enquo(group)
  weight <- enquo(weight)

  result <- data %>%
    group_by(!!group) %>%
    summarize(
      total_weight = sum(!!weight, na.rm = TRUE),
      weighted_sum_var = sum(!!var * !!weight, na.rm = TRUE),
      weighted_proportion = weighted_sum_var / total_weight
    ) %>%
    select(!!group, weighted_proportion)

  return(result)
}
