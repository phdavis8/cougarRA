### Weighted Crosstab ###

#' This is the key function of the CougarRA package. Weighted crosstab allows users to easily created and customize weighted crosstabs.
#' The function improves on the retired cross_tabulate function in several ways.
#' First, weighted crosstab allows you to customize rounding. The default rounding is set to the tenths place, but that can easily be adjusted using the digits parameter within the function.
#' Second, weighted crosstab allows users to choose whether to include a total row for the N of each value of x. The default setting is no total column, but that can be adjusted by setting total_column to TRUE.
#' Third, weighted crosstab has improved efficiency. Error messages are more specific, allowing users to easily troubleshoot. Likewise, weighted crosstab uses updated syntax, eliminating error messages for depreciated codes.
#' @export


weighted_crosstab <- function(data, weight, y, x, digits = 1, total_row = FALSE) {
  # Input validation
  if (!all(c(weight, y, x) %in% names(data))) {
    stop("All specified columns must exist in the data")
  }

  # Create survey design
  design <- svydesign(data = data, ids = ~1, weights = as.formula(paste("~", weight)))

  # Create crosstab with counts
  crosstab_n <- svytable(as.formula(paste("~", x, "+", y)), design = design)

  # Calculate proportions by row (margin = 1)
  crosstab_prop <- prop.table(crosstab_n, margin = 1)

  # Convert to data frames and merge in one step
  crosstab_df <- merge(
    as.data.frame(crosstab_n),
    as.data.frame(crosstab_prop),
    by = c(y, x),
    suffixes = c("_n", "_prop")
  )

  # Calculate totals by x category and reshape data efficiently
  result <- crosstab_df %>%
    group_by(!!sym(x)) %>%
    mutate(total_n = sum(Freq_n)) %>%
    ungroup() %>%
    # Convert proportions to percentages with specified rounding
    mutate(
      Freq_percent = round(Freq_prop * 100, digits = digits),
      N = round(Freq_n, digits = 0) # Counts should always be whole numbers
    ) %>%
    select(-Freq_n, -Freq_prop) %>%
    # Pivot wider to get categories as columns
    pivot_wider(
      names_from = all_of(x),
      values_from = c(Freq_percent, N),
      names_sep = "_"
    )

  # Summarize by response category
  condensed_result <- result %>%
    group_by(!!sym(y)) %>%
    summarise(
      across(starts_with("Freq_percent"), ~ sum(.x, na.rm = TRUE)),
      across(starts_with("N_"), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    # Calculate total N for each response
    rowwise() %>%
    mutate(N = sum(c_across(starts_with("N_")), na.rm = TRUE)) %>%
    ungroup() %>%
    rename(Response = !!sym(y))

  # Calculate total percentage
  total_N <- sum(condensed_result$N, na.rm = TRUE)
  condensed_result <- condensed_result %>%
    mutate(Total_percentage = round((N / total_N) * 100, digits = digits))

  # Remove Freq_percent_ prefix from column names and drop N_ columns
  condensed_result <- condensed_result %>%
    rename_with(~ str_replace(.x, "^Freq_percent_", ""), starts_with("Freq_percent_")) %>%
    select(-starts_with("N_"))

  # Add total row if requested
  if (total_row) {
    # Calculate column totals - get actual counts for each x category
    # Go back to the original crosstab data to get true totals by x category
    x_totals <- crosstab_df %>%
      group_by(!!sym(x)) %>%
      summarise(total_count = sum(Freq_n), .groups = "drop")

    # Create the totals row with actual counts for each x category
    total_counts <- tibble(Response = "Total_N")

    # Add each x category total as a separate column
    for (x_val in x_totals[[x]]) {
      total_counts[[as.character(x_val)]] <- x_totals$total_count[x_totals[[x]] == x_val]
    }

    # Add overall totals
    total_counts <- total_counts %>%
      mutate(
        N = sum(x_totals$total_count),
        Total_percentage = 100
      )

    # Combine results
    final_result <- bind_rows(condensed_result, total_counts)
  } else {
    final_result <- condensed_result
  }

  return(final_result)
}
