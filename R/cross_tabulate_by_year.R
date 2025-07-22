### cross_tabulate_by_year ###

#' This function allows you to easily create crosstabs for cross-sectional analysis.

#' Note: This function is still in a beta phase. Future updates should include customizable rouding, total rows, and updated syntax.
#' @export
cross_tabulate_by_year <- function(data, weight, y_col, x_col, year_col) {
  # Initialize an empty list to store results for each year
  results_list <- list()

  # Get unique years from the data
  unique_years <- unique(data[[year_col]])

  # Loop through each year and apply the cross_tabulate function
  for (year in unique_years) {
    # Filter data for the current year
    year_data <- data[data[[year_col]] == year, ]

    # Create survey design
    design <- svydesign(data = year_data, ids = ~1, weights = as.formula(paste("~", weight)))

    # Create crosstab with counts
    crosstab_n <- svytable(as.formula(paste("~", x_col, "+", y_col)), design = design)
    crosstab_n_df <- as.data.frame(crosstab_n)

    # Calculate proportions
    crosstab_prop <- prop.table(crosstab_n, margin = 1)
    crosstab_prop_df <- as.data.frame(crosstab_prop)

    # Merge counts and proportions
    merged_crosstab <- merge(crosstab_n_df, crosstab_prop_df, by = c(y_col, x_col), suffixes = c("_n", "_percent"))

    # Adding totals for each pid category
    merged_crosstab <- merged_crosstab %>%
      group_by(!!sym(x_col)) %>%
      mutate(total_n = sum(Freq_n))

    # Round values and reshape data
    merged_crosstab <- merged_crosstab %>%
      mutate(
        Freq_percent = round(Freq_percent * 100),
        N = round(Freq_n)
      ) %>%
      pivot_wider(names_from = all_of(x_col), values_from = c(Freq_percent, N))

    # Ensure N column is created correctly
    if (!"N" %in% colnames(merged_crosstab)) {
      merged_crosstab <- merged_crosstab %>%
        mutate(N = rowSums(select(., starts_with("N_")), na.rm = TRUE))
    }

    # Condense rows by summing up percentages and counts
    condensed_crosstab <- merged_crosstab %>%
      group_by(!!sym(y_col)) %>%
      summarise(across(starts_with("Freq_percent"), \(x) sum(x, na.rm = TRUE)),
        N = sum(N, na.rm = TRUE)
      ) %>%
      rename(Response = !!sym(y_col))

    # Creating a percentage column
    total_N <- sum(condensed_crosstab$N, na.rm = TRUE)
    condensed_crosstab <- condensed_crosstab %>%
      mutate(Total_percentage = round((N / total_N) * 100))

    # Calculate total counts for each category in x_col
    total_counts <- merged_crosstab %>%
      summarise(across(starts_with("N_"), sum, na.rm = TRUE)) %>%
      mutate(Response = "Total_N")

    # Rename columns with the prefix Freq_percent_
    total_counts <- total_counts %>%
      rename_with(~ paste0("Freq_percent_", sub("^N_", "", .)), starts_with("N_"))

    # Add the total row at the bottom
    final_crosstab <- bind_rows(condensed_crosstab, total_counts)

    # Add a column for the year
    final_crosstab <- final_crosstab %>%
      mutate(Year = year)

    # Append the result to the list
    results_list[[as.character(year)]] <- final_crosstab
  }

  # Combine all yearly results into one data frame
  combined_results <- bind_rows(results_list)

  return(combined_results)
}
