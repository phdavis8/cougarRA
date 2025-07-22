### Weighted Frequency by Year ###

#' This function allows users to create a simple, one variable frequency table for cross-sectional analysis.
#' The user simply states the variable in the call, the weight, and the year column
#' @export


weighted_frequency_by_year <- function(data, weight, variable, year_col) {
  # Ensure all parameters are treated as strings
  weight <- deparse(substitute(weight))
  variable <- deparse(substitute(variable))
  year_col <- deparse(substitute(year_col))

  # Initialize an empty list to store results for each year
  results_list <- list()

  # Get unique years from the data
  unique_years <- unique(data[[year_col]])

  # Loop through each year
  for (year in unique_years) {
    # Filter data for the current year
    year_data <- data[data[[year_col]] == year, ]

    # Create survey design for this year
    design <- svydesign(data = year_data, ids = ~1, weights = as.formula(paste("~", weight)))

    # Create frequency table
    freq_table <- svytable(as.formula(paste("~", variable)), design = design)
    freq_table_df <- as.data.frame(freq_table)

    # Calculate percentages within this year
    freq_summary <- freq_table_df %>%
      mutate(
        percent = round(Freq / sum(Freq) * 100, 1),
        N = round(Freq)
      ) %>%
      select(all_of(variable), N, percent) %>%
      rename(Response = all_of(variable))

    # Add year column
    freq_summary <- freq_summary %>%
      mutate(Year = year)

    # Store in results list
    results_list[[as.character(year)]] <- freq_summary
  }

  # Combine all yearly results
  combined_results <- bind_rows(results_list)

  # Calculate total N for each year
  year_totals <- combined_results %>%
    group_by(Year) %>%
    summarise(Total_N = sum(N), .groups = "drop")

  # Pivot to wide format - only percent columns for each response category
  wide_results <- combined_results %>%
    select(-N) %>% # Remove individual N columns before pivoting
    pivot_wider(
      names_from = Response,
      values_from = percent,
      names_prefix = "percent_"
    ) %>%
    left_join(year_totals, by = "Year") %>%
    select(Year, Total_N, everything())

  return(wide_results)
}
