### Multi-crosstab Function ###

#' This function allows users to simultaneously run weighted crosstabs.
#' Users input a master dataframe that contains all the specifications for each weighted crosstab.
#' In the master dataframe, users specify the source data the crosstab should draw from. The source data MUST be in the global enviroment.
#' Other columns include necesasrry specifications for weighted crosstabs: x variable, y variable, and weights.
#' Users can also specify any necesarry filtering in the master dataframe.
#' Essentially, the specifications for each crosstab are configured in the master dataframe, generally via Excel.
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



# Define the weighted_crosstab function first
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

# Main function that processes multiple crosstabs
multi_crosstab <- function(archive, digits = 1, total_row = FALSE) {
  # Input validation
  required_cols <- c("data", "weight", "y", "x")
  missing_cols <- required_cols[!required_cols %in% names(archive)]

  if (length(missing_cols) > 0) {
    stop("Missing required columns in archive: ", paste(missing_cols, collapse = ", "))
  }

  # Initialize list to store results
  results <- list()

  # Iterate through each row in the archive dataframe
  for (i in 1:nrow(archive)) {
    # Get the current row
    current_row <- archive[i, ]

    # Debug information
    cat("Processing row", i, ":\n")
    cat("  Data:", current_row$data, "\n")
    cat("  Weight:", current_row$weight, "\n")
    cat("  Y:", current_row$y, "\n")
    cat("  X:", current_row$x, "\n")

    # Check for missing values
    if (is.na(current_row$data) || is.na(current_row$weight) ||
      is.na(current_row$y) || is.na(current_row$x)) {
      warning("Row ", i, " contains missing values. Skipping.")
      next
    }

    # Check for empty strings
    if (current_row$weight == "" || current_row$y == "" || current_row$x == "") {
      warning("Row ", i, " contains empty strings. Skipping.")
      next
    }

    # Extract the data for this crosstab
    # Get the dataset from global environment using the 'data' column
    if (!exists(current_row$data)) {
      warning("Dataset '", current_row$data, "' not found in global environment. Skipping row ", i)
      next
    }

    current_data <- get(current_row$data)
    current_weight <- current_row$weight
    current_y <- current_row$y
    current_x <- current_row$x

    # Apply year filtering if specified in the archive row
    if ("year_col" %in% names(archive) && "years" %in% names(archive)) {
      current_year_col <- current_row$year_col
      current_years <- current_row$years

      # Only filter if both year_col and years are not NA/empty
      if (!is.na(current_year_col) && !is.na(current_years) &&
        current_year_col != "" && current_years != "") {
        # Check if year column exists in the data
        if (!current_year_col %in% names(current_data)) {
          warning("Year column '", current_year_col, "' not found in dataset '", current_row$data, "'. Skipping row ", i)
          next
        }

        # Parse the years - handle both comma-separated and colon ranges
        if (is.character(current_years)) {
          # Check if it's a range (contains colon)
          if (grepl(":", current_years)) {
            # Handle range like "1952:2020"
            range_parts <- as.numeric(unlist(strsplit(current_years, ":")))
            if (length(range_parts) == 2) {
              years_to_filter <- range_parts[1]:range_parts[2]
            } else {
              warning("Invalid year range format in row ", i, ": ", current_years)
              next
            }
          } else {
            # Handle comma-separated like "1996, 2004, 2020"
            years_to_filter <- as.numeric(unlist(strsplit(current_years, ",")))
          }

          # Remove any whitespace and convert to numeric
          years_to_filter <- as.numeric(trimws(years_to_filter))

          # Remove any NA values that might result from parsing errors
          years_to_filter <- years_to_filter[!is.na(years_to_filter)]

          if (length(years_to_filter) == 0) {
            warning("No valid years found in row ", i, ": ", current_years)
            next
          }
        } else {
          years_to_filter <- current_years
        }

        # Filter the data for specified years
        current_data <- current_data[current_data[[current_year_col]] %in% years_to_filter, ]

        # Check if any data remains after filtering
        if (nrow(current_data) == 0) {
          warning("No data remaining after year filtering for row ", i, ". Skipping.")
          next
        }

        cat("  Year column:", current_year_col, "\n")
        cat("  Filtered for years:", paste(years_to_filter, collapse = ", "), "\n")
        cat("  Rows remaining:", nrow(current_data), "\n")
      }
    }

    # Helper function to parse filter values
    parse_filter_values <- function(filter_string) {
      if (is.na(filter_string) || filter_string == "") {
        return(NULL)
      }

      # Convert to character if not already
      filter_string <- as.character(filter_string)

      # Check if it's a range (contains colon)
      if (grepl(":", filter_string)) {
        # Handle range like "1:5" or "A:E"
        range_parts <- unlist(strsplit(filter_string, ":"))
        if (length(range_parts) == 2) {
          # Try numeric first
          if (all(!is.na(as.numeric(range_parts)))) {
            start_val <- as.numeric(range_parts[1])
            end_val <- as.numeric(range_parts[2])
            return(start_val:end_val)
          } else {
            # For character ranges, just return the parts
            return(trimws(range_parts))
          }
        } else {
          warning("Invalid range format: ", filter_string)
          return(NULL)
        }
      } else {
        # Handle comma-separated values
        values <- unlist(strsplit(filter_string, ","))
        values <- trimws(values)

        # Try to convert to numeric if possible, otherwise keep as character
        numeric_values <- suppressWarnings(as.numeric(values))
        if (all(!is.na(numeric_values))) {
          return(numeric_values)
        } else {
          return(values)
        }
      }
    }

    # Apply x_filter if specified
    if ("x_filter" %in% names(archive)) {
      current_x_filter <- current_row$x_filter

      if (!is.na(current_x_filter) && current_x_filter != "") {
        # Check if x column exists in the data
        if (!current_x %in% names(current_data)) {
          warning("X column '", current_x, "' not found in dataset '", current_row$data, "'. Skipping row ", i)
          next
        }

        # Parse the x filter values
        x_values_to_filter <- parse_filter_values(current_x_filter)

        if (!is.null(x_values_to_filter)) {
          # Filter the data for specified x values
          current_data <- current_data[current_data[[current_x]] %in% x_values_to_filter, ]

          # Check if any data remains after filtering
          if (nrow(current_data) == 0) {
            warning("No data remaining after x filtering for row ", i, ". Skipping.")
            next
          }

          cat("  X filtered for values:", paste(x_values_to_filter, collapse = ", "), "\n")
          cat("  Rows remaining:", nrow(current_data), "\n")
        }
      }
    }

    # Apply y_filter if specified
    if ("y_filter" %in% names(archive)) {
      current_y_filter <- current_row$y_filter

      if (!is.na(current_y_filter) && current_y_filter != "") {
        # Check if y column exists in the data
        if (!current_y %in% names(current_data)) {
          warning("Y column '", current_y, "' not found in dataset '", current_row$data, "'. Skipping row ", i)
          next
        }

        # Parse the y filter values
        y_values_to_filter <- parse_filter_values(current_y_filter)

        if (!is.null(y_values_to_filter)) {
          # Filter the data for specified y values
          current_data <- current_data[current_data[[current_y]] %in% y_values_to_filter, ]

          # Check if any data remains after filtering
          if (nrow(current_data) == 0) {
            warning("No data remaining after y filtering for row ", i, ". Skipping.")
            next
          }

          cat("  Y filtered for values:", paste(y_values_to_filter, collapse = ", "), "\n")
          cat("  Rows remaining:", nrow(current_data), "\n")
        }
      }
    }

    # Perform the weighted crosstab
    tryCatch(
      {
        result <- weighted_crosstab(
          data = current_data,
          weight = current_weight,
          y = current_y,
          x = current_x,
          digits = digits,
          total_row = total_row
        )

        # Store the result with a meaningful name
        results[[length(results) + 1]] <- result
        names(results)[length(results)] <- paste0("crosstab_", i)
      },
      error = function(e) {
        warning("Error in row ", i, ": ", e$message)
      }
    )
  }

  return(results)
}
