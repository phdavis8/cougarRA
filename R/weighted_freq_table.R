### weighted freq table function ###

#' This function creates a weighted frequency table.
#' @export

weighted_freq <- function(data, weight, variable) {
  # Ensure weight and variable are treated as strings
  weight <- deparse(substitute(weight))
  variable <- deparse(substitute(variable))

  design <- svydesign(data = data, ids = ~1, weights = as.formula(paste("~", weight)))

  first_summary <- svytable(as.formula(paste("~", variable)), design = design)
  first_summary <- as.data.frame(first_summary)

  final_summary <- first_summary %>%
    mutate(percent = Freq / sum(Freq) * 100) %>%
    mutate_if(is.numeric, round, digits = 0) %>%
    select(variable, percent, Freq)

  return(final_summary)
}
