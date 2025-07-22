### Unweighted Crosstab ###

#' normal crosstab without survey weights.
#' x_order allows you to quickly arrange the order of variables in the x axis. This is particularlly useful to prepare dataframes for table creation.
#' @export

## unweighted crosstab function-----------
unweighted_crosstab <- function(data, x, y, digits, x_order = NULL) {
  # ordering the x variable only if x_order is provided
  if (!is.null(x_order)) {
    data[[x]] <- factor(data[[x]], levels = x_order)
  }
  # creating the crosstab
  round(prop.table(table(data[[y]], data[[x]]), margin = 2) * 100, digits = digits)
}
