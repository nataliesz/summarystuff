numeric_stats <- function(df, var) {
  df |>
    dplyr::summarise(min = min(var),
                     q1 = quantile(var, 0.25),
                     median = median(var),
                     mean = mean(var),
                     q3 = quantile(var, 0.75),
                     max = max(var),
                     sd = sd(var),
                     n_missing = sum(is.na(var)),
                     rate_mising = sum(!is.na(var)) / length(var))
  }

all_numeric_sum <- function(df) {
  results <- summary(df)
  return(results)
}

#' @title Safe Summary Stats
#'
#' @description Given a dataset, computes the summary statistics for the data if
#' there are no NA values
#'
#'

safe_stats <- function(x, ...) {
  if (any(is.na(x))) {
    stop("Cannot compute the summary stats of x because x contains NA values.")
  } else {
    base::summary(x, ...)
  }
}

num_distribution <- function(df, var) {
  # histogram
  plot <- ggplot2::ggplot(df, aes(x = var)) +
    geom_histogram()
  return(plot)
}

#identify outliers
outliers <- function(dataset, x) {
  out <- summary(dataset$x)
  return(out)
}

outlier_hist <- function(dataset, x, ...){
  ggplot2::ggplot(data = dataset, x = x) +
    ggplot2::geom_histogram(
      x = x,
      bins = 10,
      color = "red",
      fill = "blue")
}
