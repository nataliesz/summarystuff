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
