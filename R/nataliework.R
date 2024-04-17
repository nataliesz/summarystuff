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

numeric_stats <- function(df) {
  for (i in col(df)){
    dplyr::summarise(min = min(df[[i]]),
                     q1 = quantile(df[[i]], 0.25),
                     median = median(df[[i]]),
                     mean = mean(df[[i]]),
                     q3 = quantile(df[[i]], 0.75),
                     max = max(df[[i]]),
                     sd = sd(df[[i]]),
                     n_missing = sum(is.na(df[[i]])),
                     rate_mising = sum(!is.na(df[[i]])) / length(df[[i]]))
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
