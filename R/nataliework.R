#identify outliers
outliers <- function(dataset, x) {
  out <- summary(dataset$x)
  return(out)
}

outlier_hist <- function(dataset, x, ...){
  ggplot2::ggplot(data = dataset, x = x) +
    ggplot2::geom_histogram(
      color = "red",
      fill = "blue")
}

numeric_statistic <- function(df, variable) {
  # categorical
  #var_name <- as.character("variable")
  n_missing <- sum(is.na(variable))
  rate_missing <- sum(!is.na(variable)) / length(variable)
  mean <- mean(variable)
  median <- median(variable)
  sd <- sd(variable)
  min <- min(variable)
  max <- max(variable)

  result <- as.data.frame(n_missing, rate_missing, mean, median, sd, min, max)
  return(result)
}
