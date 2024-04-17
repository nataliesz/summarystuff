#' @title EDA for Numerics
#'
#' @description Generates exploratory data analysis for numeric variables
#'
#' @importFrom plot plot::ggplot2
#'
#' @param df A dataframe provided by the user
#' @param var A variable to compute statistics for
#' @param ... currently unused
#'
#' @examples
#'
#' Given a numeric variable, this function generate EDA including distribution and visualization.
#' @import geom_histogram ggplot2::geom_histogram

num_summary_stat <- function(df, var) {
  # categorical
  var_name <- as.character(var)
  n_missing <- sum(is.na(df$var))
  rate_missing <- sum(!is.na(df$var)) / length(df$var)
  mean <- mean(df$var)
  median <- median(df$var)
  sd <- sd(df$var)
  min <- min(df$var)
  max <- max(df$var)

  result <- list(var_name, n_missing, rate_missing, mean, median, sd, min, max)
}

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

num_distribution <- function(df, var) {
  # histogram
  plot <- ggplot2::ggplot(df, aes(x = var)) +
    geom_histogram()
  return(plot)
}

numeric_y <- function(df, var, y) {
  # boxplot with y variable
  plot <- ggplot2::ggplot(df, aes(x = var, y = var)) +
    geom_boxplot()
  return(plot)
}
