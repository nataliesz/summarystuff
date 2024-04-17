#' @title EDA for Numerics
#'
#' @description Given a numeric variable, generates basic exploratory data analysis
#'
#' @importFrom dplyr summarise
#'
#' @param df A dataframe provided by the user
#' @param var A variable to compute statistics for
#' @param ... currently unused
#'
#' @examples
#' data("mtcars")
#' numeric_stats(mtcars, mtcars$mpg)
#' @return A list of summary stats

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

histogram <- function(df, var) {
  hist(var,
       xlab = "Distribution of")
}

numeric_y <- function(df, var, y) {
  # boxplot with y variable
  plot <- ggplot2::ggplot(df, aes(x = var, y = var)) +
    geom_boxplot()
  return(plot)
}
