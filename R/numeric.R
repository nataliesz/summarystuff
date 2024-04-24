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
  var_name <- deparse(substitute(var))
  if (any(is.na(var))) {
    stop("Cannot compute the summary stats of ", var_name, " because ", var_name, " contains NA values.")
  } else {
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
}

#' @title Single Histogram
#'
#' @description Given a dataset and a variable, creates a histogram to see distribution of data
#'
#' @param df dataframe provided by the user
#' @param variable variable name
#' @param bins number of bins the user wants to create
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' histograms(mtcars, mtcars$mpg, 5)
#' }
#'
#' @return A histogram distribution of the data provided

histograms <- function(df, variable, bins) {
  var_name <- deparse(substitute(variable))
  ggplot2::ggplot(data = df, ggplot2::aes(variable)) +
    ggplot2::geom_histogram(data = df, bins = bins) +
    ggplot2::labs(x = var_name)
}


#' @title Single Boxplot
#'
#' @description Given a dataset and a variable, creates a boxplot to see if
#'
#' @param df dataframe provided by the user
#' @param variable variable name
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' boxplots(mtcars, mtcars$mpg)
#' }
#'
#' @return A boxplot of the data

boxplots <- function(df, variable) {
  var_name <- deparse(substitute(variable))
  ggplot2::ggplot(data = df, ggplot2::aes(variable)) +
    ggplot2::geom_boxplot(data = df) +
    ggplot2::labs(x = var_name)
}
