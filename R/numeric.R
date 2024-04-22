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


#' @title Safe Summary Stats
#'
#' @description Given a dataset, computes the summary statistics for the data if
#' there are no NA values
#'
#' @param df dataframe provided by the user
#' @param x variable name
#' @param ... currently unused
#'
#' @examples
#' data(penguins)
#' safe_stats(penguins$flipper_length_mm)
#'
#' @return A list of summary statistics or an error message stating there are NA values

safe_stats <- function(x, ...) {
  var_name <- deparse(substitute(x))
  if (any(is.na(x))) {
    stop("Cannot compute the summary stats of ", var_name, " because ", var_name, " contains NA values.")
  } else {
    base::summary(x, ...)
  }
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
#'   there are outliers in the data
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
