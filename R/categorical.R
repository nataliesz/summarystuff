#' @title Generate explanatory data analysis for categorical variable
#'
#' @description
#' Given a categorical variable, this function generate EDA including summary statistics and visualization.
#'
#' @importFrom ggplot2 ggplot
#'
#'
cat_summary_stat <- function(var) {
  # categorical
  n <- table(var)
  n_missing <- sum(is.na(var))
  rate_missing <- sum(!is.na(var)) / length(var)
  n_unique <- length(unique(var))
  value_unique <- unique(var)

  result <- data.frame(
    Variable_Name = var_name,
    N_Missing = n_missing,
    Rate_Missing = rate_missing,
    N_Unique = n_unique,
    Value_unique = value_unique
  )
  return(result)
}

#' @param var A categorical variable for[`summarystuff`].
#' @return A data frame containing summary statistics such as count of missing
#'   values, rate of missing values, number of unique categories, and unique
#'   category values.
#' @examples


cat_distribution <- function(df, var) {
  # barplot
  plot <- ggplot2::ggplot(df, aes(x = var)) + geom_bar()
  return(plot)
}
#' Plot Categorical Variable Distribution
#' @param df
#' @param var
#' @return A ggplot object displaying the distribution of the categorical variable.
#'
#' @examples
#'



cat_ <- function(df, var, y) {
  # boxplot with y variable
  plot <- ggplot2::ggplot(df, aes(x = var, y = y)) + geom_boxplot()
  return(plot)
}
#' Plot Categorical Variable Against Another Variable.
#' @param df
#' @param var
#' @param y The numerical variable to be plotted on y-axis.
#'
#' @return A ggplot object displaying the box plot.
#'
#' @examples
#'


# devtools::document()
# devtools::install()
# open a new session
# library(xkcd)
# xkcd(600)
