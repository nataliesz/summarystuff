#' @title EDA for Categoricals
#'
#' @description Generates exploratory data analysis for categorical variables
#'
#' @importFrom ggplot2 ggplot
#' @importFrom dplyr summarise
#'
#' @param df A dataframe provided by the user
#' @param var A categorical variable to compute statistics for
#'
#' @return A data frame containing summary statistics such as count of missing
#'   values, rate of missing values, number of unique categories, and unique
#'   category values.
#'
#' @examples

cat_summary_stat <- function(df, var) {
# categorical
  results <- df |>
    group_by({{var}}) |>
    summarise(
      n = n(),
      prop = n() / nrow(df),
      n_missing = sum(is.na({{var}})),
      prop_missing = sum(is.na({{var}})) / length({{var}})
    )
  return(results)
}

#' Plot Categorical Variable Distribution
#' @param df
#' @param var
#'
#' @return A ggplot object displaying the distribution of the categorical variable.
#'
#' @examples

cat_distribution <- function(df, var) {
  # barplot
  plot <- ggplot2::ggplot(df, aes(x = {{var}})) + geom_bar()
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

cat_relation <- function(df, var, y) {
  # boxplot with y variable
  plot <- ggplot2::ggplot(df, aes(x = {{var}}, y = {{y}})) + geom_boxplot()
  return(plot)
}

# devtools::document()
# devtools::install()
# open a new session
# library(xkcd)
# xkcd(600)
