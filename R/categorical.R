#' @title Generate explanatory data analysis for categorical variable
#'
#' @description
#' Given a numeric variable, this function generate EDA including summary statistics and visualization.

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

#' @param var A categorical variable for[`summarystuff`].
#' @return A data frame containing summary statistics such as count of missing
#'   values, rate of missing values, number of unique categories, and unique
#'   category values.
#' @examples
#'\dontrun{
#'}

cat_distribution <- function(df, var) {
  # barplot
  plot <- ggplot2::ggplot(df, aes(x = {{var}})) + geom_bar()
  return(plot)
}
#' Plot Categorical Variable Distribution
#' @param df the dataset from the user
#' @param var
#' @return A ggplot object displaying the distribution of the categorical variable.
#'
#' @examples
#'\dontrun{
#'}



cat_relation <- function(df, var, y) {
  # boxplot with y variable
  plot <- ggplot2::ggplot(df, aes(x = {{var}}, y = {{y}})) + geom_boxplot()
  return(plot)
}
#' Plot Categorical Variable Against Another Variable.
#' @param df the dataset from the user
#' @param var the variable(s) of interest
#' @param y The numerical variable to be plotted on y-axis.
#'
#' @return A ggplot object displaying the box plot.
#'
#' @examples
#'\dontrun{
#'}


# devtools::document()
# devtools::install()
# open a new session
# library(xkcd)
# xkcd(600)
