#' @title
#' generate explanatory data analysis for numeric variable
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

cat_distribution <- function(df, var) {
  # barplot
  plot <- ggplot2::ggplot(df, aes(x = var)) + geom_bar()
  return(plot)
}

cat_ <- function(df, var, y) {
  # boxplot with y variable
  plot <- ggplot2::ggplot(df, aes(x = var, y = y)) + geom_boxplot()
  return(plot)
}

# devtools::document()
# devtools::install()
# open a new session
# library(xkcd)
# xkcd(600)
