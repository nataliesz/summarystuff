#' @title
#' generate explanatory data analysis for numeric variable
#' @description
#' Given a numeric variable, this function generate EDA including summary statistics and visualization.

categorical_ss <- function(var) {
  # categorical
  var_name <- as.character(var)
  n_missing <- sum(is.na(var))
  rate_missing <- sum(!is.na(var)) / length(var)
  n_unique <- length(unique(var))
  value_unique <- unique(var)

  result <- list(var_name, n_missing, rate_missing, n_unique, value_unique)
  return(result)
}

categorical_v <- function(df, var) {
  # barplot
  plot <- ggplot2::ggplot(df, aes(x = var)) + geom_bar()
  return(plot)
}

categorical_y <- function(df, var, y) {
  # boxplot with y variable
  plot <- ggplot2::ggplot(df, aes(x = var, y = y)) + geom_boxplot()
  return(plot)
}

# devtools::document()
# devtools::install()
# open a new session
# library(xkcd)
# xkcd(600)
