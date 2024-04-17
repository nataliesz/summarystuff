#' @title
#' generate explanatory data analysis for numeric variable
#' @description
#' Given a numeric variable, this function generate EDA including summary statistics and visualization.

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
