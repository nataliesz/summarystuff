#' @title summary statistics for variables
#'
#' @description Given a variable, generates summary statistics based on its type
#'
#' @importFrom dplyr summarise
#'
#' @param df a dataframe containing the input variable.
#' @param var a numeric or character argument.
#' @param na.rm a logical indicating whether missing values should be removed.
#'
#' @examples
#' data("mtcars")
#' summary_stats(mtcars$mpg, mtcars)
#' @return a dataframe of summary stats for the mpg variable.
#' @examples
#' data("iris")
#' summary_stats(iris$Species, iris)
#' @return a dataframe of summary stats for the Species variable.

summary_stats <- function(x, ...) {
  UseMethod("summary_stats")
}

summary_stats.numeric <- function(var, df, na.rm = FALSE) {
  var_name <- deparse(substitute(var))

  if (na.rm) {
    x <- na.omit(var)
  }

  if (any(is.na(var))) {
    stop("Cannot compute the summary stats of ", var_name, " because ", var_name, " contains NA values.")
  }

  results <- df |>
    dplyr::summarise(min = min(var),
                     q1 = quantile(var, 0.25),
                     median = median(var),
                     mean = mean(var),
                     q3 = quantile(var, 0.75),
                     max = max(var),
                     sd = sd(var),
                     n_missing = sum(is.na(var)),
                     rate_mising = sum(!is.na(var)) / length(var))

  return(results)
}

summary_stats.factor <- function(var, df, na.rm = FALSE) {
  var_name <- deparse(substitute(var))

  if (na.rm) {
    x <- na.omit(var)
  }

  if (any(is.na(var))) {
    stop("Cannot compute the summary stats of ", var_name, " because ", var_name, " contains NA values.")
  }

  results <- df |>
    dplyr::group_by({{var}}) |>
    dplyr::summarise(
      n = dplyr::n(),
      prop = dplyr::n() / nrow(df),
      n_missing = sum(is.na({{var}})),
      prop_missing = sum(is.na({{var}})) / length({{var}})
    )

  return(results)
}
