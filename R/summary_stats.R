#' @title summary statistics for variables
#'
#' @description Given a variable, generates summary statistics based on its type
#'
#' @importFrom dplyr summarise
#'@importFrom dplyr n
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

check_variable_type <- function(x) {
  # Check if all values can be coerced to numeric
  if (all(!is.na(as.numeric(x)))) {
    # Check if the range of numeric values is relatively small compared to the number of unique values
    numeric_var <- as.numeric(x)
    if ((max(numeric_var) - min(numeric_var)) / length(unique(numeric_var)) < 0.1) {
      return("factor")
    } else {
      return("numeric")
    }
  }
  # Check if the number of unique values is small
  if (length(unique(x)) < 10) {
    return("factor")
  }
  # Otherwise, default to character
  return("character")
}
