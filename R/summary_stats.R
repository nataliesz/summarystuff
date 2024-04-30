#' @title summary statistics for variables
#'
#' @description Given a dataset and a variable, the function generates summary statistics based on its type
#'
#' @importFrom dplyr summarise
#' @importFrom dplyr n
#' @importFrom dplyr group_by
#'
#' @param df a dataframe containing the input variable.
#' @param var a numeric or character argument.
#' @param na.rm a logical indicating whether missing values should be removed.
#'
#' @examples
#' \dontrun{
#' data("mtcars")
#' summary_stats(mtcars$mpg, mtcars)
#' }
#' @return a dataframe of summary stats for the mpg variable.
#'
#' @exportS3Method
#'
summary_stats <- function(var, df, na.rm = FALSE) {
  UseMethod("summary_stats")
}
#' @title summary statistics for numeric variables
#'
#' @description Given a dataset and a variable, the function generates summary statistics based on its type
#'
#' @importFrom dplyr summarise
#' @importFrom dplyr n
#' @importFrom dplyr group_by
#'
#' @param df a dataframe containing the input variable.
#' @param var a numeric or character argument.
#' @param na.rm a logical indicating whether missing values should be removed.
#'
#' @examples
#' \dontrun{
#' data("mtcars")
#' summary_stats(mtcars$mpg, mtcars)
#' }
#' @return a dataframe of summary stats for the mpg variable.
#'
#' @exportS3Method
#'
summary_stats.numeric <- function(var, df, na.rm = FALSE) {
  var_name <- deparse(substitute(var))

  if (na.rm) {
    x <- na.omit(var)
  }

  if (any(is.na(var))) {
    stop("Cannot compute the summary stats of ", var_name, " because ", var_name, " contains NA values.")
  }

  results <- summarise(df,
                       min = min({{var}}),
                       q1 = quantile({{var}}, 0.25),
                       median = median({{var}}),
                       mean = mean({{var}}),
                       q3 = quantile({{var}}, 0.75),
                       max = max({{var}}),
                       sd = sd({{var}}),
                       n_missing = sum(is.na({{var}})),
                       rate_missing = mean(!is.na({{var}})))

  return(results)
}
#' @title summary statistics for factor variables
#'
#' @description Given a dataset and a variable, the function generates summary statistics based on its type
#'
#' @importFrom dplyr summarise
#' @importFrom dplyr n
#' @importFrom dplyr group_by
#'
#' @param df a dataframe containing the input variable.
#' @param var a numeric or character argument.
#' @param na.rm a logical indicating whether missing values should be removed.
#'
#' @examples
#' \dontrun{
#' data("iris")
#' summary_stats(iris$Species, iris)
#' }
#' @return a dataframe of summary stats for the mpg variable.
#'
#' @exportS3Method
#'
summary_stats.factor <- function(var, df, na.rm = FALSE) {
  var_name <- deparse(substitute(var))

  if (na.rm) {
    x <- na.omit(var)
  }

  if (any(is.na(var))) {
    stop("Cannot compute the summary stats of ", var_name, " because ", var_name, " contains NA values.")
  }

  results <- df %>%
    group_by({{var}}) %>%
    summarise(n = n(),
              prop = n() / nrow(df),
              n_missing = sum(is.na({{var}})),
              prop_missing = sum(is.na({{var}})) / length({{var}}))

  return(results)
}
