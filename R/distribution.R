#' @title distribution plots for variables
#'
#' @description Given a variable, generates plot displaying distribution based on its type
#'
#' @importFrom ggplot2 ggplot
#'
#' @param df a dataframe containing the input variable.
#' @param var a numeric or character argument.
#'
#' @examples
#' data("mtcars")
#' distribution(mpg$mtcars, mtcars)
#' @return a dataframe of summary stats for the mpg variable.
#' @examples
#' data("iris")
#' distribution(Species$iris, iris)
#' @return a dataframe of summary stats for the Species variable.

distribution <- function(x, ...) {
  UseMethod("distribution")
}

distribution.numeric <- function(var, df) {
  var_name <- deparse(substitute(var))

  ggplot2::ggplot(df, ggplot2::aes(var)) +
    ggplot2::geom_histogram() +
    ggplot2::labs(x = var_name)
}

distribution.factor <- function(var, df) {
  var_name <- deparse(substitute(var))

  ggplot2::ggplot(df, ggplot2::aes(var)) +
    ggplot2::geom_bar() +
    ggplot2::labs(x = var_name)
}
