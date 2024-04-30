#' @title distribution plots for variables
#'
#' @description Given a variable, generates plot displaying distribution based on its type
#'
#' @importFrom ggplot2 ggplot
#'
#' @param df a dataframe containing the input variable.
#' @param var a numeric or character argument.
#' @param ... optional argument for user to specify binwidth (default is 30)
#'
#' @examples
#' \dontrun{
#' data("mtcars")
#' distribution(mtcars$mpg, mtcars)
#' }
#' @return a histogram for the mpg variable.
#'
#' @export
#'
distribution <- function(var, df, ...) {
  UseMethod("distribution")
}
#' @title distribution plot for numeric variables
#'
#' @description Given a numeric variable, generates a histogram displaying distribution with the option of specifying the number of bins
#'
#' @importFrom ggplot2 ggplot
#'
#' @param df a dataframe containing the input variable.
#' @param var a numeric or character argument.
#' @param ... optional argument for user to specify binwidth (default is 30)
#'
#' @examples
#' data("mtcars")
#' distribution(mtcars$drat, mtcars)
#' @return a histogram for the drat variable.
#'
#' @exportS3Method
#'
distribution.numeric <- function(var, df, ...) {
  var_name <- deparse(substitute(var))

  ggplot2::ggplot(df, ggplot2::aes(var)) +
    ggplot2::geom_histogram(...) +
    ggplot2::labs(x = var_name)
}
#' @title distribution plot for factor variables
#'
#' @description Given a factor variable, generates a histogram displaying distribution counts
#'
#' @importFrom ggplot2 ggplot
#'
#' @param df a dataframe containing the input variable.
#' @param var a factor variable argument.
#' @param ... optional argument for user to specify binwidth (default is 30)
#'
#' @examples
#' data("mtcars")
#' distribution(mtcars$cyl, mtcars)
#' @return a histogram of counts for each of the levels for the cyl variable.
#'
#' @exportS3Method
#'
distribution.factor <- function(var, df, ...) {
  var_name <- deparse(substitute(var))

  ggplot2::ggplot(df, ggplot2::aes(var)) +
    ggplot2::geom_bar() +
    ggplot2::labs(x = var_name)
}
