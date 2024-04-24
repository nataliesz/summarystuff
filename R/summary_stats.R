
summary_stats <- function(x, ...) {
  UseMethod("summary_stats")
}

summary_stats.numeric <- function(df, var, na.rm = FALSE) {
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

summary_stats.factor <- function(df, var, na.rm = FALSE) {
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
      n = n(),
      prop = n() / nrow(df),
      n_missing = sum(is.na({{var}})),
      prop_missing = sum(is.na({{var}})) / length({{var}})
    )

  return(results)
}

# summary_stats(mtcars$mpg)
# summary_stats(iris$Species)
