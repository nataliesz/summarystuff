
numeric_statistic <- function(df, var) {
  # categorical
  var_name <- as.character(var)
  n_missing <- sum(is.na(df$var))
  rate_missing <- sum(!is.na(df$var)) / length(df$var)
  mean <- mean(df$var)
  median <- median(df$var)
  sd <- sd(df$var)
  min <- min(df$var)
  max <- max(df$var)

  result <- data.frame(var_name, n_missing, rate_missing, mean, median, sd, min, max)
}

numeric_v <- function(df, var) {
  # histogram
  ggplot(df, aes(x = var)) +
    geom_histogram()
}

numeric_y <- function(df, var, y) {
  # boxplot with y variable
  ggplot(df, aes(x = var, y = var)) +
    geom_boxplot()
}
