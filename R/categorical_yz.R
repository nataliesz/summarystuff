
categorical_ss <- function(df, var) {
  # categorical
  var_name <- as.character(var)
  n_missing <- sum(is.na(df$var))
  rate_missing <- sum(!is.na(df$var)) / length(df$var)
  n_unique <- length(unique(df$var))
  value_unique <- unique(df$var)

  result <- data.frame(var_name, n_missing, rate_missing, n_unique, value_unique)
  return(result)
}

categorical_v <- function(df, var) {
  # barplot
  plot <- ggplot(df, aes(x = var)) +
    geom_bar()
  return(plot)
}

categorical_y <- function(df, var, y) {
  # boxplot with y variable
  plot <- ggplot(df, aes(x = var, y = y)) +
    geom_boxplot()
  return(plot)
}
