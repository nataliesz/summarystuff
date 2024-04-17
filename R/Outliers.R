
# Extracting outliers

outliers<- function(df,var){
  # extract the outliers
  extracted_outliers<- boxplot.stats(df, var)$out
  return(extracted_outliers)
}
