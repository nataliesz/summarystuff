## code to prepare `sample_df` dataset goes here

sample_df <- data.frame(ID = 1:50,
                        salary = sample(30000:500000, size = 50, replace = TRUE),
                        tax_amt = sample(30:1000, size = 50, replace = TRUE),
                        age = sample(18:65, size = 50, replace = TRUE),
                        occupation = sample(c("professor", "CEO", "lawyer", "salesperson"), size = 50, replace = TRUE))

usethis::use_data(sample_df, overwrite = TRUE)
