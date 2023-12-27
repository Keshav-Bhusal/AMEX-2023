set.seed(123)  
sample_indices <- sample(nrow(early_legal), size = 0.8 * nrow(early_legal))
training_data <- early_legal[sample_indices, ]
validation_data <- early_legal[-sample_indices, ]

# Define a range of k values to test
k_values <- 1:100
  
  #c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21)

# Initialize variables to store results
best_k <- NULL
best_metric <- Inf

# Loop over k values
for (k in k_values) {
  # Impute missing values on the training set using the current k
  imputed_data <- kNN(training_data, variable = c(
      "age_num_days", "message_DPB", "setup_cdss",
      "total_case_amount_due", "total_case_exposure", "total_past_due"),k = k)
  
  # Calculate your chosen evaluation metric on the validation set
  rmse <- sqrt(mean((validation_data$total_past_due - imputed_data$total_past_due)^2, na.rm = TRUE))
  
  if (rmse < best_metric) {
    best_metric <- rmse
    best_k <- k
  }
}

# The best k value is stored in best_k
cat("The best k value is:", best_k, "\n")

# Apply kNN imputation using the best k to your entire dataset
final_imputed_data <- kNN(
  early_legal,
  variable = c(
    "age_num_days", "message_DPB", "setup_cdss",
    "total_case_amount_due", "total_case_exposure", "total_past_due"
  ),
  k = best_k
)

ggplot(results, aes(x = K, y = RMSE)) +
  geom_line() +
  labs(x = "K Value", y = "RMSE") +
  theme_minimal()