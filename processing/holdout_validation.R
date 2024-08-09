library(mlr3)
library(mlr3viz)
library(dplyr)
library(ggplot2)

# Load the necessary initialization and data preparation scripts
source("init_Hannah.R")
source("prepare_analysis_data_mlr3_compare models.R")

# Load a trained random forest model
learner_rf <- readRDS("results/Models/random_forest_model_actigraph_15delay_METs_est.rds")

# Get unique participant IDs
participant_ids <- unique(actigraph_cc$ID)

# Set seed for reproducibility
set.seed(123)

# Determine the number of participants for the train group
train_size <- ceiling(2/3 * length(participant_ids))

# Split participant IDs into train and test groups
train_participant_ids <- participant_ids[1:train_size]
test_participant_ids <- participant_ids[(train_size + 1):length(participant_ids)]

# Subset the data based on the test participant IDs
test_data <- actigraph_cc[actigraph_cc$ID %in% test_participant_ids, ]

# Generate predictions on the test data using the trained model
predictions_rf <- learner_rf$predict_newdata(newdata = test_data)

# Create a data frame containing the actual and predicted values
results <- data.frame(ID = test_data$ID, Actual = test_data$METs_est, Predicted_rf = predictions_rf$response)

# Calculate RMSE for random forest predictions per participant
rmse_rf <- results %>%
  group_by(ID) %>%
  summarize(RMSE_rf = sqrt(mean((Actual - Predicted_rf)^2)))

# Print the RMSE results
print(rmse_rf)

# Calculate and print the average RMSE
average_rmse <- mean(rmse_rf$RMSE_rf)
cat("Average RMSE:", average_rmse, "\n")

# Save the holdout validation results to an RDS file
saveRDS(rmse_rf, file = "/.../.../....rds")

# Boxplot of RMSE values for all participants together
boxplot_rmse <- ggplot(rmse_rf, aes(x = "", y = RMSE_rf)) +
  geom_boxplot() +
  labs(x = "", y = "RMSE", title = "RMSE for Random Forest Model (Holdout Validation)") +
  theme_minimal()

# Print the boxplot
print(boxplot_rmse)

# Save the boxplot as an image file
ggsave("/.../.../....png", plot = boxplot_rmse)

