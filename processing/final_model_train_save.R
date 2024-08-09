library(mlr3viz)
# Include the initialization code
source("init.R")
source("prepare_analysis_data_mlr3_compare models.R")

# Get unique participant IDs
participant_ids <- unique(actigraph_cc$ID)

# Set seed for reproducibility
set.seed(123)

# Determine the number of participants for the train group
train_size <- ceiling(2/3 * length(participant_ids))

# Split participant IDs into train and test groups
train_participant_ids <- participant_ids[1:train_size]
test_participant_ids <- participant_ids[(train_size + 1):length(participant_ids)]

# Subset the data based on the train and test participant IDs
train_data <- actigraph_cc[actigraph_cc$ID %in% train_participant_ids, ]

# Print participant IDs for train and test groups
cat("Participants used for train:\n")
print(train_participant_ids)

cat("\nParticipants used for test:\n")
print(test_participant_ids)

# Define a custom task with group column
task_loo <- TaskRegr$new("actigraph_right_loo", 
                         backend = train_data, 
                         target = outcome)

# Define a custom resampling strategy for LOO with grouping at the participant level
rsmp_loo <- rsmp("loo")
task_grp <- task_loo
task_grp$set_col_roles("ID", "group")
rsmp_loo$instantiate(task_grp)

# Learner for Holdout Validation
if (outcome == "METs_est" || outcome == "kJ") {
  learner_rf <- lrn("regr.ranger", id = "RF", num.trees = 500)
} else {
  stop("Unknown outcome.")
}

# Train the random forest model
learner_rf$train(task_loo)

# Save the trained model to disk
saveRDS(learner_rf, file = "results/Models/random_forest_model_opal_left_original_METs_est.rds")

# Print confirmation message
cat("Random forest model has been trained and saved as random_forest_model.rds\n")

