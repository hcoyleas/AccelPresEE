# Load necessary libraries
source("init.R")

# Get data
actigraph <- fread(actigraph)
opal_left <- fread(opal_left)
opal_right <- fread(opal_right)

# Variable selection
vars <- c(
  grep("X\\..+", colnames(actigraph), value = TRUE), 
  grep("Y\\..+", colnames(actigraph), value = TRUE), 
  grep("Z\\..+", colnames(actigraph), value = TRUE), 
  "ID", outcome
)

if (participant_anthroprometrics_option == "Yes") {
  vars <- c(vars, "Age", "Height", "Weight", "Sex")
}

# Complete cases only
actigraph_cc <- actigraph[complete.cases(actigraph), ..vars]
opal_left_cc <- opal_left[complete.cases(opal_left), ..vars]
opal_right_cc <- opal_right[complete.cases(opal_right), ..vars]

# Z-score Normalization function
z_score_normalization <- function(df, var_names) {
  df[, (var_names) := lapply(.SD, function(x) (x - mean(x)) / sd(x)), .SDcols = var_names]
}

# Extract variable names for X, Y, Z components and anthropometrics
x_vars <- grep("X\\..+", colnames(actigraph), value = TRUE)
y_vars <- grep("Y\\..+", colnames(actigraph), value = TRUE)
z_vars <- grep("Z\\..+", colnames(actigraph), value = TRUE)
#anthropometrics_vars <- c("Age", "Height", "Weight")

# Apply Z-score Normalization to each dataset
z_score_normalization(actigraph_cc, c(x_vars, y_vars, z_vars))
z_score_normalization(opal_left_cc, c(x_vars, y_vars, z_vars))
z_score_normalization(opal_right_cc, c(x_vars, y_vars, z_vars))

# Verify normalization
check_normalization <- function(df, var_names) {
  sapply(df[, ..var_names], function(x) c(mean = mean(x), sd = sd(x)))
}

cat("Normalization check for actigraph:\n")
print(check_normalization(actigraph_cc, c(x_vars, y_vars, z_vars)))
cat("Normalization check for opal_left:\n")
print(check_normalization(opal_left_cc, c(x_vars, y_vars, z_vars)))
cat("Normalization check for opal_right:\n")
print(check_normalization(opal_right_cc, c(x_vars, y_vars, z_vars)))

# Convert "Sex" column to binary indicator
convert_sex_to_binary <- function(df) {
  if ("Sex" %in% colnames(df)) {
    df[, Sex := as.numeric(Sex == "M")]
  }
}

convert_sex_to_binary(actigraph_cc)
convert_sex_to_binary(opal_left_cc)
convert_sex_to_binary(opal_right_cc)

# Verify conversion
cat("Actigraph CC:\n")
print(head(actigraph_cc))
cat("Opal Left CC:\n")
print(head(opal_left_cc))
cat("Opal Right CC:\n")
print(head(opal_right_cc))

# actigraph set up
# Convert ID to factor
actigraph_cc$ID <- factor(actigraph_cc$ID)
# Get unique participant IDs
participant_ids_a <- unique(actigraph_cc$ID)
print(participant_ids_a)
# Set seed for reproducibility
set.seed(123)
# Determine the number of participants for the train group
train_size <- ceiling(2/3 * length(participant_ids_a))
# Split participant IDs into train and test groups
train_participant_ids_a <- participant_ids_a[1:train_size]
test_participant_ids_a <- participant_ids_a[(train_size + 1):length(participant_ids_a)]
# Subset the data based on the train and test participant IDs
train_data_a <- actigraph_cc[actigraph_cc$ID %in% train_participant_ids_a, ]
test_data_a <- actigraph_cc[actigraph_cc$ID %in% test_participant_ids_a, ]
# Print participant IDs for train and test groups
cat("Participants used for train:\n")
print(train_participant_ids_a)
cat("\nParticipants used for test:\n")
print(test_participant_ids_a)

# opal_right set up
# Convert ID to factor
opal_right_cc$ID <- factor(opal_right_cc$ID)
# Get unique participant IDs
participant_ids_or <- unique(opal_right_cc$ID)
# Set seed for reproducibility
set.seed(123)
# Determine the number of participants for the train group
train_size <- ceiling(2/3 * length(participant_ids_or))
# Split participant IDs into train and test groups
train_participant_ids_or <- participant_ids_or[1:train_size]
test_participant_ids_or <- participant_ids_or[(train_size + 1):length(participant_ids_or)]
# Subset the data based on the train and test participant IDs
train_data_or <- opal_right_cc[opal_right_cc$ID %in% train_participant_ids_or, ]
test_data_or <- opal_right_cc[opal_right_cc$ID %in% test_participant_ids_or, ]
# Print participant IDs for train and test groups
cat("Participants used for train:\n")
print(train_participant_ids_or)
cat("\nParticipants used for test:\n")
print(test_participant_ids_or)

# opal_left set up
# Convert ID to factor
opal_left_cc$ID <- factor(opal_left_cc$ID)
# Get unique participant IDs
participant_ids_ol <- unique(opal_left_cc$ID)
# Set seed for reproducibility
set.seed(123)
# Determine the number of participants for the train group
train_size <- ceiling(2/3 * length(participant_ids_ol))
# Split participant IDs into train and test groups
train_participant_ids_ol <- participant_ids_ol[1:train_size]
test_participant_ids_ol <- participant_ids_ol[(train_size + 1):length(participant_ids_ol)]
# Subset the data based on the train and test participant IDs
train_data_ol <- opal_left_cc[opal_left_cc$ID %in% train_participant_ids_ol, ]
test_data_ol <- opal_left_cc[opal_left_cc$ID %in% test_participant_ids_ol, ]
# Print participant IDs for train and test groups
cat("Participants used for train:\n")
print(train_participant_ids_ol)
cat("\nParticipants used for test:\n")
print(test_participant_ids_ol)

# Verify final datasets
cat("Train Data OL:\n")
print(head(train_data_ol))
cat("Test Data OL:\n")
print(head(test_data_ol))

# Task
# Define the tasks
tasks <- list(
  TaskRegr$new(
    id = "actigraph_right", 
    backend = train_data_a, 
    target = outcome),
  
  TaskRegr$new(
    id = "opal_right", 
    backend = train_data_or, 
    target = outcome),
  
  TaskRegr$new(
    id = "opal_left", 
    backend = train_data_ol, 
    target = outcome)
)

# Apply custom resampling strategy to each task
resamplings <- lapply(tasks, function(task) {
  rsmp_loo <- rsmp("loo")
  task$set_col_roles("ID", roles = "group")
  rsmp_loo$instantiate(task)
  rsmp_loo
})
