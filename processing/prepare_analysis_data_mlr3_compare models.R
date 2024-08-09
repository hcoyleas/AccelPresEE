# Load necessary libraries
source("init_Hannah.R")

# Get data
actigraph <- fread(actigraph)

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

# Create a copy for normalization
actigraph_cc_normalized <- copy(actigraph_cc)

# Z-score Normalization function
z_score_normalization <- function(df, var_names) {
  df[, (var_names) := lapply(.SD, function(x) (x - mean(x)) / sd(x)), .SDcols = var_names]
}

# Extract variable names for X, Y, Z components and anthropometrics
x_vars <- grep("X\\..+", colnames(actigraph), value = TRUE)
y_vars <- grep("Y\\..+", colnames(actigraph), value = TRUE)
z_vars <- grep("Z\\..+", colnames(actigraph), value = TRUE)
#anthropometrics_vars <- c("Age", "Height", "Weight")

# Apply Z-score Normalization to the normalized actigraph dataset
z_score_normalization(actigraph_cc_normalized, c(x_vars, y_vars, z_vars))


# Verify normalization
check_normalization <- function(df, var_names) {
  sapply(df[, ..var_names], function(x) c(mean = mean(x), sd = sd(x)))
}

cat("Normalization check for actigraph:\n")
#print(check_normalization(actigraph_cc_normalized, c(x_vars, y_vars, z_vars, anthropometrics_vars)))

# Convert "Sex" column to binary indicator
convert_sex_to_binary <- function(df) {
  if ("Sex" %in% colnames(df)) {
    df[, Sex := as.numeric(Sex == "M")]
  }
}

convert_sex_to_binary(actigraph_cc)
convert_sex_to_binary(actigraph_cc_normalized)

# Verify conversion
cat("Actigraph CC:\n")
print(head(actigraph_cc))
cat("Normalized Actigraph CC:\n")
print(head(actigraph_cc_normalized))

# actigraph set up
# Convert ID to factor
actigraph_cc$ID <- factor(actigraph_cc$ID)
actigraph_cc_normalized$ID <- factor(actigraph_cc_normalized$ID)
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
train_data_a_normalized <- actigraph_cc_normalized[actigraph_cc_normalized$ID %in% train_participant_ids_a, ]
test_data_a_normalized <- actigraph_cc_normalized[actigraph_cc_normalized$ID %in% test_participant_ids_a, ]
# Print participant IDs for train and test groups
cat("Participants used for train:\n")
print(train_participant_ids_a)
cat("\nParticipants used for test:\n")
print(test_participant_ids_a)

# Task
# Define the task
task <- TaskRegr$new(
  id = "actigraph", 
  backend = train_data_a, 
  target = outcome
)

task_normalized <- TaskRegr$new(
  id = "actigraph_normalized", 
  backend = train_data_a_normalized, 
  target = outcome
)

# Apply custom resampling strategy to the tasks
rsmp_loo <- rsmp("loo")
task$set_col_roles("ID", roles = "group")
rsmp_loo$instantiate(task)

task_normalized$set_col_roles("ID", roles = "group")
rsmp_loo$instantiate(task_normalized)
