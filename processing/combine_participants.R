# Load necessary libraries
source("init.R")


# Read all processed files in the processed data folder
processed_files <- list.files(processed_data_folder, full.names = TRUE)

# Initialize an empty list to store processed data frames
processed_data_list <- list()

# Iterate over each processed file
for (file in processed_files) {
  # Read processed data
  processed_data <- fread(file)
  
  # Extract participant ID from the file name
  participant_id <- sub("_.*", "", basename(file))
  
  # Add ID column
  processed_data$ID <- participant_id
  
  # Append processed data to the list
  processed_data_list[[participant_id]] <- processed_data
}

# Combine all processed data frames into a single data table
combined_data <- bind_rows(processed_data_list)

# Reorder columns to have "ID" as the first column
combined_data <- combined_data %>% select(ID, everything())

# Save combined data to a CSV file
write.csv(combined_data, file_actigraph_right, row.names = FALSE)


print("Done combining participants!")
