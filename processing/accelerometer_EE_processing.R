# Load necessary libraries
source("init.R")

# Function for frequency features
calculate_dominant_frequency <- function(signal, fs) {
  fft_result <- abs(fft(signal))
  max_freq_index <- which.max(fft_result[2:(length(fft_result) / 2)])
  max_freq <- max_freq_index * fs / length(signal)
  return(max_freq)
}

calculate_amplitude_spectral_peak <- function(signal, fs) {
  fft_result <- abs(fft(signal))
  amplitude_spectral_peak <- max(fft_result[2:(length(fft_result) / 2)])
  return(amplitude_spectral_peak)
}

calculate_25th_percentile_frequency <- function(signal, fs) {
  fft_result <- abs(fft(signal))
  freq_indices <- 2:(length(fft_result) / 2)
  fft_values <- fft_result[freq_indices]
  
  percentile_25 <- quantile(fft_values, 0.25)
  
  return(percentile_25)
}

calculate_50th_percentile_frequency <- function(signal, fs) {
  fft_result <- abs(fft(signal))
  freq_indices <- 2:(length(fft_result) / 2)
  fft_values <- fft_result[freq_indices]
  
  percentile_50 <- quantile(fft_values, 0.50)
  
  return(percentile_50)
}

calculate_75th_percentile_frequency <- function(signal, fs) {
  fft_result <- abs(fft(signal))
  freq_indices <- 2:(length(fft_result) / 2)
  fft_values <- fft_result[freq_indices]
  
  percentile_75 <- quantile(fft_values, 0.75)
  
  return(percentile_75)
}

calculate_90th_percentile_frequency <- function(signal, fs) {
  fft_result <- abs(fft(signal))
  freq_indices <- 2:(length(fft_result) / 2)
  fft_values <- fft_result[freq_indices]
  
  percentile_90 <- quantile(fft_values, 0.90)
  
  return(percentile_90)
}

##__________________________##

# Function for time domain features
feature_extraction <- function(x) {
  percentiles <- as.list(quantile(x, c(0.1, 0.25, 0.5, 0.75, 0.9), names = FALSE))
  names(percentiles) <- c("p10", "p25", "p50", "p75", "p90")
  cov <- acf(x, lag.max = 1, plot = FALSE)$acf[2, , ]
  if (is.na(cov)) {
    cov <- 0
  }
  names(cov) <- c("cov")
  list(mean = mean(x), sd = sd(x), min = min(x), max = max(x), cov, percentiles)
}

##__________________________##

# Function to process accelerometer data
process_accelerometer_data <- function(file_path, frequency_feature_option, abs_value_option) {
  
  # Read accelerometer data
  accelerometer_data <- fread(file_path)
  
  # Extract participant ID from file name
  participant_id <- gsub("_.*", "", basename(file_path))
  
  # Rename columns
  setnames(accelerometer_data, c("Timestamp", "X", "Y", "Z"))
  
  # Perform filtering if option is set to "Filtered"
  if (accelerometer_processing_option == "Filtered") {
    # Perform filtering using Butterworth Bandpass filter
    # Individual filtering for X, Y, Z axes
    # Sampling frequency
    fs <- 100 
    
    # Filter order
    order <- 4
    # Critical frequencies; note this is a digital filter these cutoffs have been normalized as per Nyquist theorem; 0.3 to 10 Hz
    cutoffs <- c(0.006, 0.2)
    
    # Apply Butterworth filter
    bf <- butter(order, cutoffs, type = c("pass"))
    filtered_x <- filter(bf, accelerometer_data$X)
    filtered_y <- filter(bf, accelerometer_data$Y)
    filtered_z <- filter(bf, accelerometer_data$Z)
    
    # Combine filtered data
    filtered_data <- data.frame(Timestamp = accelerometer_data$Timestamp, 
                                filtered_x, filtered_y, filtered_z)
    # Rename column
    setnames(filtered_data, c("Timestamp", "X", "Y", "Z"))
    
    # Convert timestamp column to POSIXct
    filtered_data$Timestamp <- as.POSIXct(filtered_data$Timestamp, format = "%Y-%m-%d %H:%M:%S")
    
    # Floor the timestamp column to the nearest 30 seconds
    filtered_data$Timestamp <- lubridate::floor_date(filtered_data$Timestamp, "30 sec")
    setDT(filtered_data)
    
    # Calculate dominant frequency for each 30-second interval for each axis
    if (frequency_feature_option == "Yes") {
      dominant_frequencies <- filtered_data[, .(X.domfreq = calculate_dominant_frequency(X, fs),
                                                Y.domfreq = calculate_dominant_frequency(Y, fs),
                                                Z.domfreq = calculate_dominant_frequency(Z, fs),
                                                X.25f = calculate_25th_percentile_frequency (X, fs),
                                                Z.25f = calculate_25th_percentile_frequency (Z, fs),
                                                Y.25f = calculate_25th_percentile_frequency (Y, fs),
                                                X.50f = calculate_50th_percentile_frequency (X, fs),
                                                Z.50f = calculate_50th_percentile_frequency (Z, fs),
                                                Y.50f = calculate_50th_percentile_frequency (Y, fs),
                                                X.75f = calculate_75th_percentile_frequency (X, fs),
                                                Z.75f = calculate_75th_percentile_frequency (Z, fs),
                                                Y.75f = calculate_75th_percentile_frequency (Y, fs),
                                                X.90f = calculate_90th_percentile_frequency (X, fs),
                                                Z.90f = calculate_90th_percentile_frequency (Z, fs),
                                                Y.90f = calculate_90th_percentile_frequency (Y, fs),
                                                X.specpeak =calculate_amplitude_spectral_peak(X, fs),
                                                Y.specpeak =calculate_amplitude_spectral_peak(Y, fs),
                                                Z.specpeak =calculate_amplitude_spectral_peak(Z, fs)), by = Timestamp]
      
      
      
    } else {
      dominant_frequencies <- NULL  # Set dominant_frequencies to NULL if frequency_feature_option is "No"
    }
    
    # Take absolute value of data if option is set to "Yes"
    if (abs_value_option == "Yes") {
      filtered_data[, c("X", "Y", "Z") := lapply(.SD, abs), .SDcols = c("X", "Y", "Z")]
    } else {
      # Do nothing if option is set to "No"
    }
    
    
    # Perform feature extraction
    features <- filtered_data[, as.list(unlist(lapply(.SD, feature_extraction))), by = "Timestamp", .SDcols = c("X", "Y", "Z")]
    
    # Read corresponding energy expenditure data
    ee_file_path <- file.path(ee_data_folder, paste0(participant_id, "_EE values.csv"))
    ee_data <- fread(ee_file_path, select = c("Timestamp", outcome))
    
    # Convert timestamp column to POSIXct
    ee_data$Timestamp <- as.POSIXct(ee_data$Timestamp, format = "%Y-%m-%d %H:%M:%S")
    
    # Merge feature data with energy expenditure data based on timestamp
    combined_data <- merge(features, ee_data, by = "Timestamp", all.x = TRUE)
    
    # Merge dominant frequencies with combined data based on timestamp
    if (!is.null(dominant_frequencies)) {
      combined_data <- merge(combined_data, dominant_frequencies, by = "Timestamp", all.x = TRUE)
    }
    
    # Drop rows with NA values in the EE column (METs_est or kJ/min)
    combined_data <- combined_data[complete.cases(combined_data$METs_est), ]
    
    # Merge participant anthropometrics data if option is set to "Yes"
    if (participant_anthroprometrics_option == "Yes") {
      # Read participant anthropometrics data
      anthropometrics_data <- fread(participant_anthroprometrics_file)
      
      # Filter anthropometrics data for the current participant
      participant_anthroprometrics <- subset(anthropometrics_data, ID == participant_id)
      
      # Replicate anthropometrics data to match the length of combined data
      num_rows <- nrow(combined_data)
      replicated_anthroprometrics <- participant_anthroprometrics[rep(seq_len(nrow(participant_anthroprometrics)), length.out = num_rows), ]
      
      # Ensure replicated_anthroprometrics has the same row names as combined_data
      rownames(replicated_anthroprometrics) <- rownames(combined_data)
      
      # Select relevant columns from anthropometrics data
      anthropometrics_selected <- replicated_anthroprometrics[, c("Age", "Weight", "Height", "Sex")]
      
      # Combine combined_data and anthropometrics_selected by row
      combined_data <- cbind(combined_data, anthropometrics_selected)
    }
    
    # Save combined data to CSV file with modified column names
    output_file <- file.path(processed_data_folder, paste0(participant_id, "_combined_data.csv"))
    fwrite(combined_data, output_file, quote = FALSE)
    
    return(combined_data)
    
  } else {
    # If no filtering is required, return the raw accelerometer data
    
    # Sampling frequency
    fs <- 100
    
    # Convert timestamp column to POSIXct
    accelerometer_data$Timestamp <- as.POSIXct(accelerometer_data$Timestamp, format = "%Y-%m-%d %H:%M:%S")
    
    # Floor the timestamp column to the nearest 30 seconds
    accelerometer_data$Timestamp <- lubridate::floor_date(accelerometer_data$Timestamp, "30 sec")
    setDT(accelerometer_data)
    
    
    # Sampling frequency
    fs <- 100
    
    # Calculate dominant frequency for each 30-second interval for each axis
    if (frequency_feature_option == "Yes") {
      dominant_frequencies <- accelerometer_data[, .(X.domfreq = calculate_dominant_frequency(X, fs),
                                                     Y.domfreq = calculate_dominant_frequency(Y, fs),
                                                     Z.domfreq = calculate_dominant_frequency(Z, fs),
                                                     X.25f = calculate_25th_percentile_frequency (X, fs),
                                                     Z.25f = calculate_25th_percentile_frequency (Z, fs),
                                                     Y.25f = calculate_25th_percentile_frequency (Y, fs),
                                                     X.50f = calculate_50th_percentile_frequency (X, fs),
                                                     Z.50f = calculate_50th_percentile_frequency (Z, fs),
                                                     Y.50f = calculate_50th_percentile_frequency (Y, fs),
                                                     X.75f = calculate_75th_percentile_frequency (X, fs),
                                                     Z.75f = calculate_75th_percentile_frequency (Z, fs),
                                                     Y.75f = calculate_75th_percentile_frequency (Y, fs),
                                                     X.90f = calculate_90th_percentile_frequency (X, fs),
                                                     Z.90f = calculate_90th_percentile_frequency (Z, fs),
                                                     Y.90f = calculate_90th_percentile_frequency (Y, fs),
                                                     X.specpeak =calculate_amplitude_spectral_peak(X, fs),
                                                     Y.specpeak =calculate_amplitude_spectral_peak(Y, fs),
                                                     Z.specpeak =calculate_amplitude_spectral_peak(Z, fs)), by = Timestamp]
      
    } else {
      dominant_frequencies <- NULL  # Set dominant_frequencies to NULL if frequency_feature_option is "No"
    }
    
    
    # Take absolute value of data if option is set to "Yes"
    if (abs_value_option == "Yes") {
      accelerometer_data[, c("X", "Y", "Z") := lapply(.SD, abs), .SDcols = c("X", "Y", "Z")]
    } else {
      # Do nothing if option is set to "No"
    }
    
    # Perform feature extraction
    features <- accelerometer_data[, as.list(unlist(lapply(.SD, feature_extraction))), by = "Timestamp", .SDcols = c("X", "Y", "Z")]
    
    # Read corresponding energy expenditure data
    ee_file_path <- file.path(ee_data_folder, paste0(participant_id, "_EE values.csv"))
    ee_data <- fread(ee_file_path, select = c("Timestamp", outcome))
    
    # Convert timestamp column to POSIXct
    ee_data$Timestamp <- as.POSIXct(ee_data$Timestamp, format = "%Y-%m-%d %H:%M:%S")
    
    # Merge feature data with energy expenditure data based on timestamp
    combined_data <- merge(features, ee_data, by = "Timestamp", all.x = TRUE)
    
    # Merge dominant frequencies with combined data based on timestamp
    if (!is.null(dominant_frequencies)) {
      combined_data <- merge(combined_data, dominant_frequencies, by = "Timestamp", all.x = TRUE)
    }
    
    # Drop rows with NA values in the METs_est column or kJ/min
    combined_data <- combined_data[complete.cases(combined_data$METs_est), ]
    
    # Merge participant anthropometrics data if option is set to "Yes"
    if (participant_anthroprometrics_option == "Yes") {
      # Read participant anthropometrics data
      anthropometrics_data <- fread(participant_anthroprometrics_file)
      
      # Filter anthropometrics data for the current participant
      participant_anthroprometrics <- subset(anthropometrics_data, ID == participant_id)
      
      # Replicate anthropometrics data to match the length of combined data
      num_rows <- nrow(combined_data)
      replicated_anthroprometrics <- participant_anthroprometrics[rep(seq_len(nrow(participant_anthroprometrics)), length.out = num_rows), ]
      
      # Ensure replicated_anthroprometrics has the same row names as combined_data
      rownames(replicated_anthroprometrics) <- rownames(combined_data)
      
      # Select relevant columns from anthropometrics data
      anthropometrics_selected <- replicated_anthroprometrics[, c("Age", "Weight", "Height", "Sex")]
      
      # Combine combined_data and anthropometrics_selected by row
      combined_data <- cbind(combined_data, anthropometrics_selected)
    }
    
    # Save combined data to CSV file with modified column names
    output_file <- file.path(processed_data_folder, paste0(participant_id, "_combined_data.csv"))
    fwrite(combined_data, output_file, quote = FALSE)
    
    return(combined_data)
  }
}

# Iterate over accelerometer files and process data for each participant
accelerometer_files <- list.files(accelerometer_data_folder, full.names = TRUE)
for (file in accelerometer_files) {
  process_accelerometer_data(file, frequency_feature_option = frequency_feature_option, abs_value_option = abs_value_option)
}

print("Done accelerometer and energy expenditure data processing!")
