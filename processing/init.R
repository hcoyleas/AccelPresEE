# Load necessary libraries
library(data.table)
library(mlr3)
library(ggplot2)
library(parallelMap)
library(dplyr)
library(signal)
library(lubridate)
library(ranger)
library(mlr3tuning)
library(mlr3learners)
library(mlr3mbo)
library(paradox)


# Set working directory
setwd("/Volumes/My Passport/PhD/Machine learning")
#Time interval (30 seconds)
interval <- 30
#Energy expenditure outcome measure
outcome <- "METs_est" # options: METs_est, kJ/min
#sampling frequency
fs <- 100

# Adjust the file path to point to the CSV file
file_actigraph_right <- paste0("data/actigraph_right_", interval, "s.csv") #Options: actigraph_right_, opal_right_, opal_left_

# Specify the option for accelerometer data processing
accelerometer_processing_option <- "Filtered"  # Options: "Filtered" or "Unfiltered"

# Option to include dominant frequency as a feature
frequency_feature_option <- "Yes"  # Options: "Yes" or "No"

# Option to take absolute value of the data
abs_value_option <- "No"  # Options: "Yes" or "No"

# Option to include participant anthropometrics data
participant_anthroprometrics_option <- "Yes"  # Options: "Yes" or "No"
participant_anthroprometrics_file <- "Data Collection_Participant Anthro.csv"

# LOSOCV Result files
file_benchmark_result <- paste0("results/LOSOCV_", accelerometer_processing_option,"_", frequency_feature_option, "_", abs_value_option, "_",participant_anthroprometrics_option,"_", outcome, "_", interval, "s.Rds")
file_benchmark_plot <- paste0("results/LOSOCV_", accelerometer_processing_option,"_", frequency_feature_option, "_", abs_value_option, "_",participant_anthroprometrics_option,"_", outcome, "_", interval,"s.pdf")

# Holdout Result files
file_holdout_benchmark_result <- paste0("results/holdout/benchmark_results_", accelerometer_processing_option,"_", frequency_feature_option, "_", abs_value_option, "_",participant_anthroprometrics_option,"_", outcome, "_", interval, "s.Rds")
file_holdout_benchmark_plot <- paste0("results/holdout/benchmark_results_", accelerometer_processing_option,"_", frequency_feature_option, "_", abs_value_option, "_",participant_anthroprometrics_option,"_", outcome, "_", interval,"s.pdf")

#Tuning Result files
file_tuning_result <- paste0("results tuning/tuning_results_", accelerometer_processing_option,"_", frequency_feature_option, "_", abs_value_option, "_",participant_anthroprometrics_option,"_", outcome, "_", interval, "s.Rds")

# Specify the folder paths for accelerometer data and energy expenditure data
accelerometer_data_folder <- "actigraph data_"

# Specify the folder path for saving the processed data
processed_data_folder <- "processed data"

#For tuning data files
actigraph<- paste0("data/actigraph_right_", interval, "s.csv") 
opal_left<- paste0("data/opal_left_", interval, "s.csv") 
opal_right<- paste0("data/opal_right_", interval, "s.csv") 



