# Install mlr3mbo, DiceKriging, and rgenoud if not already installed
if (!requireNamespace("mlr3mbo", quietly = TRUE)) {
  install.packages("mlr3mbo")
}
if (!requireNamespace("DiceKriging", quietly = TRUE)) {
  install.packages("DiceKriging")
}
if (!requireNamespace("rgenoud", quietly = TRUE)) {
  install.packages("rgenoud")
}

# Load required libraries
library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(mlr3mbo)
library(paradox)
library(future)
library(data.table)
library(parallelly)

source("init.R")
source("prepare_analysis_data_mlr3_tuning.R")

# Check if outcome is either "METs_est" or "kJ"
if (!(outcome %in% c("METs_est", "kJ"))) {
  stop("Unknown outcome. Only 'METs_est' and 'kJ' are allowed.")
}

# Define the learner
learner <- lrn("regr.nnet", size = 15, maxit = 100, trace = FALSE)

# Define the parameter space for tuning
param_set <- ParamSet$new(list(
  ParamDbl$new("decay", lower = 0, upper = 100)
))

# Setup parallel processing using multisession (compatible with RStudio)
availableCores <- availableCores()
print(paste("Available CPU cores:", availableCores))
future::plan("multisession", workers = min(6, availableCores)) # Set workers to  
# Define and run the tuning instance
results <- list()
for (i in seq_along(tasks)) {
  task <- tasks[[i]]
  resampling <- resamplings[[i]]
  cat("Tuning task:", task$id, "\n")
  
  instance <- TuningInstanceSingleCrit$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = msr("regr.rmse"),
    search_space = param_set,
    terminator = trm("evals", n_evals = 100)
  )
  
  tuner <- tnr("mbo")
  tuner$optimize(instance)
  
  results[[task$id]] <- instance$result
}

# Save the results
saveRDS(results,file_tuning_result)

