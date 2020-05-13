# Prepare the final dataset for use in the R project
# Requirements: you must have downloaded and preprocessed the data using the python program
# Written by: Jasper Ginn

library(logger)
library(magrittr)
library(reticulate)
np <- import("numpy")
log_info("Started preprocessing script ...")
# Select these channels
channels <- c("EOG_var_theta", "EEG_Pz_Oz_mean_gamma", "EEG_Fpz_Cz_min_beta", "EOG_median_alpha")

# Headers (for data frames)
stats <- c("median", "mean", "var", "min", "max")
waves <- c("delta", "theta", "alpha", "beta", "spindle", "gamma")
# All combinations
combined <- expand.grid(stats, waves) %>%
  dplyr::mutate(Var1 = as.character(Var1), 
                Var2 = as.character(Var2)) %>%
  apply(., 1, function(x) paste0(x[1], "_", x[2]))
 
# Make headers
head <- c("sleep_state", 
          paste0("EEG_Fpz_Cz_", combined),
          paste0("EEG_Pz_Oz_", combined),
          paste0("EOG_", combined))

# Logit transform
logit <- function(x) {
  log(x / (1-x))
}

#' Take command-line arguments
#' 
#' @return command-line arguments
argsget <- function() {
  # Get arguments. Ignore arguments with --<param> in front
  args <- commandArgs(trailingOnly = TRUE)
  # Length of args 
  assertthat::assert_that(length(args) >= 2, msg="Not enough arguments passed to the function. Please specify a source directory where the data is stored and a target directory to save the output of this program.")
  # Source of raw data
  raw_data <- args[1]
  # Location of new data
  store_data <- args[2]
  # If force
  if(length(args) == 3) {
    force_process <- TRUE
  } else {
    force_process <- FALSE
  }
  # Checks
  # Check if folder exists
  source_check <- file.exists(raw_data)
  output_check <- file.exists(store_data)
  # Assertions
  assertthat::assert_that(source_check, msg=paste0("Folder ","'",raw_data, "'", 
                          " does not exist. Do you need to download the data first?"))
  #assertthat::assert_that(output_check, msg=paste0("Folder ","'",store_data, "'", 
  #                        " does not exist. Does your output folder exist?"))
  assertthat::is.flag(force_process)
  # Return
  return(list("source"=raw_data, "target"=store_data, "force"=force_process))
}

#' Preprocess sleep data downloaded from Physionet
#' 
#' @param source source file (as output by the program in 1_data-collection). See README for 
#'                more information.
#' @param channels channels to extract from the source file. The channels I use have been 
#'                  hard-coded in the global environment. (see line #8). 
#' 
#' @return Data Frame containing the channels requested. Each variable has been processed as follows:
#'          1. Apply a logit transformation on the data.
#'          2. standardize the data by subtracting the mean and dividing by the standard deviation.
preprocess_sleep_file <- function(source, patient, channels) {
  # Get patient ID
  PID_raw <- stringr::str_split(source, "/")[[1]]
  PID <- PID_raw[length(PID_raw)] %>%
    stringr::str_split("-") %>%
    .[[1]] %>%
    .[1]
  # Remove from filepath
  fp_new <- stringr::str_replace(source, PID_raw[length(PID_raw)], "")
  # Swap processed for raw
  fp_raw <- stringr::str_replace(fp_new, "/processed", "/raw")
  # Read headers
  patient_headers <- edfReader::readEdfHeader(paste0(fp_raw, PID, "-PSG.edf"))
  # Get details
  patient_details <- patient_headers$patient
  # Gender / age
  details_split <- stringr::str_split(patient_details, " ")[[1]]
  details_split <- details_split[length(details_split)]
  gender <- stringr::str_split(details_split, "_")[[1]][1]
  age <- stringr::str_split(details_split, "_")[[1]][2] %>%
    stringr::str_replace("yr", "") %>%
    as.numeric()
  # Load data file
  SDD <- np$load(source) %>%
    as.data.frame()
  # Set column names
  colnames(SDD) <- head
  # If empty, then pass
  if(nrow(SDD) == 0) {
    log_info("Data set is empty. Skipping file ...")
    return(NULL)
  }
  # Remap sleep data
  # 1 --> Awake
  # 2 --> NREM
  # 3 --> REM
  remap <- c(1,2,2,2,3)
  SDD$sleep_state <- remap[SDD$sleep_state]
  # Remap to factor
  sleep_map <- c("Awake", "NREM", "REM")
  SDD$sleep_state <- sleep_map[SDD$sleep_state]
  # Apply logit transformation and group-mean center the data
  SDD[,2:ncol(SDD)] <- apply(SDD[,2:ncol(SDD)], 2, function(x) {
    tibble::tibble(
      "xnew" = x,
      "class" = SDD$sleep_state
    ) %>%
      #dplyr::group_by(class) %>%
      #dplyr::mutate(xnew = logit(xnew),
      #       xnew = (xnew - mean(xnew) / stats::sd(xnew))) %>%
      #dplyr::ungroup() %>%
      #dplyr::select(xnew) %>%
      dplyr::pull()
  })
  # Add patient and sleep states
  SDD <- SDD %>%#[, channels] %>%
    data.frame() %>%
    dplyr::mutate("patient" = patient,
                  "identifier" = PID,
                  "sleep_state" = SDD[,1],
                  "epoch" = 1:dplyr::n(),
                  "gender" = gender,
                  "age" = age)
  # Re-arrange columns
  #SDD <- SDD[,c("patient", "identifier", "gender", "age", "epoch", "sleep_state",
  #              "EOG_var_theta", "EEG_Pz_Oz_mean_gamma", 
  #              "EEG_Fpz_Cz_min_beta", "EOG_median_alpha")]
  # Return
  return(SDD)
}

#' Main function
#' 
#' @details this function looks for files that have been saved by the program in '1_data-collection', loads
#'           these files and then preprocesses each. The resulting files will be stored in a target folder
#'           that is specified by the user.
main <- function() {
  # Get args
  args <- argsget()
  # If target folder does not exist, make it
  if(!file.exists(args[["target"]])) {
    dir.create(args[["target"]])
  } else {
    # Check if final dataset already exists
    file_check <- file.exists(paste0(args[["target"]], "/", "EEG_data_final.rds"))
    # Check if file exists
    if(file_check & !(args[["force"]])) {
      logger_info("Sleep data is already downloaded and preprocessed. Quitting now. If you want to re-run the entire script, add 'force_preprocess' as an argument to this script.")
      return(NULL)
    }
  }
  # 1. Read preprocessed data file names
  fns <- list.files(args[["source"]])
  # 2. Want only preprocessed data (not TPM)
  prep <- file.path(args[["source"]], fns[stringr::str_ends(fns, "preprocessed.npy")])
  # To store final data
  out <- vector("list", length(prep))
  for(patient in seq_along(prep)) {
    # Emit
    log_info(paste0("Preprocessing data for file ", patient))
    # Preprocess
    patient_processed <- preprocess_sleep_file(prep[patient], patient, channels)
    # If null, then next
    if(is.null(patient_processed)) {
      next
    }
    # Allocate
    out[[patient]] <- patient_processed
  }
  # Emit
  log_info("Finished processing data of ", length(prep), " patients ...")
  # Bind together
  final_out <- do.call(rbind.data.frame, out) %>%
    # To factor
    dplyr::mutate(patient = forcats::as_factor(patient),
                  identifier = forcats::as_factor(identifier),
                  sleep_state = forcats::as_factor(sleep_state))
  # Save 
  trgtflder = ifelse(endsWith(args[["target"]], "/"), args[["target"]], 
                      paste0(args[["target"]], "/"))
  saveRDS(final_out, paste0(trgtflder, "EEG_data_final.rds"))
  log_info("Program finished ...")
}

# Call main
main()
