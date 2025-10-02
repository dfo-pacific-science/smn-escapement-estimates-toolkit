# Utility functions for the SMN Escapement Estimates Toolkit

#' Load and validate environment variables
#' @return list of environment variables
load_env_vars <- function() {
  env_vars <- list(
    # Database connection
    db_host = Sys.getenv("DB_HOST", "localhost"),
    db_port = as.numeric(Sys.getenv("DB_PORT", "5432")),
    db_name = Sys.getenv("DB_NAME", "escapement_db"),
    db_user = Sys.getenv("DB_USER", "postgres"),
    db_password = Sys.getenv("DB_PASSWORD", ""),
    
    # API keys
    openai_api_key = Sys.getenv("OPENAI_API_KEY", ""),
    
    # App configuration
    app_port = as.numeric(Sys.getenv("APP_PORT", "3838")),
    app_host = Sys.getenv("APP_HOST", "0.0.0.0"),
    debug_mode = as.logical(Sys.getenv("DEBUG_MODE", "FALSE")),
    
    # File paths
    data_path = Sys.getenv("DATA_PATH", "data/"),
    output_path = Sys.getenv("OUTPUT_PATH", "output/")
  )
  
  # Validate required variables
  required_vars <- c("db_password", "openai_api_key")
  missing_vars <- required_vars[sapply(env_vars[required_vars], function(x) x == "")]
  
  if (length(missing_vars) > 0) {
    warning("Missing required environment variables: ", paste(missing_vars, collapse = ", "))
  }
  
  return(env_vars)
}

#' Validate data file exists and is readable
#' @param file_path path to data file
#' @return logical indicating if file is valid
validate_data_file <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("Data file not found: ", file_path)
  }
  
  if (!file.access(file_path, 4) == 0) {
    stop("Data file not readable: ", file_path)
  }
  
  return(TRUE)
}

#' Clean and standardize column names
#' @param df data frame
#' @return data frame with cleaned column names
clean_column_names <- function(df) {
  names(df) <- gsub("^X\\.", "", names(df))  # Remove X. prefix
  names(df) <- gsub("\\.", "_", names(df))   # Replace dots with underscores
  names(df) <- tolower(names(df))            # Convert to lowercase
  return(df)
}

#' Log application events
#' @param message log message
#' @param level log level (INFO, WARN, ERROR)
log_event <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0("[", timestamp, "] ", level, ": ", message)
  
  if (level == "ERROR") {
    stop(log_entry)
  } else {
    cat(log_entry, "\n")
  }
}

#' Check if required packages are installed
#' @param packages vector of package names
#' @return logical indicating if all packages are available
check_packages <- function(packages) {
  missing_packages <- packages[!packages %in% installed.packages()[,"Package"]]
  
  if (length(missing_packages) > 0) {
    log_event(paste("Missing packages:", paste(missing_packages, collapse = ", ")), "WARN")
    return(FALSE)
  }
  
  return(TRUE)
}

#' Install missing packages
#' @param packages vector of package names
install_missing_packages <- function(packages) {
  missing_packages <- packages[!packages %in% installed.packages()[,"Package"]]
  
  if (length(missing_packages) > 0) {
    log_event(paste("Installing packages:", paste(missing_packages, collapse = ", ")), "INFO")
    install.packages(missing_packages, dependencies = TRUE)
  }
}
