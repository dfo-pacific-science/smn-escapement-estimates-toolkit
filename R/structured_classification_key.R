# Structured Classification Key Parser
# This file contains functions to parse the structured YAML format

library(yaml)

#' Load structured classification key from YAML
#' @return list containing parsed classification data
load_structured_classification_key <- function() {
  yaml_file <- "matrix_key/structured_dichotomous_key.yaml"
  
  if (!file.exists(yaml_file)) {
    stop("Structured classification key file not found: ", yaml_file)
  }
  
  # Load YAML data
  key_data <- yaml::read_yaml(yaml_file)
  
  # Convert to data frames for easier use in Shiny
  dichotomous_key <- convert_to_dataframe(key_data$enumeration_methods)
  estimation_methods <- convert_estimation_methods(key_data$estimation_methods)
  downgrade_criteria <- convert_downgrade_criteria(key_data$downgrade_criteria)
  final_checks <- key_data$final_checks
  
  return(list(
    dichotomous_key = dichotomous_key,
    estimation_methods = estimation_methods,
    downgrade_criteria = downgrade_criteria,
    final_checks = final_checks
  ))
}

#' Convert enumeration methods to data frame
#' @param enum_methods list of enumeration methods from YAML
#' @return data frame with classification steps
convert_to_dataframe <- function(enum_methods) {
  key_steps <- data.frame(
    step = integer(0),
    question = character(0),
    yes_action = character(0),
    no_action = character(0),
    method_family = character(0),
    stringsAsFactors = FALSE
  )
  
  for (method_code in names(enum_methods)) {
    method <- enum_methods[[method_code]]
    
    for (step_data in method$steps) {
      # Convert actions to strings for compatibility
      yes_action_str <- convert_action_to_string(step_data$yes_action)
      no_action_str <- convert_action_to_string(step_data$no_action)
      
      key_steps <- rbind(key_steps, data.frame(
        step = step_data$step,
        question = step_data$question,
        yes_action = yes_action_str,
        no_action = no_action_str,
        method_family = method_code,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Sort by step number
  key_steps <- key_steps[order(key_steps$step), ]
  
  return(key_steps)
}

#' Convert action object to string for compatibility
#' @param action list containing action details
#' @return string representation of action
convert_action_to_string <- function(action) {
  if (is.null(action)) {
    return("Continue to next step")
  }
  
  action_parts <- c()
  
  # Handle different action types
  if (action$type == "goto") {
    action_parts <- c(action_parts, paste("go to", action$target))
  }
  
  if (action$type == "set_max_type") {
    action_parts <- c(action_parts, paste("set max Type =", action$value))
  }
  
  if (action$type == "record_flags") {
    flags <- paste(action$flags, collapse = "/")
    action_parts <- c(action_parts, paste("record", flags))
  }
  
  if (action$type == "provisional_type") {
    if (!is.null(action$condition)) {
      action_parts <- c(action_parts, paste("provisional Type", action$value, "if", action$condition))
      if (!is.null(action$else)) {
        action_parts <- c(action_parts, paste("else Type", action$else))
      }
    } else {
      action_parts <- c(action_parts, paste("provisional Type", action$value))
    }
  }
  
  if (action$type == "treat_as") {
    action_parts <- c(action_parts, paste("treat as", action$value))
  }
  
  if (action$type == "note") {
    action_parts <- c(action_parts, action$value)
  }
  
  if (action$type == "keep_type") {
    action_parts <- c(action_parts, paste("keep", action$value))
  }
  
  if (action$type == "consider_type") {
    action_parts <- c(action_parts, paste("consider Type", action$value))
  }
  
  if (action$type == "retain_prior") {
    action_parts <- c(action_parts, paste("retain prior", action$value))
  }
  
  if (action$type == "retain_outcome") {
    action_parts <- c(action_parts, "retain outcome")
  }
  
  if (action$type == "keep_or_downgrade") {
    action_parts <- c(action_parts, paste("keep/downgrade to", action$value))
  }
  
  # Handle chained actions (then, then2)
  if (!is.null(action$then)) {
    if (action$then == "goto") {
      action_parts <- c(action_parts, paste("then go to", action$target))
    } else if (action$then == "set_max_type") {
      action_parts <- c(action_parts, paste("then set max Type =", action$value))
    } else if (action$then == "continue") {
      action_parts <- c(action_parts, "then continue")
    }
  }
  
  if (!is.null(action$then2)) {
    if (action$then2 == "goto") {
      action_parts <- c(action_parts, paste("then go to", action$target))
    }
  }
  
  # Handle conditional flags
  if (!is.null(action$conditional_flags)) {
    for (cond_flag in action$conditional_flags) {
      action_parts <- c(action_parts, paste(cond_flag$condition, "record", paste(cond_flag$flags, collapse = "/")))
    }
  }
  
  # Handle flags in no_action
  if (!is.null(action$flags)) {
    flags <- paste(action$flags, collapse = "/")
    action_parts <- c(action_parts, paste("record", flags))
  }
  
  # Handle notes
  if (!is.null(action$note)) {
    action_parts <- c(action_parts, action$note)
  }
  
  # Join all parts
  result <- paste(action_parts, collapse = "; ")
  
  # Special case for "Proceed to 50"
  if (grepl("Proceed to 50", result)) {
    result <- "Proceed to 50"
  }
  
  return(result)
}

#' Convert estimation methods to data frame
#' @param est_methods list of estimation methods from YAML
#' @return data frame with estimation method rules
convert_estimation_methods <- function(est_methods) {
  est_df <- data.frame(
    method_code = character(0),
    method_name = character(0),
    rules = character(0),
    stringsAsFactors = FALSE
  )
  
  for (method_code in names(est_methods)) {
    method <- est_methods[[method_code]]
    
    # Convert rules to string
    rules_text <- c()
    for (rule in method$rules) {
      rule_text <- paste("If", rule$condition, "→", rule$result)
      if (!is.null(rule$else)) {
        rule_text <- paste(rule_text, "else", rule$else)
      }
      rules_text <- c(rules_text, rule_text)
    }
    
    est_df <- rbind(est_df, data.frame(
      method_code = method_code,
      method_name = method$name,
      rules = paste(rules_text, collapse = "; "),
      stringsAsFactors = FALSE
    ))
  }
  
  return(est_df)
}

#' Convert downgrade criteria to data frame
#' @param downgrade_criteria list of downgrade criteria from YAML
#' @return data frame with downgrade criteria
convert_downgrade_criteria <- function(downgrade_criteria) {
  downgrade_df <- data.frame(
    flag_code = character(0),
    name = character(0),
    description = character(0),
    impact = character(0),
    stringsAsFactors = FALSE
  )
  
  for (flag_code in names(downgrade_criteria)) {
    criteria <- downgrade_criteria[[flag_code]]
    
    downgrade_df <- rbind(downgrade_df, data.frame(
      flag_code = flag_code,
      name = criteria$name,
      description = criteria$description,
      impact = criteria$impact,
      stringsAsFactors = FALSE
    ))
  }
  
  return(downgrade_df)
}

#' Get downgrade criteria details for a flag
#' @param flag_code character code for the downgrade flag
#' @param downgrade_criteria data frame of downgrade criteria
#' @return list with flag details or NULL if not found
get_downgrade_details <- function(flag_code, downgrade_criteria) {
  flag_row <- downgrade_criteria[downgrade_criteria$flag_code == flag_code, ]
  
  if (nrow(flag_row) == 0) {
    return(NULL)
  }
  
  return(list(
    name = flag_row$name,
    description = flag_row$description,
    impact = flag_row$impact
  ))
}

#' Test the structured key loading
test_structured_key <- function() {
  tryCatch({
    key_data <- load_structured_classification_key()
    
    cat("Successfully loaded structured classification key:\n")
    cat("- Dichotomous key steps:", nrow(key_data$dichotomous_key), "\n")
    cat("- Estimation methods:", nrow(key_data$estimation_methods), "\n")
    cat("- Downgrade criteria:", nrow(key_data$downgrade_criteria), "\n")
    
    # Test specific step
    method_a_steps <- key_data$dichotomous_key[key_data$dichotomous_key$method_family == "A", ]
    cat("\nMethod A steps:\n")
    print(method_a_steps[, c("step", "question", "no_action")])
    
    return(TRUE)
  }, error = function(e) {
    cat("Error loading structured key:", e$message, "\n")
    return(FALSE)
  })
}
