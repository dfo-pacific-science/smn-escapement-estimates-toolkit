# Structured Classification Key Parser
# This file contains functions to parse the structured YAML format

library(yaml)

load_structured_classification_key <- function() {
  yaml_file <- "matrix_key/structured_dichotomous_key.yaml"
  
  if (!file.exists(yaml_file)) {
    stop("Structured classification key file not found: ", yaml_file)
  }
  
  key_data <- yaml::read_yaml(yaml_file)
  
  dichotomous_key <- convert_to_dataframe(key_data$enumeration_methods)
  estimation_methods <- convert_estimation_methods(key_data$estimation_methods)
  downgrade_criteria <- convert_downgrade_criteria(key_data$downgrade_criteria)
  final_checks <- key_data$final_checks
  
  # Create enumeration methods table
  enumeration_methods <- convert_enumeration_methods(key_data$enumeration_methods)
  
  # Create estimate type matrix
  estimate_type_matrix <- create_estimate_type_matrix()
  
  return(list(
    dichotomous_key = dichotomous_key,
    estimation_methods = estimation_methods,
    downgrade_criteria = downgrade_criteria,
    final_checks = final_checks,
    enumeration_methods = enumeration_methods,
    estimate_type_matrix = estimate_type_matrix
  ))
}

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
  
  key_steps <- key_steps[order(key_steps$step), ]
  
  return(key_steps)
}

convert_action_to_string <- function(action) {
  if (is.null(action)) {
    return("Continue to next step")
  }
  
  # Handle new actions array structure
  if (!is.null(action$actions)) {
    action_parts <- c()
    for (sub_action in action$actions) {
      action_parts <- c(action_parts, convert_single_action(sub_action))
    }
    return(paste(action_parts, collapse = "; "))
  }
  
  # Handle single action
  return(convert_single_action(action))
}

convert_single_action <- function(action) {
  if (is.null(action)) {
    return("Continue to next step")
  }
  
  action_parts <- c()
  
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
      if (!is.null(action[["else"]])) {
        action_parts <- c(action_parts, paste("else Type", action[["else"]]))
      }
    } else {
      action_parts <- c(action_parts, paste("provisional Type", action$value))
    }
  }
  
  if (action$type == "note") {
    action_parts <- c(action_parts, action$value)
  }
  
  # Legacy support for deprecated actions
  if (action$type == "treat_as") {
    action_parts <- c(action_parts, paste("treat as", action$value))
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
  
  # Legacy support for then/then2
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
  
  if (!is.null(action$conditional_flags)) {
    for (cond_flag in action$conditional_flags) {
      action_parts <- c(action_parts, paste(cond_flag$condition, "record", paste(cond_flag$flags, collapse = "/")))
    }
  }
  
  if (!is.null(action$flags)) {
    flags <- paste(action$flags, collapse = "/")
    action_parts <- c(action_parts, paste("record", flags))
  }
  
  if (!is.null(action$note)) {
    action_parts <- c(action_parts, action$note)
  }
  
  result <- paste(action_parts, collapse = "; ")
  
  if (grepl("Proceed to 50", result)) {
    result <- "Proceed to 50"
  }
  
  return(result)
}

convert_estimation_methods <- function(est_methods) {
  est_df <- data.frame(
    method_code = character(0),
    method_name = character(0),
    rules = character(0),
    stringsAsFactors = FALSE
  )
  
  # Handle the new structure where methods are under est_methods$rules
  if (!is.null(est_methods$rules)) {
    methods_list <- est_methods$rules
  } else {
    methods_list <- est_methods
  }
  
  for (method_code in names(methods_list)) {
    method <- methods_list[[method_code]]
    
    rules_text <- c()
    for (rule in method$rules) {
      rule_text <- paste("If", rule$condition, "then", rule$result)
      if (!is.null(rule[["else"]])) {
        rule_text <- paste(rule_text, "else", rule[["else"]])
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

convert_enumeration_methods <- function(enum_methods) {
  enum_df <- data.frame(
    method_code = character(0),
    method_name = character(0),
    description = character(0),
    default_type = character(0),
    max_type = character(0),
    stringsAsFactors = FALSE
  )
  
  for (method_code in names(enum_methods)) {
    method <- enum_methods[[method_code]]
    
    # Determine default and max types based on method
    default_type <- switch(method_code,
      "A" = "Type 1 (if Fixed-Station Tally)",
      "B" = "Type 1 (if Fixed-Station Tally)", 
      "C" = "Type 2",
      "D" = "Type 2-3",
      "E" = "Type 2-3",
      "F" = "Type 3",
      "G" = "Type 2-3",
      "H" = "Type 2-3",
      "I" = "Type 3-4",
      "J" = "Type 2-3",
      "Unknown"
    )
    
    max_type <- switch(method_code,
      "A" = "Type 1",
      "B" = "Type 1",
      "C" = "Type 2", 
      "D" = "Type 2",
      "E" = "Type 2",
      "F" = "Type 3",
      "G" = "Type 2",
      "H" = "Type 2",
      "I" = "Type 3",
      "J" = "Type 2",
      "Type 5"
    )
    
    enum_df <- rbind(enum_df, data.frame(
      method_code = method_code,
      method_name = method$name,
      description = method$description,
      default_type = default_type,
      max_type = max_type,
      stringsAsFactors = FALSE
    ))
  }
  
  return(enum_df)
}

create_estimate_type_matrix <- function() {
  # Create a comprehensive estimate type matrix based on the classification system
  type_matrix <- data.frame(
    Type = c("Type 1", "Type 2", "Type 3", "Type 4", "Type 5", "Type 6"),
    Description = c(
      "Census - Complete count with negligible error",
      "Census - Complete count with minor error or Index - High precision",
      "Index - Moderate precision", 
      "Index - Low precision",
      "Relative - Very low precision",
      "Presence/Absence - Non-numeric"
    ),
    Accuracy = c("±5%", "±10%", "±25%", "±50%", "±100%", "Qualitative"),
    Precision = c("CV < 5%", "CV < 10%", "CV < 25%", "CV < 50%", "CV > 50%", "N/A"),
    Documentation = c(
      "SIL/SEN logs + QA report required",
      "SIL/SEN logs + QA report required", 
      "Methods documentation required",
      "Basic documentation required",
      "Minimal documentation",
      "Basic documentation"
    ),
    Typical_Methods = c(
      "Fixed-site manual/electronic counting",
      "Fixed-site counting, Hydroacoustic, AUC, Mark-recapture",
      "Visual surveys, Aerial surveys, Trap models",
      "Electrofishing, Limited surveys",
      "Unknown methods, Inconsistent data",
      "Presence/absence surveys"
    ),
    stringsAsFactors = FALSE
  )
  
  return(type_matrix)
}

test_structured_key <- function() {
  tryCatch({
    key_data <- load_structured_classification_key()
    
    cat("Successfully loaded structured classification key:\n")
    cat("- Dichotomous key steps:", nrow(key_data$dichotomous_key), "\n")
    cat("- Estimation methods:", nrow(key_data$estimation_methods), "\n")
    cat("- Downgrade criteria:", nrow(key_data$downgrade_criteria), "\n")
    
    method_a_steps <- key_data$dichotomous_key[key_data$dichotomous_key$method_family == "A", ]
    cat("\nMethod A steps:\n")
    print(method_a_steps[, c("step", "question", "no_action")])
    
    return(TRUE)
  }, error = function(e) {
    cat("Error loading structured key:", e$message, "\n")
    return(FALSE)
  })
}
