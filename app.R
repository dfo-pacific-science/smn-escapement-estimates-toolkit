# R Shiny App for SMN Escapement Estimates Classification Toolkit
# Interactive Classification Key Application

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(markdown)
library(shinyjs)

# Source utility functions
source("R/utils.R")
source("R/classification_key.R")

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "SMN Escapement Estimates Classification Toolkit"),
  
          dashboardSidebar(
            sidebarMenu(
              menuItem("Classification Key", tabName = "classifier", icon = icon("key")),
              menuItem("Estimate Types", tabName = "types", icon = icon("table")),
              menuItem("Enumeration Methods", tabName = "enumeration", icon = icon("list")),
              menuItem("Estimation Methods", tabName = "estimation", icon = icon("calculator")),
              menuItem("Downgrade Criteria", tabName = "downgrade", icon = icon("exclamation-triangle")),
              menuItem("Documentation", tabName = "docs", icon = icon("book")),
              menuItem("Development", tabName = "development", icon = icon("code"))
            )
          ),
  
  dashboardBody(
    useShinyjs(),
    tabItems(
              # Classification Key tab
              tabItem(tabName = "classifier",
                fluidRow(
                  box(title = "Interactive Classification Key", status = "primary", solidHeader = TRUE,
                      width = 12,
                      p("Use this interactive key to determine the appropriate estimate type classification for your salmon escapement estimate."),
                      p("Start by selecting your enumeration method, then answer the questions that follow based on your specific situation.")
                  )
                ),
                fluidRow(
                  box(title = "Classification Process", status = "info", solidHeader = TRUE,
                      width = 8,
                      uiOutput("classificationQuestions")
                  ),
                  box(title = "Recommended Classification", status = "success", solidHeader = TRUE,
                      width = 4,
                      uiOutput("classificationResult"),
                      br(),
                      actionButton("reset_classification", "Start Over", 
                                   class = "btn-warning btn-sm")
                  )
                )
              ),
      
      # Estimate Types tab
      tabItem(tabName = "types",
        fluidRow(
          box(title = "Estimate Type Matrix", status = "success", solidHeader = TRUE,
              width = 12,
              DTOutput("typesTable")
          )
        )
      ),
      
      # Enumeration Methods tab
      tabItem(tabName = "enumeration",
        fluidRow(
          box(title = "Enumeration Methods", status = "info", solidHeader = TRUE,
              width = 12,
              DTOutput("enumerationTable")
          )
        )
      ),
      
      # Estimation Methods tab
      tabItem(tabName = "estimation",
        fluidRow(
          box(title = "Estimation Methods", status = "warning", solidHeader = TRUE,
              width = 12,
              DTOutput("estimationTable")
          )
        )
      ),
      
              # Documentation tab
              tabItem(tabName = "docs",
                fluidRow(
                  box(title = "Updated Estimate Type Guidance", status = "info", solidHeader = TRUE,
                      width = 12,
                      h3("Download the Complete Guidance Document"),
                      p("The complete Updated Escapement Estimate Classification Guidance document is available for download below."),
                      p("This document contains the full classification key, enumeration methods, estimation methods, and downgrade criteria."),
                      br(),
                      downloadButton("download_guidance", "Download Word Document", 
                                     class = "btn-primary btn-lg"),
                      br(), br(),
                      p("Document includes:"),
                      tags$ul(
                        tags$li("Complete dichotomous classification key (Section 4)"),
                        tags$li("Enumeration method definitions and default types"),
                        tags$li("Estimation method descriptions"),
                        tags$li("Type downgrade criteria and definitions"),
                        tags$li("Accuracy and precision specifications")
                      )
                  )
                )
              ),

              # Downgrade Criteria tab
              tabItem(tabName = "downgrade",
                fluidRow(
                  box(title = "Type Downgrade Criteria", status = "warning", solidHeader = TRUE,
                      width = 12,
                      p("These criteria are used to downgrade estimate types when specific conditions are not met."),
                      DTOutput("downgradeTable")
                  )
                )
              ),

              # Development tab
              tabItem(tabName = "development",
                fluidRow(
                  box(title = "Iterative Development Tools", status = "primary", solidHeader = TRUE,
                      width = 12,
                      p("Tools for improving the classification key based on user feedback and testing.")
                  )
                ),
                fluidRow(
                  box(title = "Feedback Collection", status = "info", solidHeader = TRUE,
                      width = 6,
                      textAreaInput("feedback", "User Feedback:", 
                                   placeholder = "Enter feedback about the classification process...",
                                   rows = 4),
                      textInput("user_email", "Email (optional):", 
                               placeholder = "your.email@example.com"),
                      actionButton("submit_feedback", "Submit Feedback", 
                                   class = "btn-primary")
                  ),
                  box(title = "Key Testing", status = "success", solidHeader = TRUE,
                      width = 6,
                      p("Test the classification key with different scenarios:"),
                      textInput("test_scenario", "Test Scenario Name:", 
                               placeholder = "e.g., 'Fence with bypass issues'"),
                      actionButton("run_test", "Run Test Scenario", 
                                   class = "btn-success"),
                      br(), br(),
                      verbatimTextOutput("test_results")
                  )
                ),
                fluidRow(
                  box(title = "Key Statistics", status = "info", solidHeader = TRUE,
                      width = 12,
                      fluidRow(
                        column(6,
                          h4("Classification Key Stats"),
                          verbatimTextOutput("key_stats")
                        ),
                        column(6,
                          h4("Recent Feedback"),
                          verbatimTextOutput("recent_feedback")
                        )
                      )
                    )
                )
              )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Load classification guidance
  guidance <- reactive({
    source("R/structured_classification_key.R", local = TRUE)
    load_structured_classification_key()
  })
  
  # Classification questions UI
  # Reactive values for classification state
  classification_state <- reactiveValues(
    phase = "enumeration",  # enumeration, estimation, documentation, complete
    current_step = 1,
    answers = list(),
    method_family = "",
    estimation_method = "",
    max_type = 1,
    provisional_type = NULL,
    candidate_type = NULL,
    final_type = NULL,
    downgrade_flags = character(0),
    has_documentation = NULL,
    precision_accuracy_adequate = NULL
  )
  
  output$classificationQuestions <- renderUI({
    guidance <- guidance()
    key_steps <- guidance$dichotomous_key
    
    if (nrow(key_steps) == 0) {
      return(p("Classification key not available. Please check the guidance document."))
    }
    
    # PHASE 1: ENUMERATION METHOD SELECTION AND QUESTIONS
    if (classification_state$phase == "enumeration") {
      # If no method family selected, show method selection
      if (classification_state$method_family == "") {
        return(
          div(
            h4("Phase 1: Enumeration Method"),
            p(strong("Select your enumeration method to begin classification:")),
            radioButtons(
              inputId = "method_selection",
              label = "Enumeration Method:",
              choices = list(
                "A. Census by Manual Count at Fixed Site" = "A",
                "B. Census by Semi-automated Electronic Count at Fixed Station" = "B", 
                "C. Modelled Count by Hydroacoustic Station Sonar" = "C",
                "D. Visual Count by Stream or Bank Walk" = "D",
                "E. Visual Snorkel Count" = "E",
                "F. Aerial Survey Count" = "F",
                "G. Redd Survey" = "G",
                "H. Modelled Count Trap Non-Spanning" = "H",
                "I. Electrofishing" = "I",
                "J. Mark-Recapture Modelled Count" = "J"
              ),
              selected = character(0)
            )
          )
        )
      }
      
      # Get questions for the selected method family
      method_questions <- key_steps[key_steps$method_family == classification_state$method_family, ]
      
      if (nrow(method_questions) == 0) {
        return(p("No questions available for the selected method."))
      }
      
      # Check if we've reached the end of enumeration questions
      if (classification_state$current_step > nrow(method_questions)) {
        # Move to estimation phase
        classification_state$phase <- "estimation"
        classification_state$current_step <- 1
        return(NULL)  # Will re-render with estimation phase
      }
      
      # Show current question
      current_question <- method_questions[classification_state$current_step, ]
      
      div(
        h4("Phase 1: Enumeration Method Questions"),
        p(strong(paste("Question", classification_state$current_step, "of", nrow(method_questions)))),
        p(current_question$question),
        radioButtons(
          inputId = "current_answer",
          label = "Your Answer:",
          choices = list("Yes" = "yes", "No" = "no"),
          selected = character(0)
        ),
        br(),
        div(
          if (classification_state$current_step > 1) {
            actionButton("previous_question", "Previous Question", 
                         class = "btn-secondary", style = "margin-right: 10px;")
          },
          actionButton("next_question", "Next Question", 
                       class = "btn-primary", disabled = TRUE)
        )
      )
    }
    
    # PHASE 2: ESTIMATION METHOD MAPPING (Step 50)
    else if (classification_state$phase == "estimation") {
      if (classification_state$estimation_method == "") {
        return(
          div(
            h4("Phase 2: Estimation Method Mapping (Step 50)"),
            p(strong("Select the estimation method used for your escapement estimate:")),
            p("The estimation method will determine the candidate type based on the provisional type from Phase 1."),
            p(paste("Current provisional type from enumeration:", 
                    ifelse(is.null(classification_state$provisional_type), 
                           paste0("Type-", classification_state$max_type), 
                           paste0("Type-", classification_state$provisional_type)))),
            p(paste("Maximum allowed type:", paste0("Type-", classification_state$max_type))),
            radioButtons(
              inputId = "estimation_method_selection",
              label = "Estimation Method:",
              choices = list(
                "Fixed-Station Tally" = "fixed_station",
                "Hydroacoustic Modelling" = "hydroacoustic",
                "Area Under the Curve" = "auc",
                "Peak Count Analysis" = "peak_count",
                "Mark-Recapture Analysis" = "mark_recapture",
                "Calibrated Time Series" = "time_series",
                "Expansion/Mathematical Operations" = "expansion"
              ),
              selected = character(0)
            ),
            br(),
            div(
              actionButton("back_to_enumeration", "Back to Questions", 
                           class = "btn-secondary", style = "margin-right: 10px;"),
              actionButton("apply_estimation_method", "Apply Estimation Method", 
                           class = "btn-primary")
            )
          )
        )
      } else {
        # Show result of estimation method mapping
        return(
          div(
            h4("Phase 2: Estimation Method Applied"),
            p(paste("Estimation Method:", classification_state$estimation_method)),
            p(paste("Candidate Type:", paste0("Type-", classification_state$candidate_type))),
            p("Click 'Next' to proceed to the final documentation and accuracy check."),
            br(),
            div(
              actionButton("back_to_estimation_selection", "Back to Estimation Method", 
                           class = "btn-secondary", style = "margin-right: 10px;"),
              actionButton("proceed_to_documentation", "Next: Documentation Check", 
                           class = "btn-primary")
            )
          )
        )
      }
    }
    
    # PHASE 3: DOCUMENTATION AND ACCURACY GATE (Step 53)
    else if (classification_state$phase == "documentation") {
      if (is.null(classification_state$has_documentation)) {
        return(
          div(
            h4("Phase 3: Final Documentation and Accuracy Gate (Step 53)"),
            p(strong("Documentation Check:")),
            p("For Type-1 and Type-2 estimates, SIL/SEN logs and a QA report are required."),
            p(paste("Current candidate type:", paste0("Type-", classification_state$candidate_type))),
            radioButtons(
              inputId = "has_documentation_input",
              label = "Are SIL/SEN logs and a QA report present?",
              choices = list("Yes" = "yes", "No" = "no"),
              selected = character(0)
            ),
            br(),
            div(
              actionButton("back_to_estimation_result", "Back to Estimation", 
                           class = "btn-secondary", style = "margin-right: 10px;"),
              actionButton("submit_documentation", "Next", 
                           class = "btn-primary", disabled = TRUE)
            )
          )
        )
      } else if (is.null(classification_state$precision_accuracy_adequate)) {
        return(
          div(
            h4("Phase 3: Precision and Accuracy Check"),
            p(strong("Precision/Accuracy Alignment:")),
            p("Does the precision and accuracy of your estimate align with the candidate type?"),
            p(paste("Current candidate type:", paste0("Type-", classification_state$candidate_type))),
            p("If weaker than expected for this type, the final type will be downgraded one level."),
            radioButtons(
              inputId = "precision_accuracy_input",
              label = "Do Precision/Accuracy align with candidate type?",
              choices = list("Yes" = "yes", "No" = "no"),
              selected = character(0)
            ),
            br(),
            div(
              actionButton("back_to_documentation", "Back to Documentation", 
                           class = "btn-secondary", style = "margin-right: 10px;"),
              actionButton("submit_precision_accuracy", "Calculate Final Type", 
                           class = "btn-success", disabled = TRUE)
            )
          )
        )
      } else {
        # Move to complete phase
        classification_state$phase <- "complete"
        return(NULL)  # Will re-render with complete phase
      }
    }
    
    # PHASE 4: COMPLETE
    else if (classification_state$phase == "complete") {
      return(
        div(
          h4("Classification Complete!"),
          p(strong("You have completed all phases of the classification process.")),
          p("Check the result panel on the right for your final recommended estimate type."),
          br(),
          p(em("To classify another estimate, click 'Start Over'"))
        )
      )
    }
  })
  
  # Handle method selection
  observeEvent(input$method_selection, {
    classification_state$method_family <- input$method_selection
    classification_state$current_step <- 1
    classification_state$answers <- list()
    classification_state$max_type <- 1
    classification_state$provisional_type <- NULL
    classification_state$downgrade_flags <- character(0)
  })
  
  # Handle answer selection
  observeEvent(input$current_answer, {
    if (!is.null(input$current_answer) && input$current_answer != "") {
      shinyjs::enable("next_question")
    }
  })
  
  # Handle previous question
  observeEvent(input$previous_question, {
    if (classification_state$current_step > 1) {
      # Remove the last answer
      guidance <- guidance()
      key_steps <- guidance$dichotomous_key
      method_questions <- key_steps[key_steps$method_family == classification_state$method_family, ]
      current_question <- method_questions[classification_state$current_step, ]
      
      # Remove the answer for the current step
      answer_key <- paste0("step_", current_question$step)
      if (answer_key %in% names(classification_state$answers)) {
        classification_state$answers[[answer_key]] <- NULL
      }
      
      # Move back one step
      classification_state$current_step <- classification_state$current_step - 1
      
      # Reset current answer
      updateRadioButtons(session, "current_answer", selected = character(0))
      shinyjs::disable("next_question")
    }
  })
  
  # Handle next question
  observeEvent(input$next_question, {
    if (!is.null(input$current_answer) && input$current_answer != "") {
      # Store the answer
      guidance <- guidance()
      key_steps <- guidance$dichotomous_key
      method_questions <- key_steps[key_steps$method_family == classification_state$method_family, ]
      current_question <- method_questions[classification_state$current_step, ]
      
      classification_state$answers[[paste0("step_", current_question$step)]] <- input$current_answer
      
      # Check for downgrades and type changes
      if (input$current_answer == "yes") {
        action <- current_question$yes_action
      } else {
        action <- current_question$no_action
      }
      
      # Debug output
      cat("DEBUG: Processing answer:", input$current_answer, "for step", current_question$step, "\n")
      cat("DEBUG: Action text:", action, "\n")
      
      # Check for type downgrades - improved pattern matching
      if (grepl("set max Type = ([0-9])", action, ignore.case = TRUE)) {
        max_type_match <- regmatches(action, regexec("set max Type = ([0-9])", action, ignore.case = TRUE))[[1]]
        if (length(max_type_match) >= 2) {
          new_max_type <- as.numeric(max_type_match[2])
          classification_state$max_type <- new_max_type
          # Debug output
          cat("DEBUG: Set max_type to", new_max_type, "from action:", action, "\n")
        }
      }
      
      # Check for downgrade flags - handle multiple flags separated by slashes
      downgrade_matches <- regmatches(action, gregexec("record ([A-Z_/]+)", action))[[1]]
      if (length(downgrade_matches) >= 2) {
        # Split by slash to handle multiple flags like "RUN_COVERAGE/UPTIME"
        flags <- strsplit(downgrade_matches[2], "/")[[1]]
        classification_state$downgrade_flags <- c(classification_state$downgrade_flags, flags)
        cat("DEBUG: Added downgrade flags:", paste(flags, collapse = ", "), "\n")
      }
      
      # Check for provisional type assignment
      if (grepl("provisional Type ([0-9])", action, ignore.case = TRUE)) {
        type_match <- regmatches(action, regexec("provisional Type ([0-9])", action, ignore.case = TRUE))[[1]]
        if (length(type_match) >= 2) {
          prov_type <- as.numeric(type_match[2])
          # Only set if within max_type constraint
          if (prov_type <= classification_state$max_type) {
            classification_state$provisional_type <- prov_type
          } else {
            classification_state$provisional_type <- classification_state$max_type
          }
        }
      }
      
      # Move to next question
      classification_state$current_step <- classification_state$current_step + 1
      
      # Reset current answer
      updateRadioButtons(session, "current_answer", selected = character(0))
      shinyjs::disable("next_question")
    }
  })
  
  # Handle estimation method application (Phase 2)
  observeEvent(input$apply_estimation_method, {
    if (!is.null(input$estimation_method_selection) && input$estimation_method_selection != "") {
      classification_state$estimation_method <- input$estimation_method_selection
      
      # Apply Step 50 logic - Estimation Method Mapping
      # Start with provisional type or max_type
      base_type <- ifelse(is.null(classification_state$provisional_type), 
                          classification_state$max_type, 
                          classification_state$provisional_type)
      
      # Apply estimation method rules
      candidate_type <- switch(input$estimation_method_selection,
        "fixed_station" = {
          # Fixed-Station Tally → Type 1 if Steps 2--7 or 9--15 satisfied; else Type 2
          # If enumeration was Method A or B and max_type allows Type 1
          if ((classification_state$method_family %in% c("A", "B")) && classification_state$max_type == 1) {
            1
          } else {
            min(2, classification_state$max_type)
          }
        },
        "hydroacoustic" = {
          # Hydroacoustic Modelling → Type 2
          min(2, classification_state$max_type)
        },
        "auc" = {
          # Area Under the Curve → Type 2 if thresholds met; else Peak/Index (Type 3)
          # Use provisional type if available
          if (!is.null(classification_state$provisional_type) && classification_state$provisional_type == 2) {
            2
          } else {
            min(3, classification_state$max_type)
          }
        },
        "peak_count" = {
          # Peak Count Analysis → Type 3; or Type 4 with ≤ 4 visits
          # Use provisional type if available
          if (!is.null(classification_state$provisional_type)) {
            min(classification_state$provisional_type, classification_state$max_type)
          } else {
            min(3, classification_state$max_type)
          }
        },
        "mark_recapture" = {
          # Mark--Recapture Analysis → Type 2 if Steps 48--49 satisfied
          if (classification_state$method_family == "J" && !is.null(classification_state$provisional_type) && classification_state$provisional_type == 2) {
            2
          } else {
            min(3, classification_state$max_type)
          }
        },
        "time_series" = {
          # Calibrated Time Series → Type 2 if calibrated to Type 1/2 with diagnostics; else Type 3
          # Would need additional questions - default to Type 3 for now
          min(3, classification_state$max_type)
        },
        "expansion" = {
          # Expansion/Mathematical Operations → inherit base; weak provenance = downgrade one Type
          # Use base type, possibly downgraded
          min(base_type + 1, classification_state$max_type, 4)  # Downgrade by 1
        },
        base_type  # Default
      )
      
      classification_state$candidate_type <- candidate_type
    }
  })
  
  # Handle back to enumeration phase
  observeEvent(input$back_to_enumeration, {
    classification_state$phase <- "enumeration"
  })
  
  # Handle back to estimation selection
  observeEvent(input$back_to_estimation_selection, {
    classification_state$estimation_method <- ""
    classification_state$candidate_type <- NULL
  })
  
  # Handle back to estimation result
  observeEvent(input$back_to_estimation_result, {
    classification_state$phase <- "estimation"
  })
  
  # Handle back to documentation
  observeEvent(input$back_to_documentation, {
    classification_state$has_documentation <- NULL
    updateRadioButtons(session, "has_documentation_input", selected = character(0))
    shinyjs::disable("submit_documentation")
  })
  
  # Handle proceed to documentation phase
  observeEvent(input$proceed_to_documentation, {
    classification_state$phase <- "documentation"
  })
  
  # Handle documentation input
  observeEvent(input$has_documentation_input, {
    if (!is.null(input$has_documentation_input) && input$has_documentation_input != "") {
      shinyjs::enable("submit_documentation")
    }
  })
  
  observeEvent(input$submit_documentation, {
    classification_state$has_documentation <- input$has_documentation_input
    updateRadioButtons(session, "has_documentation_input", selected = character(0))
    shinyjs::disable("submit_documentation")
  })
  
  # Handle precision/accuracy input
  observeEvent(input$precision_accuracy_input, {
    if (!is.null(input$precision_accuracy_input) && input$precision_accuracy_input != "") {
      shinyjs::enable("submit_precision_accuracy")
    }
  })
  
  observeEvent(input$submit_precision_accuracy, {
    classification_state$precision_accuracy_adequate <- input$precision_accuracy_input
    
    # Apply Step 53 logic - Final Documentation and Accuracy Gate
    final_type <- classification_state$candidate_type
    
    # Check precision/accuracy alignment
    if (classification_state$precision_accuracy_adequate == "no") {
      # Downgrade one Type if precision/accuracy is weaker
      final_type <- min(final_type + 1, 4)
      classification_state$downgrade_flags <- c(classification_state$downgrade_flags, "PRECISION_ACCURACY")
    }
    
    # Check documentation for Type 1/2
    if (final_type %in% c(1, 2)) {
      if (classification_state$has_documentation == "no") {
        # Downgrade one Type if documentation missing
        final_type <- min(final_type + 1, 4)
        classification_state$downgrade_flags <- c(classification_state$downgrade_flags, "DOC")
      }
    }
    
    classification_state$final_type <- final_type
    classification_state$phase <- "complete"
  })
  
  # Handle reset
  observeEvent(input$reset_classification, {
    classification_state$phase <- "enumeration"
    classification_state$method_family <- ""
    classification_state$estimation_method <- ""
    classification_state$current_step <- 1
    classification_state$answers <- list()
    classification_state$max_type <- 1
    classification_state$provisional_type <- NULL
    classification_state$candidate_type <- NULL
    classification_state$final_type <- NULL
    classification_state$downgrade_flags <- character(0)
    classification_state$has_documentation <- NULL
    classification_state$precision_accuracy_adequate <- NULL
    updateRadioButtons(session, "method_selection", selected = character(0))
    updateRadioButtons(session, "current_answer", selected = character(0))
    updateRadioButtons(session, "estimation_method_selection", selected = character(0))
    updateRadioButtons(session, "has_documentation_input", selected = character(0))
    updateRadioButtons(session, "precision_accuracy_input", selected = character(0))
    shinyjs::disable("next_question")
    shinyjs::disable("submit_documentation")
    shinyjs::disable("submit_precision_accuracy")
  })
  
  # Classification result
  output$classificationResult <- renderUI({
    if (classification_state$phase == "enumeration" && classification_state$method_family == "") {
      return(
        div(
          p(strong("Status:"), "Ready to begin"),
          p("Select an enumeration method in Phase 1 to start classification.")
        )
      )
    }
    
    if (classification_state$phase == "enumeration") {
      return(
        div(
          h5("Phase 1: In Progress"),
          p(strong("Enumeration Method:"), classification_state$method_family),
          p(strong("Current Max Type:"), paste0("Type-", classification_state$max_type)),
          if (!is.null(classification_state$provisional_type)) {
            p(strong("Provisional Type:"), paste0("Type-", classification_state$provisional_type))
          },
          if (length(classification_state$downgrade_flags) > 0) {
            div(
              p(strong("Downgrade Criteria Applied:")),
              tags$ul(
                lapply(classification_state$downgrade_flags, function(flag) {
                  # Try to get detailed information about the flag
                  flag_details <- tryCatch({
                    guidance_data <- guidance()
                    get_downgrade_details(flag, guidance_data$downgrade_criteria)
                  }, error = function(e) NULL)
                  
                  if (!is.null(flag_details)) {
                    tags$li(
                      strong(flag_details$name), " (", flag, "): ",
                      flag_details$description
                    )
                  } else {
                    tags$li(flag)
                  }
                })
              )
            )
          }
        )
      )
    }
    
    if (classification_state$phase == "estimation") {
      prov_type <- ifelse(is.null(classification_state$provisional_type), 
                          classification_state$max_type, 
                          classification_state$provisional_type)
      return(
        div(
          h5("Phase 2: Estimation Method"),
          p(strong("Enumeration Complete")),
          p(strong("Provisional Type:"), paste0("Type-", prov_type)),
          p(strong("Max Type:"), paste0("Type-", classification_state$max_type)),
          if (classification_state$estimation_method != "") {
            div(
              p(strong("Estimation Method:"), classification_state$estimation_method),
              p(strong("Candidate Type:"), paste0("Type-", classification_state$candidate_type), 
                style = "font-size: 16px; font-weight: bold; color: #007bff;")
            )
          }
        )
      )
    }
    
    if (classification_state$phase == "documentation") {
      return(
        div(
          h5("Phase 3: Documentation Check"),
          p(strong("Candidate Type:"), paste0("Type-", classification_state$candidate_type)),
          p("Checking documentation and precision/accuracy alignment...")
        )
      )
    }
    
    if (classification_state$phase == "complete" && !is.null(classification_state$final_type)) {
      return(
        div(
          h4("FINAL CLASSIFICATION:", style = "color: #28a745;"),
          div(
            p(paste0("Type-", classification_state$final_type), 
              style = "font-size: 24px; font-weight: bold; color: #28a745;")
          ),
          hr(),
          p(strong("Summary:")),
          p(paste("Enumeration Method:", classification_state$method_family)),
          p(paste("Estimation Method:", classification_state$estimation_method)),
          p(paste("Provisional Type:", 
                  ifelse(is.null(classification_state$provisional_type), 
                         paste0("Type-", classification_state$max_type), 
                         paste0("Type-", classification_state$provisional_type)))),
          p(paste("Candidate Type:", paste0("Type-", classification_state$candidate_type))),
          p(paste("Final Type:", paste0("Type-", classification_state$final_type))),
          if (length(classification_state$downgrade_flags) > 0) {
            div(
              hr(),
              p(strong("Downgrade Criteria Applied:"), style = "color: #dc3545;"),
              tags$ul(
                lapply(classification_state$downgrade_flags, function(flag) {
                  # Try to get detailed information about the flag
                  flag_details <- tryCatch({
                    guidance_data <- guidance()
                    get_downgrade_details(flag, guidance_data$downgrade_criteria)
                  }, error = function(e) NULL)
                  
                  if (!is.null(flag_details)) {
                    tags$li(
                      strong(flag_details$name), " (", flag, "): ",
                      flag_details$description, " - ",
                      em(flag_details$impact)
                    )
                  } else {
                    tags$li(flag)
                  }
                })
              )
            )
          }
        )
      )
    }
    
    # Default fallback
    p("Continue through the classification process...")
  })
  
  # Download handler for guidance document
  output$download_guidance <- downloadHandler(
    filename = function() {
      "Updated_Escapement_Estimate_Classification_Guidance.docx"
    },
    content = function(file) {
      # Copy the Word document to the download location
      doc_path <- "docs/Updated_Escapement_Estimate_Classification_Guidance.docx"
      if (file.exists(doc_path)) {
        file.copy(doc_path, file)
      } else {
        # Create a simple text file if the Word doc doesn't exist
        writeLines("Guidance document not found. Please check the docs/ directory.", file)
      }
    }
  )
  
  # Estimate types table
  output$typesTable <- renderDT({
    datatable(guidance()$estimate_type_matrix, 
              options = list(pageLength = 10, scrollX = TRUE),
              filter = "top")
  })
  
  # Enumeration methods table
  output$enumerationTable <- renderDT({
    datatable(guidance()$enumeration_methods, 
              options = list(pageLength = 10, scrollX = TRUE),
              filter = "top")
  })
  
  # Estimation methods table
  output$estimationTable <- renderDT({
    datatable(guidance()$estimation_methods, 
              options = list(pageLength = 10, scrollX = TRUE),
              filter = "top")
  })
  
  # Downgrade criteria table
  output$downgradeTable <- renderDT({
    datatable(guidance()$downgrade_criteria, 
              options = list(pageLength = 15, scrollX = TRUE),
              filter = "top")
  })
  
  # Handle feedback submission
  observeEvent(input$submit_feedback, {
    if (nchar(trimws(input$feedback)) > 0) {
      # Create feedback entry
      feedback_entry <- data.frame(
        timestamp = Sys.time(),
        feedback = input$feedback,
        email = ifelse(nchar(trimws(input$user_email)) > 0, input$user_email, "Anonymous"),
        stringsAsFactors = FALSE
      )
      
      # Save feedback to file
      feedback_file <- "output/feedback_log.csv"
      if (file.exists(feedback_file)) {
        write.table(feedback_entry, feedback_file, 
                   append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
      } else {
        write.csv(feedback_entry, feedback_file, row.names = FALSE)
      }
      
      # Clear inputs
      updateTextAreaInput(session, "feedback", value = "")
      updateTextInput(session, "user_email", value = "")
      
      # Show confirmation
      showNotification("Feedback submitted successfully!", type = "success")
    } else {
      showNotification("Please enter feedback before submitting.", type = "warning")
    }
  })
  
  # Handle test scenario execution
  observeEvent(input$run_test, {
    if (nchar(trimws(input$test_scenario)) > 0) {
      # Run a test scenario
      test_answers <- list(
        step_1 = "yes",  # Census method
        step_2 = "no",   # Has bypass issues
        step_3 = "yes"   # Good documentation
      )
      
      result <- run_classification_key(test_answers)
      
      # Save test result
      test_entry <- data.frame(
        timestamp = Sys.time(),
        scenario = input$test_scenario,
        result = result,
        stringsAsFactors = FALSE
      )
      
      test_file <- "output/test_log.csv"
      if (file.exists(test_file)) {
        write.table(test_entry, test_file, 
                   append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
      } else {
        write.csv(test_entry, test_file, row.names = FALSE)
      }
      
      # Update test results display
      output$test_results <- renderText({
        paste("Test Scenario:", input$test_scenario, "\n",
              "Result:", result, "\n",
              "Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      })
      
      showNotification("Test scenario completed!", type = "success")
    } else {
      showNotification("Please enter a test scenario name.", type = "warning")
    }
  })
  
  # Render key statistics
  output$key_stats <- renderText({
    guidance_data <- guidance()
    key_steps <- nrow(guidance_data$dichotomous_key)
    enum_methods <- nrow(guidance_data$enumeration_methods)
    est_methods <- nrow(guidance_data$estimation_methods)
    downgrade_cats <- nrow(guidance_data$downgrade_criteria)
    
    paste("Key Steps:", key_steps, "\n",
          "Enumeration Methods:", enum_methods, "\n",
          "Estimation Methods:", est_methods, "\n",
          "Downgrade Categories:", downgrade_cats, "\n",
          "Last Updated:", format(file.info("docs/Updated_Escapement_Estimate_Classification_Guidance.md")$mtime, "%Y-%m-%d %H:%M"))
  })
  
  # Render recent feedback
  output$recent_feedback <- renderText({
    feedback_file <- "output/feedback_log.csv"
    if (file.exists(feedback_file)) {
      feedback_data <- read.csv(feedback_file, stringsAsFactors = FALSE)
      if (nrow(feedback_data) > 0) {
        recent <- tail(feedback_data, 3)
        paste(apply(recent, 1, function(x) {
          paste("Time:", format(as.POSIXct(x[1]), "%m-%d %H:%M"),
                "Feedback:", substr(x[2], 1, 50), "...")
        }), collapse = "\n")
      } else {
        "No feedback submitted yet."
      }
    } else {
      "No feedback submitted yet."
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
