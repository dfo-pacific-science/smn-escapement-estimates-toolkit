# SMN Escapement Estimates Classification Toolkit

An interactive R Shiny application for classifying salmon escapement estimates using a comprehensive three-phase dichotomous key based on the Updated Escapement Estimate Classification Guidance.

## 🚀 Quick Start

### Prerequisites
- **R** (>= 4.0.0)
- **R Packages**: shiny, shinydashboard, DT, dplyr, tidyr, ggplot2, markdown, shinyjs, yaml

### Quick Setup (Windows)
```powershell
# Run the setup script
.\scripts\windows-setup.ps1

# Or for quick setup
.\scripts\setup-simple.ps1
```

### Manual Setup
```r
# Install required packages (if not already installed)
install.packages(c("shiny", "shinydashboard", "DT", "dplyr", "tidyr", "ggplot2", "markdown", "shinyjs", "yaml"))

# Run the app
shiny::runApp('app.R', port=3838, host='127.0.0.1')
```

### Access the Application
- **Local**: http://localhost:3838
- **Features**: Three-phase classification process, detailed downgrade criteria, downloadable guidance document

## ✨ Key Features

- **Three-Phase Classification Process**:
  1. **Enumeration Method Assessment** - Determines provisional type based on data collection method
  2. **Estimation Method Mapping** - Applies estimation method rules to determine candidate type  
  3. **Documentation & Accuracy Gate** - Final checks for documentation and precision/accuracy alignment

- **Interactive Navigation**: Back buttons throughout the process for easy correction
- **Detailed Downgrade Criteria**: Comprehensive explanations of quality flags and their impacts
- **Real-time Results**: Immediate classification recommendations with full traceability
- **User Feedback System**: Context-aware feedback collection at each question with persistent logging
- **Structured Data Format**: Easy-to-maintain YAML-based classification key
- **Document Generation**: R Markdown template for generating Word documents from structured data

## 📁 Project Structure

```
smn-escapement-estimates-toolkit/
├── app.R                    # Main Shiny application
├── test_structured_key.R    # Test script for structured key
├── DESCRIPTION              # R package configuration
├── renv.lock               # R package lock file
├── .gitignore              # Git ignore rules
├── AGENTS.md               # Development configuration
├── R/                      # R source code
│   ├── utils.R            # Utility functions
│   └── structured_classification_key.R  # Structured key parser
├── docs/                   # Documentation files
│   ├── Updated_Escapement_Estimate_Classification_Guidance.docx
│   ├── Updated_Escapement_Estimate_Classification_Guidance.md
│   ├── generate_guidance_document.Rmd  # R Markdown template
│   ├── CLASSIFICATION_LOGIC_REVIEW.md
│   ├── CLASSIFICATION_FIX_SUMMARY.md
│   ├── BACK_BUTTON_AND_MAX_TYPE_FIXES.md
│   └── STRUCTURED_FORMAT_IMPLEMENTATION.md
├── config/                 # Configuration files
│   └── env.example        # Environment variables template
├── scripts/                # Setup and utility scripts
│   ├── windows-setup.ps1  # Windows development setup
│   ├── setup-simple.ps1   # Quick Windows setup
│   ├── setup.sh           # Codex Cloud setup
│   ├── maintenance.sh     # Codex Cloud maintenance
│   ├── lxd-deploy.sh      # LXD deployment
│   └── convert-doc-to-md.py  # Document conversion script
├── matrix_key/            # Classification data (single source of truth)
│   └── structured_dichotomous_key.yaml
├── output/                # Output files (gitignored)
│   └── question_feedback_log.csv  # User feedback submissions
├── logs/                  # Log files (gitignored)
└── tests/                 # Test files (gitignored)
```

## 🛠️ Development

### Testing the Application
```r
# Test all components
source("test_app.R")

# Test just the structured key
source("test_structured_key.R")
```

### Updating Classification Logic
1. **Edit** `matrix_key/structured_dichotomous_key.yaml` (single source of truth)
2. **Test** with `test_structured_key.R`
3. **Restart** the Shiny app
4. **Regenerate** Word document if needed

### Generating Documentation
```r
# Generate Word document from structured YAML data
rmarkdown::render("docs/generate_guidance_document.Rmd")
```

## 📊 Classification Process

### Phase 1: Enumeration Method Assessment
- Select enumeration method (A-J)
- Answer method-specific questions
- System tracks provisional type, max type, and downgrade flags

### Phase 2: Estimation Method Mapping (Step 50)
- Select estimation method
- Apply estimation method rules to determine candidate type
- Consider provisional type and max type constraints

### Phase 3: Documentation & Accuracy Gate (Step 53)
- Check documentation requirements (SIL/SEN logs, QA reports)
- Verify precision/accuracy alignment
- Apply final downgrades if needed

## 💬 User Feedback System

The application includes a comprehensive feedback system that allows users to provide context-specific feedback at any point during the classification process:

### Features
- **Context-Aware Feedback**: Automatically captures the current question, phase, and classification state
- **User Information**: Collects user name and optional email for follow-up
- **Persistent Logging**: All feedback is saved to `output/question_feedback_log.csv` with detailed context
- **Admin Review**: Feedback Review tab provides easy access to all submitted feedback

### Feedback Data Captured
- Timestamp and user information
- Current classification context (phase, step, question)
- Method family and estimation method
- Current type classifications and downgrade flags
- User's specific feedback text

### Accessing Feedback
- Navigate to the "Feedback Review" tab in the application
- View all submitted feedback in a searchable, sortable table
- Filter by user, date, phase, or other criteria

## 🔧 Configuration

### Environment Variables
Copy `config/env.example` to `.env` and configure as needed:
```env
APP_PORT=3838
APP_HOST=127.0.0.1
DEBUG_MODE=FALSE
```

### Document Conversion
- Use `scripts/convert-doc-to-md.py` to convert Word documents to Markdown
- Classification guidance document in `docs/` directory
- Structured classification key in `matrix_key/structured_dichotomous_key.yaml`

## 🧪 Testing

```r
# Test structured key loading and parsing
source("test_structured_key.R")

# Test specific functions
source("R/structured_classification_key.R")
key_data <- load_structured_classification_key()
```

## 📚 Documentation

- **Implementation Summary**: `docs/STRUCTURED_FORMAT_IMPLEMENTATION.md`
- **Classification Logic Review**: `docs/CLASSIFICATION_LOGIC_REVIEW.md`
- **Fix Summary**: `docs/CLASSIFICATION_FIX_SUMMARY.md`
- **Back Button & Max Type Fixes**: `docs/BACK_BUTTON_AND_MAX_TYPE_FIXES.md`

## 🤝 Contributing

1. **Edit** `matrix_key/structured_dichotomous_key.yaml` for classification logic changes
2. **Test** with `test_structured_key.R`
3. **Update** documentation as needed
4. **Regenerate** Word document if changes affect guidance

## 📋 Requirements

- **R** (>= 4.0.0)
- **R Packages**: shiny, shinydashboard, DT, dplyr, tidyr, ggplot2, markdown, shinyjs, yaml
- **Python** (>= 3.8) for document conversion scripts (optional)
- **Pandoc** for generating Word documents from R Markdown (optional)

## 🎯 Scope

- **IN**: Interactive three-phase classification key, structured data format, document generation
- **OUT**: Database integration, advanced analytics, data processing (future phases)

## 📞 Support

- Check application logs in `logs/` directory
- Review `docs/` for detailed implementation information
- Test structured key with `test_structured_key.R`
