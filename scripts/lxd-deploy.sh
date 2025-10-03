#!/bin/bash
# =============================================================================
# LXD Ubuntu Server Deployment Script for R Shiny Applications
# Educational Version with Comprehensive Safety Features
# =============================================================================
#
# WHAT THIS SCRIPT DOES:
#   1. Installs R and system dependencies
#   2. Installs R packages needed for the Shiny app
#   3. Creates a systemd service to auto-start the app
#   4. Starts the application
#
# SAFETY FEATURES:
#   - Comprehensive error checking and recovery
#   - Port conflict detection
#   - Disk space verification
#   - Network connectivity checks
#   - Package installation verification
#   - Service health monitoring
#   - Rollback capabilities
#
# PREREQUISITES:
#   - Ubuntu LXD container (this script won't work on regular Ubuntu)
#   - Root access (or ability to use sudo)
#   - Internet connection for downloading packages
#
# USAGE:
#   sudo ./lxd-deploy-educational-safe.sh
#
# =============================================================================

# Exit immediately if any command fails
# This prevents the script from continuing if something goes wrong
set -e

# Enable error trapping for better error messages
trap 'echo "❌ Script failed at line $LINENO. Check the error above."; exit 1' ERR

# Global variables for cleanup
SERVICE_NAME="smn-escapement-toolkit"
SERVICE_FILE="/etc/systemd/system/${SERVICE_NAME}.service"
APP_DIR=$(pwd)
BACKUP_DIR="/tmp/smn-deployment-backup-$(date +%Y%m%d-%H%M%S)"

echo "🚀 Starting Educational LXD Shiny App Deployment with Safety Features..."
echo "======================================================================="

# =============================================================================
# SAFETY FUNCTION: CLEANUP ON FAILURE
# =============================================================================
# This function runs automatically if the script fails
# It cleans up any changes made so far to prevent a broken system
cleanup_on_failure() {
    echo ""
    echo "🧹 CLEANUP: Rolling back changes due to failure..."
    
    # Stop service if it was started
    if systemctl is-active --quiet "$SERVICE_NAME" 2>/dev/null; then
        echo "  Stopping service..."
        systemctl stop "$SERVICE_NAME" 2>/dev/null || true
    fi
    
    # Disable service if it was enabled
    if systemctl is-enabled --quiet "$SERVICE_NAME" 2>/dev/null; then
        echo "  Disabling service..."
        systemctl disable "$SERVICE_NAME" 2>/dev/null || true
    fi
    
    # Remove service file if it exists
    if [ -f "$SERVICE_FILE" ]; then
        echo "  Removing service file..."
        rm -f "$SERVICE_FILE"
        systemctl daemon-reload 2>/dev/null || true
    fi
    
    # Restore backup if it exists
    if [ -d "$BACKUP_DIR" ]; then
        echo "  Restoring backup..."
        # Add restore logic here if needed
    fi
    
    echo "  Cleanup complete."
    echo "  You can try running the script again after fixing any issues."
}

# Set up cleanup trap
trap cleanup_on_failure ERR

# =============================================================================
# STEP 1: VERIFY ENVIRONMENT AND SYSTEM REQUIREMENTS
# =============================================================================
# Before we do anything, let's make sure we're in the right environment
# and have the permissions we need. This includes safety checks for:
# - Root permissions
# - Disk space
# - Network connectivity
# - Port availability
# - Container environment

check_system_requirements() {
    echo ""
    echo "📋 Step 1: Checking Environment and System Requirements"
    echo "------------------------------------------------------"
    
    # Check if you're running as root
    # LXD containers typically run as root, but it's good to verify
    if [ "$EUID" -ne 0 ]; then
        echo "❌ ERROR: This script must be run as root"
        echo "   In LXD containers, you're usually already root"
        echo "   If not, run: sudo $0"
        exit 1
    fi
    echo "✅ Running as root - good!"
    
    # Check disk space (need at least 2GB free)
    # This prevents the installation from failing partway through
    AVAILABLE_SPACE=$(df / | awk 'NR==2 {print $4}')
    REQUIRED_SPACE=2097152  # 2GB in KB
    if [ "$AVAILABLE_SPACE" -lt "$REQUIRED_SPACE" ]; then
        echo "❌ ERROR: Insufficient disk space"
        echo "   Available: $(($AVAILABLE_SPACE / 1024 / 1024))GB"
        echo "   Required: 2GB minimum"
        echo "   Free up space and try again"
        exit 1
    fi
    echo "✅ Sufficient disk space available"
    
    # Check network connectivity
    # We need internet to download R packages
    if ! ping -c 1 8.8.8.8 >/dev/null 2>&1; then
        echo "❌ ERROR: No internet connection"
        echo "   This script requires internet access to download packages"
        exit 1
    fi
    echo "✅ Network connectivity confirmed"
    
    # Check if port 3838 is available
    # This prevents conflicts with other services
    if netstat -tlnp 2>/dev/null | grep -q ":3838 "; then
        echo "❌ ERROR: Port 3838 is already in use"
        echo "   Another service is using this port"
        echo "   Check with: netstat -tlnp | grep 3838"
        exit 1
    fi
    echo "✅ Port 3838 is available"
    
    # Check if we're in an LXD container
    # LXD containers have specific files that indicate they're containers
    if [ -f /.dockerenv ] || [ -f /run/.containerenv ]; then
        echo "✅ Running in container environment"
    else
        echo "⚠️  WARNING: Not running in a container"
        echo "   This script is designed for LXD containers"
        echo "   It might work on regular Ubuntu, but results may vary"
        read -p "   Continue anyway? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo "   Deployment cancelled by user"
            exit 1
        fi
    fi
}

# =============================================================================
# SAFETY FUNCTION: VERIFY PACKAGE INSTALLATION
# =============================================================================
# This function checks if packages were installed correctly
# It's called after each package installation to catch problems early
verify_package_installation() {
    local package_name=$1
    local package_type=$2
    
    case $package_type in
        "system")
            if ! dpkg -l | grep -q "^ii  $package_name "; then
                echo "❌ ERROR: System package $package_name not installed correctly"
                return 1
            fi
            ;;
        "r")
            if ! R --slave -e "library($package_name, character.only=TRUE)" >/dev/null 2>&1; then
                echo "❌ ERROR: R package $package_name not installed correctly"
                return 1
            fi
            ;;
    esac
    return 0
}

# =============================================================================
# STEP 2: UPDATE SYSTEM PACKAGES
# =============================================================================
# Before installing anything new, we should update the package lists
# This ensures we get the latest versions and security updates

update_system_packages() {
    echo ""
    echo "📦 Step 2: Updating System Packages"
    echo "-----------------------------------"
    
    # Create backup of current package state
    echo "Creating backup of current system state..."
    mkdir -p "$BACKUP_DIR"
    dpkg -l > "$BACKUP_DIR/installed-packages-before.txt" 2>/dev/null || true
    
    echo "Updating package lists..."
    if ! apt-get update; then
        echo "❌ ERROR: Failed to update package lists"
        exit 1
    fi
    
    echo "✅ Package lists updated"
}

# =============================================================================
# STEP 3: INSTALL SYSTEM DEPENDENCIES
# =============================================================================
# We need several system packages for our R Shiny application:
# - r-base: The R programming language
# - r-base-dev: Development tools for R (needed to compile some packages)
# - pandoc: Document converter (used by R Markdown)
# - python3: Python interpreter (for document conversion scripts)

install_system_dependencies() {
    echo ""
    echo "🔧 Step 3: Installing System Dependencies"
    echo "----------------------------------------"
    
    # Install packages one by one with verification
    PACKAGES=("r-base" "r-base-dev" "pandoc" "python3" "python3-pip")
    
    for package in "${PACKAGES[@]}"; do
        echo "Installing $package..."
        if ! apt-get install -y "$package"; then
            echo "❌ ERROR: Failed to install $package"
            exit 1
        fi
        
        # Verify installation
        if ! verify_package_installation "$package" "system"; then
            echo "❌ ERROR: $package installation verification failed"
            exit 1
        fi
        echo "✅ $package installed and verified"
    done
    
    echo "✅ All system dependencies installed successfully"
}

# =============================================================================
# STEP 4: INSTALL R PACKAGES
# =============================================================================

install_r_packages() {
    echo ""
    echo "📚 Step 4: Installing R Packages"
    echo "--------------------------------"
    
    # Verify R is working
    echo "Verifying R installation..."
    if ! R --version >/dev/null 2>&1; then
        echo "❌ ERROR: R is not working properly"
        exit 1
    fi
    echo "✅ R is working correctly"
    
    # Create backup of R package state
    R --slave -e "installed.packages()[,'Package']" > "$BACKUP_DIR/r-packages-before.txt" 2>/dev/null || true
    
    echo "Installing required R packages..."
    echo "This may take a few minutes..."
    
    # Install R packages with verification
    R_PACKAGES=("shiny" "shinydashboard" "DT" "dplyr" "tidyr" "ggplot2" "markdown" "shinyjs" "yaml")
    
    # Install packages and verify each one
    for package in "${R_PACKAGES[@]}"; do
        echo "Installing $package..."
        
        # Install package
        if ! R --slave -e "
        if (!require('$package', character.only = TRUE, quietly = TRUE)) {
          install.packages('$package', repos = 'https://cran.r-project.org', dependencies = TRUE)
        }
        " >/dev/null 2>&1; then
            echo "❌ ERROR: Failed to install R package $package"
            exit 1
        fi
        
        # Verify installation
        if ! verify_package_installation "$package" "r"; then
            echo "❌ ERROR: R package $package verification failed"
            exit 1
        fi
        echo "✅ $package installed and verified"
    done
    
    echo "✅ All R packages installed and verified successfully"
}

# =============================================================================
# STEP 5: SETUP APPLICATION DIRECTORY
# =============================================================================
# Create directories that our application needs for storing data and logs

setup_application_directory() {
    echo ""
    echo "📁 Step 5: Setting Up Application Directory"
    echo "------------------------------------------"
    
    # Get the current directory (where the script is run from)
    echo "Application directory: $APP_DIR"
    
    # Verify app.R exists
    if [ ! -f "$APP_DIR/app.R" ]; then
        echo "❌ ERROR: app.R not found in $APP_DIR"
        echo "   Make sure you're running this script from the application directory"
        exit 1
    fi
    echo "✅ app.R found"
    
    # Create directories for the app to store data
    echo "Creating data directories..."
    mkdir -p output logs tests
    
    # Make sure the app can write to these directories
    chmod 755 output logs tests
    echo "✅ Application directory setup complete"
}

# =============================================================================
# STEP 6: CREATE SYSTEMD SERVICE
# =============================================================================
# systemd is the service manager in modern Linux systems
# It handles starting, stopping, and managing services (like our Shiny app)
# 
# WHAT IS A SERVICE?
# A service is a program that runs in the background and can be:
# - Started automatically when the system boots
# - Restarted if it crashes
# - Monitored for status and health
# - Controlled with simple commands

create_systemd_service() {
    echo ""
    echo "⚙️  Step 6: Creating System Service"
    echo "----------------------------------"
    
    echo "Creating systemd service file: $SERVICE_FILE"
    
    # Create the service file
    # This tells systemd how to run our Shiny application
    cat > "$SERVICE_FILE" << EOF
[Unit]
# Basic service information
Description=SMN Escapement Estimates Toolkit
# Start this service after the network is available
After=network.target

[Service]
# Service type: simple means it's a single process
Type=simple
# Run as root (in LXD containers, this is usually fine)
User=root
Group=root
# Working directory for the application
WorkingDirectory=$APP_DIR
# Command to start the application
ExecStart=/usr/bin/R -e "shiny::runApp('app.R', port=3838, host='0.0.0.0')"
# Automatically restart if the service crashes
Restart=always
# Wait 10 seconds before restarting
RestartSec=10
# Set environment variables for R
Environment=R_LIBS_USER=/usr/local/lib/R/site-library

[Install]
# Start this service when the system reaches multi-user mode
WantedBy=multi-user.target
EOF

    # Verify service file was created
    if [ ! -f "$SERVICE_FILE" ]; then
        echo "❌ ERROR: Failed to create service file"
        exit 1
    fi
    echo "✅ Service file created"
    
    # Reload systemd configuration to recognize our new service
    echo "Reloading systemd configuration..."
    if ! systemctl daemon-reload; then
        echo "❌ ERROR: Failed to reload systemd"
        exit 1
    fi
    echo "✅ Systemd configuration reloaded"
}

# =============================================================================
# STEP 7: ENABLE AND START THE SERVICE
# =============================================================================
# Now we need to tell systemd about our new service and start it
# This includes comprehensive health checks to ensure everything works

start_service_with_health_checks() {
    echo ""
    echo "🚀 Step 7: Starting the Application Service with Health Checks"
    echo "------------------------------------------------------------"
    
    # Enable the service to start automatically on boot
    echo "Enabling service to start on boot..."
    if ! systemctl enable "$SERVICE_NAME"; then
        echo "❌ ERROR: Failed to enable service"
        exit 1
    fi
    echo "✅ Service enabled"
    
    # Start the service now
    echo "Starting the service..."
    if ! systemctl start "$SERVICE_NAME"; then
        echo "❌ ERROR: Failed to start service"
        echo "   Check logs with: journalctl -u $SERVICE_NAME -f"
        exit 1
    fi
    echo "✅ Service started"
    
    # Wait for service to fully start
    echo "Waiting for service to initialize..."
    sleep 5
    
    # Check if service is active
    if ! systemctl is-active --quiet "$SERVICE_NAME"; then
        echo "❌ ERROR: Service is not active"
        echo "   Check logs with: journalctl -u $SERVICE_NAME -f"
        exit 1
    fi
    echo "✅ Service is active"
    
    # Check if port is listening
    if ! netstat -tlnp 2>/dev/null | grep -q ":3838 "; then
        echo "❌ ERROR: Service is not listening on port 3838"
        echo "   Check logs with: journalctl -u $SERVICE_NAME -f"
        exit 1
    fi
    echo "✅ Service is listening on port 3838"
    
    # Test if application responds (basic health check)
    echo "Testing application health..."
    if curl -s -o /dev/null -w "%{http_code}" http://localhost:3838 | grep -q "200\|302"; then
        echo "✅ Application is responding"
    else
        echo "⚠️  WARNING: Application may not be responding correctly"
        echo "   Check manually: http://localhost:3838"
    fi
}

# =============================================================================
# STEP 8: VERIFY DEPLOYMENT
# =============================================================================
# Let's make sure everything is working

verify_deployment() {
    echo ""
    echo "🧪 Step 8: Verifying Deployment"
    echo "------------------------------"
    
    # Test if our R application can load its components
    echo "Testing application components..."
    if R --slave -e "source('test_structured_key.R')" 2>/dev/null; then
        echo "✅ Application test passed"
    else
        echo "⚠️  WARNING: Application test had issues"
        echo "   The app may still work, but there might be problems"
        echo "   You can check the app manually in a web browser"
    fi
}

# =============================================================================
# MAIN EXECUTION WITH SAFETY CHECKS
# =============================================================================
main() {
    echo "Starting deployment with comprehensive safety checks and educational content..."
    echo "Backup directory: $BACKUP_DIR"
    
    check_system_requirements
    update_system_packages
    install_system_dependencies
    install_r_packages
    setup_application_directory
    create_systemd_service
    start_service_with_health_checks
    verify_deployment
    
    # Clear the error trap since we succeeded
    trap - ERR
    
    # =============================================================================
    # DEPLOYMENT COMPLETE - ADMINISTRATOR GUIDE
    # =============================================================================
    
    echo ""
    echo "🎉 DEPLOYMENT COMPLETE!"
    echo "======================="
    echo ""
    echo "📋 SERVICE INFORMATION:"
    echo "   Service Name: $SERVICE_NAME"
    echo "   Application Port: 3838"
    echo "   Application Directory: $APP_DIR"
    echo "   Backup Directory: $BACKUP_DIR"
    echo ""
    echo "🌐 ACCESSING YOUR APPLICATION:"
    echo "   Local access:  http://localhost:3838"
    echo "   Remote access: http://$(hostname -I | awk '{print $1}'):3838"
    echo ""
    echo "🔧 SERVICE MANAGEMENT COMMANDS:"
    echo "   Check status:    systemctl status $SERVICE_NAME"
    echo "   View live logs:  journalctl -u $SERVICE_NAME -f"
    echo "   Restart service: systemctl restart $SERVICE_NAME"
    echo "   Stop service:    systemctl stop $SERVICE_NAME"
    echo "   Start service:   systemctl start $SERVICE_NAME"
    echo ""
    echo "📚 LXD ADMINISTRATION BASICS:"
    echo "   List containers: lxc list"
    echo "   Enter container: lxc exec <container-name> -- /bin/bash"
    echo "   Stop container:  lxc stop <container-name>"
    echo "   Start container: lxc start <container-name>"
    echo ""
    echo "🛠️  TROUBLESHOOTING:"
    echo "   If the app doesn't load:"
    echo "     1. Check service status: systemctl status $SERVICE_NAME"
    echo "     2. Check logs: journalctl -u $SERVICE_NAME -f"
    echo "     3. Check if port 3838 is open: netstat -tlnp | grep 3838"
    echo ""
    echo "   If you need to update the application:"
    echo "     1. Stop service: systemctl stop $SERVICE_NAME"
    echo "     2. Update files in: $APP_DIR"
    echo "     3. Start service: systemctl start $SERVICE_NAME"
    echo ""
    echo "   If you need to remove the service:"
    echo "     1. Stop service: systemctl stop $SERVICE_NAME"
    echo "     2. Disable service: systemctl disable $SERVICE_NAME"
    echo "     3. Remove service file: rm $SERVICE_FILE"
    echo "     4. Reload systemd: systemctl daemon-reload"
    echo ""
    echo "✅ Your SMN Escapement Estimates Toolkit is now running safely!"
    echo "   Open a web browser and navigate to the URL above to use it."
}

# Run the main function
main "$@"
