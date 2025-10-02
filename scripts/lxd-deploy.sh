#!/bin/bash
# LXD Ubuntu Server Deployment Script
# This script deploys the R Shiny application to an LXD Ubuntu server

echo "Deploying SMN Escapement Estimates Toolkit to LXD Ubuntu server..."

# Check if running in LXD container
if [ -f /.dockerenv ] || [ -f /run/.containerenv ]; then
    echo "✅ Running in container environment"
else
    echo "⚠️ Not running in container - this script is designed for LXD containers"
fi

# Update package lists
echo "Updating package lists..."
apt-get update

# Install R and required system packages
echo "Installing R and system dependencies..."
apt-get install -y r-base r-base-dev pandoc

# Install Python and pip (for document conversion)
echo "Installing Python and pip..."
apt-get install -y python3 python3-pip

# Install required R packages
echo "Installing required R packages..."
R --slave -e "
packages <- c('shiny', 'shinydashboard', 'DT', 'plotly', 'dplyr', 'tidyr', 'ggplot2', 'markdown', 'shinyjs', 'yaml')
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = 'https://cran.r-project.org')
  }
}
"

# Create necessary directories
echo "Creating directories..."
mkdir -p output logs tests

# Set permissions
echo "Setting permissions..."
chmod +x scripts/*.sh

# Create systemd service file for auto-start
echo "Creating systemd service..."
cat > /etc/systemd/system/smn-escapement-toolkit.service << EOF
[Unit]
Description=SMN Escapement Estimates Toolkit
After=network.target

[Service]
Type=simple
User=root
WorkingDirectory=/root/smn-escapement-estimates-toolkit
ExecStart=/usr/bin/R -e "shiny::runApp('app.R', port=3838, host='0.0.0.0')"
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
EOF

# Enable and start the service
echo "Enabling and starting service..."
systemctl daemon-reload
systemctl enable smn-escapement-toolkit
systemctl start smn-escapement-toolkit

# Test the application
echo "Testing application..."
R --slave -e "source('test_structured_key.R')"

echo "✅ Deployment complete!"
echo "Application is running on port 3838"
echo "Access via: http://your-server-ip:3838"
echo ""
echo "Service management:"
echo "  systemctl status smn-escapement-toolkit"
echo "  systemctl restart smn-escapement-toolkit"
echo "  systemctl stop smn-escapement-toolkit"
