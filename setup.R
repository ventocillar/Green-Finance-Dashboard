# Setup Script for Green Finance Dashboard
# Install all required packages

cat("\n╔══════════════════════════════════════════════════╗\n")
cat("║  Green Finance Dashboard - Package Setup        ║\n")
cat("╚══════════════════════════════════════════════════╝\n\n")

# List of required packages
required_packages <- c(
  # Core data manipulation
  "tidyverse",
  "dplyr",
  "tidyr",
  "purrr",
  "readr",
  "stringr",

  # Visualization
  "ggplot2",
  "plotly",
  "leaflet",
  "viridis",
  "scales",
  "RColorBrewer",

  # Shiny and dashboard
  "shiny",
  "bslib",
  "shinycssloaders",
  "DT",

  # Spatial data (optional but recommended)
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",

  # Data acquisition
  "httr",
  "jsonlite",
  "rvest",
  "readxl",

  # Utilities
  "glue",
  "here",
  "lubridate",
  "countrycode"
)

# Function to install missing packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

  if(length(new_packages)) {
    cat("\n📦 Installing missing packages:\n")
    cat(paste("  -", new_packages, collapse = "\n"), "\n\n")

    install.packages(new_packages, dependencies = TRUE, repos = "https://cloud.r-project.org/")

    cat("\n✓ Package installation complete!\n")
  } else {
    cat("\n✓ All required packages are already installed!\n")
  }
}

# Install packages
install_if_missing(required_packages)

# Load packages to verify
cat("\n📋 Verifying package installation...\n")
failed <- c()

for (pkg in required_packages) {
  result <- suppressWarnings(require(pkg, character.only = TRUE, quietly = TRUE))
  if (result) {
    cat(sprintf("  ✓ %s\n", pkg))
  } else {
    cat(sprintf("  ✗ %s (FAILED)\n", pkg))
    failed <- c(failed, pkg)
  }
}

if (length(failed) > 0) {
  cat("\n⚠ Warning: The following packages failed to load:\n")
  cat(paste("  -", failed, collapse = "\n"), "\n")
  cat("\nPlease install them manually using: install.packages(c('", paste(failed, collapse = "', '"), "'))\n", sep = "")
} else {
  cat("\n✓ All packages loaded successfully!\n")
}

# Create directory structure
cat("\n📁 Creating directory structure...\n")

dirs <- c(
  "data",
  "data/raw",
  "data/processed",
  "data/cache",
  "app",
  "R",
  "reports"
)

for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat(sprintf("  ✓ Created: %s\n", dir))
  } else {
    cat(sprintf("  ℹ Exists: %s\n", dir))
  }
}

cat("\n╔══════════════════════════════════════════════════╗\n")
cat("║  ✓ Setup Complete!                              ║\n")
cat("╚══════════════════════════════════════════════════╝\n\n")

cat("Next steps:\n")
cat("  1. Run: source('R/data_acquisition.R')     # Fetch data\n")
cat("  2. Run: source('R/data_processing.R')      # Process data\n")
cat("  3. Run: source('R/visualizations.R')       # Generate visualizations\n")
cat("  4. Run: shiny::runApp('app')              # Launch dashboard\n\n")
