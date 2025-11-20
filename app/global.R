# Global Settings for Green Finance Dashboard
# This file loads required packages, data, and shared functions

# Load packages ----
library(shiny)
library(bslib)
library(plotly)
library(leaflet)
library(tidyverse)
library(DT)
library(shinycssloaders)
library(htmltools)

# Source R scripts ----
source(here::here("R/utils.R"), local = TRUE)

# Load processed data and visualizations ----
data_file <- here::here("data/processed/processed_climate_finance.rds")
viz_file <- here::here("data/processed/visualizations.rds")

if (!file.exists(data_file)) {
  stop("Data file not found. Please run R/data_acquisition.R and R/data_processing.R first.")
}

processed_data <- readRDS(data_file)

if (file.exists(viz_file)) {
  visualizations <- readRDS(viz_file)
} else {
  message("Visualizations file not found. Please run R/visualizations.R first.")
  visualizations <- NULL
}

# Dashboard theme ----
app_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#212529",
  primary = "#2E7D32",
  secondary = "#1976D2",
  success = "#2E7D32",
  info = "#0288D1",
  warning = "#F57C00",
  danger = "#C62828",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  font_scale = 0.95
)

# Helper functions for dashboard ----

# Create value box HTML
create_value_box_html <- function(value, title, icon, change = NULL, change_positive = TRUE) {
  change_html <- ""
  if (!is.null(change) && !is.na(change)) {
    change_color <- if (change_positive) {
      if (change > 0) "text-success" else "text-danger"
    } else {
      if (change > 0) "text-danger" else "text-success"
    }

    change_icon <- if (change > 0) "↑" else if (change < 0) "↓" else "→"

    change_html <- sprintf(
      '<div class="%s" style="font-size: 0.9rem; margin-top: 5px;">
        <span style="font-weight: 600;">%s %s%%</span>
      </div>',
      change_color,
      change_icon,
      abs(round(change, 1))
    )
  }

  HTML(sprintf(
    '<div class="card h-100 border-0 shadow-sm">
      <div class="card-body">
        <div class="d-flex justify-content-between align-items-start">
          <div>
            <h6 class="text-muted text-uppercase" style="font-size: 0.75rem; font-weight: 600; letter-spacing: 0.5px;">%s</h6>
            <h2 class="mb-0" style="font-weight: 700; color: #2E7D32;">%s</h2>
            %s
          </div>
          <div class="text-muted" style="font-size: 2rem; opacity: 0.4;">%s</div>
        </div>
      </div>
    </div>',
    title,
    value,
    change_html,
    icon
  ))
}

# Calculate dashboard KPIs ----
calculate_kpis <- function(data) {
  latest_year <- max(data$raw_finance$year)
  prev_year <- latest_year - 1

  # Current year metrics
  current_commitment <- sum(data$raw_finance$commitment_usd_billions[data$raw_finance$year == latest_year], na.rm = TRUE)
  prev_commitment <- sum(data$raw_finance$commitment_usd_billions[data$raw_finance$year == prev_year], na.rm = TRUE)

  current_disbursement <- sum(data$raw_finance$disbursement_usd_billions[data$raw_finance$year == latest_year], na.rm = TRUE)

  green_current <- sum(data$raw_finance$commitment_usd_billions[
    data$raw_finance$year == latest_year & data$raw_finance$investment_type == "Green"
  ], na.rm = TRUE)
  green_prev <- sum(data$raw_finance$commitment_usd_billions[
    data$raw_finance$year == prev_year & data$raw_finance$investment_type == "Green"
  ], na.rm = TRUE)

  disbursement_rate <- (current_disbursement / current_commitment) * 100

  list(
    total_commitment = format_billions(current_commitment),
    commitment_change = calculate_yoy_change(current_commitment, prev_commitment),
    green_investment = format_billions(green_current),
    green_change = calculate_yoy_change(green_current, green_prev),
    disbursement_rate = paste0(round(disbursement_rate, 1), "%"),
    n_countries = length(unique(data$raw_finance$country_code)),
    year = latest_year
  )
}

kpis <- calculate_kpis(processed_data)

# Custom CSS ----
custom_css <- HTML("
<style>
  .navbar {
    background: linear-gradient(135deg, #2E7D32 0%, #1B5E20 100%) !important;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }

  .navbar-brand {
    font-weight: 700;
    font-size: 1.5rem;
  }

  .card {
    transition: transform 0.2s, box-shadow 0.2s;
  }

  .card:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 12px rgba(0,0,0,0.15) !important;
  }

  .nav-tabs .nav-link {
    color: #666;
    border: none;
    border-bottom: 2px solid transparent;
    font-weight: 500;
  }

  .nav-tabs .nav-link.active {
    color: #2E7D32;
    border-bottom: 2px solid #2E7D32;
    background: none;
  }

  h4, h5 {
    font-weight: 600;
    color: #212529;
  }

  .section-header {
    border-left: 4px solid #2E7D32;
    padding-left: 1rem;
    margin-bottom: 1.5rem;
  }

  .plot-container {
    background: white;
    border-radius: 8px;
    padding: 1rem;
    box-shadow: 0 1px 3px rgba(0,0,0,0.1);
  }
</style>
")
