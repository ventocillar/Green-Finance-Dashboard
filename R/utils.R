# Utility Functions for Green Finance Dashboard

library(tidyverse)
library(scales)

# Formatting Functions ----

format_billions <- function(x) {
  if (is.na(x) || is.null(x)) return("N/A")
  if (x >= 1000) {
    paste0("$", round(x / 1000, 1), "T")
  } else if (x >= 1) {
    paste0("$", round(x, 1), "B")
  } else {
    paste0("$", round(x * 1000, 0), "M")
  }
}

format_percentage <- function(x, digits = 1) {
  if (is.na(x) || is.null(x)) return("N/A")
  paste0(round(x, digits), "%")
}

format_number <- function(x, suffix = "") {
  if (is.na(x) || is.null(x)) return("N/A")
  paste0(comma(round(x, 0)), suffix)
}

# Color Palettes ----

get_color_palette <- function(n = 5, type = "categorical") {
  if (type == "categorical") {
    colors <- c(
      "#2E7D32",  # Green
      "#1976D2",  # Blue
      "#F57C00",  # Orange
      "#7B1FA2",  # Purple
      "#C62828",  # Red
      "#00796B",  # Teal
      "#F9A825",  # Yellow
      "#5D4037"   # Brown
    )
    return(colors[1:min(n, length(colors))])
  } else if (type == "sequential") {
    return(colorRampPalette(c("#E8F5E9", "#2E7D32"))(n))
  } else if (type == "diverging") {
    return(colorRampPalette(c("#C62828", "#FFFFFF", "#2E7D32"))(n))
  } else if (type == "green_shades") {
    return(c("#E8F5E9", "#A5D6A7", "#66BB6A", "#43A047", "#2E7D32", "#1B5E20"))
  }
}

get_investment_colors <- function() {
  list(
    green = "#2E7D32",
    brown = "#5D4037",
    transition = "#F57C00",
    public = "#1976D2",
    private = "#7B1FA2"
  )
}

# Data Transformation Functions ----

normalize_values <- function(x, method = "minmax") {
  if (all(is.na(x))) return(x)

  if (method == "minmax") {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  } else if (method == "zscore") {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  } else {
    x
  }
}

calculate_growth_rate <- function(df, value_col) {
  df %>%
    arrange(year) %>%
    mutate(
      prev_value = lag(!!sym(value_col)),
      growth_rate = (!!sym(value_col) - prev_value) / prev_value * 100,
      growth_abs = !!sym(value_col) - prev_value
    )
}

calculate_yoy_change <- function(current, previous) {
  if (is.na(current) || is.na(previous) || previous == 0) return(NA)
  ((current - previous) / previous) * 100
}

calculate_cagr <- function(start_value, end_value, years) {
  if (is.na(start_value) || is.na(end_value) || years <= 0 || start_value <= 0) return(NA)
  ((end_value / start_value)^(1/years) - 1) * 100
}

# Geographic Functions ----

get_country_coords <- function() {
  tibble(
    country_code = c("USA", "CHN", "DEU", "GBR", "FRA", "JPN", "IND", "BRA",
                     "CAN", "AUS", "NLD", "SWE", "NOR", "DNK", "ESP",
                     "ITA", "KOR", "MEX", "IDN", "ZAF"),
    country_name = c("United States", "China", "Germany", "United Kingdom", "France",
                     "Japan", "India", "Brazil", "Canada", "Australia",
                     "Netherlands", "Sweden", "Norway", "Denmark", "Spain",
                     "Italy", "South Korea", "Mexico", "Indonesia", "South Africa"),
    lat = c(37.09, 35.86, 51.17, 55.38, 46.23, 36.20, 20.59, -14.24,
            56.13, -25.27, 52.13, 60.13, 60.47, 56.26, 40.46,
            41.87, 35.91, 23.63, -0.79, -30.56),
    lon = c(-95.71, 104.20, 10.45, -3.44, 2.21, 138.25, 78.96, -51.93,
            -106.35, 133.78, 5.29, 18.64, 8.47, 9.50, -3.75,
            12.57, 127.77, -102.55, 113.92, 22.94)
  )
}

get_region_mapping <- function() {
  tibble(
    country_code = c("USA", "CAN", "MEX", "BRA", "CHN", "JPN", "IND", "IDN", "KOR",
                     "DEU", "GBR", "FRA", "ITA", "ESP", "NLD", "SWE", "NOR", "DNK",
                     "AUS", "NZL", "ZAF"),
    region = c("North America", "North America", "Latin America", "Latin America",
               "Asia", "Asia", "Asia", "Asia", "Asia",
               "Europe", "Europe", "Europe", "Europe", "Europe", "Europe", "Europe", "Europe", "Europe",
               "Oceania", "Oceania", "Africa")
  )
}

# Statistical Functions ----

calculate_percentile <- function(x, value) {
  if (all(is.na(x))) return(NA)
  ecdf(x)(value) * 100
}

calculate_summary_stats <- function(x) {
  list(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    q25 = quantile(x, 0.25, na.rm = TRUE),
    q75 = quantile(x, 0.75, na.rm = TRUE)
  )
}

# Cache Management ----

cache_key <- function(...) {
  paste(..., sep = "_")
}

save_to_cache <- function(data, cache_file) {
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(data, cache_file)
  message(glue::glue("Data cached to {cache_file}"))
}

load_from_cache <- function(cache_file) {
  if (file.exists(cache_file)) {
    message(glue::glue("Loading from cache: {cache_file}"))
    return(readRDS(cache_file))
  }
  return(NULL)
}

# Investment Classification ----

classify_investment_type <- function(sector, instrument) {
  green_sectors <- c("Renewable Energy", "Energy Efficiency", "Clean Transport",
                     "Sustainable Agriculture", "Forestry Conservation", "Green Buildings",
                     "Water Conservation", "Waste Management", "Biodiversity")

  brown_sectors <- c("Fossil Fuels", "Coal", "Oil & Gas", "Conventional Transport",
                     "Heavy Industry", "Mining")

  transition_sectors <- c("Natural Gas", "Nuclear", "Hybrid Vehicles", "Industrial Efficiency")

  case_when(
    sector %in% green_sectors ~ "Green",
    sector %in% brown_sectors ~ "Brown",
    sector %in% transition_sectors ~ "Transition",
    TRUE ~ "Other"
  )
}

# Dashboard Helper Functions ----

create_value_box_data <- function(value, title, icon, color = "green", change = NULL) {
  list(
    value = value,
    title = title,
    icon = icon,
    color = color,
    change = change
  )
}

create_trend_icon <- function(value) {
  if (is.na(value)) return("")
  if (value > 0) {
    return("↑")
  } else if (value < 0) {
    return("↓")
  } else {
    return("→")
  }
}

create_trend_color <- function(value, inverse = FALSE) {
  if (is.na(value)) return("gray")

  if (inverse) {
    if (value > 0) return("red") else return("green")
  } else {
    if (value > 0) return("green") else return("red")
  }
}

# Export Functions ----

prepare_for_export <- function(data, format = "csv") {
  if (format == "csv") {
    return(data)
  } else if (format == "json") {
    return(jsonlite::toJSON(data, pretty = TRUE))
  } else if (format == "excel") {
    return(data)
  }
}
