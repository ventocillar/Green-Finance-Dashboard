# Enhanced Data Processing for Green Finance Dashboard
# Includes YoY analysis, commitment tracking, and green/brown comparisons

if(require(tidyverse, quietly = TRUE)) library(tidyverse)
if(require(countrycode, quietly = TRUE)) library(countrycode)

# Load spatial packages only if available
sf_available <- require(sf, quietly = TRUE)
rnaturalearth_available <- require(rnaturalearth, quietly = TRUE)

if (file.exists(here::here("R/utils.R"))) {
  source(here::here("R/utils.R"))
}

# Process Green vs Brown Investment Trends ----
process_green_brown_analysis <- function(finance_data) {
  message("Processing green vs brown investment analysis...")

  # Overall trends
  investment_trends <- finance_data %>%
    group_by(year, investment_type) %>%
    summarise(
      commitment = sum(commitment_usd_billions, na.rm = TRUE),
      disbursement = sum(disbursement_usd_billions, na.rm = TRUE),
      n_projects = n(),
      avg_climate_impact = mean(climate_impact_score, na.rm = TRUE),
      total_co2_impact = sum(co2_impact_mt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(investment_type) %>%
    arrange(year) %>%
    mutate(
      commitment_growth = calculate_yoy_change(commitment, lag(commitment)),
      disbursement_growth = calculate_yoy_change(disbursement, lag(disbursement)),
      project_growth = calculate_yoy_change(n_projects, lag(n_projects))
    ) %>%
    ungroup()

  # Calculate CAGR for each investment type
  cagr_summary <- investment_trends %>%
    group_by(investment_type) %>%
    summarise(
      start_year = min(year),
      end_year = max(year),
      start_value = first(commitment),
      end_value = last(commitment),
      years = end_year - start_year,
      cagr = calculate_cagr(start_value, end_value, years)
    )

  # By country and investment type
  country_analysis <- finance_data %>%
    group_by(country_code, country, investment_type) %>%
    summarise(
      total_commitment = sum(commitment_usd_billions, na.rm = TRUE),
      total_disbursement = sum(disbursement_usd_billions, na.rm = TRUE),
      disbursement_rate = (total_disbursement / total_commitment) * 100,
      n_projects = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = investment_type,
      values_from = c(total_commitment, total_disbursement, n_projects),
      values_fill = 0
    ) %>%
    mutate(
      green_share = (total_commitment_Green / (total_commitment_Green + total_commitment_Brown + total_commitment_Transition)) * 100,
      total_investment = total_commitment_Green + total_commitment_Brown + total_commitment_Transition
    )

  # By sector
  sector_analysis <- finance_data %>%
    group_by(sector, investment_type) %>%
    summarise(
      commitment = sum(commitment_usd_billions, na.rm = TRUE),
      disbursement = sum(disbursement_usd_billions, na.rm = TRUE),
      avg_impact_score = mean(climate_impact_score, na.rm = TRUE),
      .groups = "drop"
    )

  return(list(
    trends = investment_trends,
    cagr = cagr_summary,
    by_country = country_analysis,
    by_sector = sector_analysis
  ))
}

# Process Commitment vs Disbursement Gap ----
process_commitment_gap_analysis <- function(finance_data, flow_data) {
  message("Analyzing commitment vs disbursement gaps...")

  # Overall gap analysis
  gap_trends <- finance_data %>%
    group_by(year) %>%
    summarise(
      total_commitment = sum(commitment_usd_billions, na.rm = TRUE),
      total_disbursement = sum(disbursement_usd_billions, na.rm = TRUE),
      gap = total_commitment - total_disbursement,
      disbursement_rate = (total_disbursement / total_commitment) * 100,
      .groups = "drop"
    ) %>%
    mutate(
      cumulative_commitment = cumsum(total_commitment),
      cumulative_disbursement = cumsum(total_disbursement),
      cumulative_gap = cumulative_commitment - cumulative_disbursement
    )

  # Gap by instrument type
  instrument_gaps <- finance_data %>%
    group_by(year, instrument) %>%
    summarise(
      commitment = sum(commitment_usd_billions, na.rm = TRUE),
      disbursement = sum(disbursement_usd_billions, na.rm = TRUE),
      gap = commitment - disbursement,
      disbursement_rate = (disbursement / commitment) * 100,
      .groups = "drop"
    )

  # Gap by country (top 20)
  country_gaps <- finance_data %>%
    group_by(country_code, country) %>%
    summarise(
      commitment = sum(commitment_usd_billions, na.rm = TRUE),
      disbursement = sum(disbursement_usd_billions, na.rm = TRUE),
      gap = commitment - disbursement,
      gap_pct = (gap / commitment) * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(commitment)) %>%
    head(20)

  # International flows gap
  flows_gap <- flow_data %>%
    group_by(year) %>%
    summarise(
      commitment = sum(commitment_usd_billions, na.rm = TRUE),
      disbursement = sum(disbursement_usd_billions, na.rm = TRUE),
      gap = commitment - disbursement,
      avg_disbursement_rate = mean(disbursement_rate, na.rm = TRUE),
      .groups = "drop"
    )

  return(list(
    overall = gap_trends,
    by_instrument = instrument_gaps,
    by_country = country_gaps,
    flows = flows_gap
  ))
}

# Year-over-Year Growth Analysis ----
process_yoy_growth <- function(finance_data) {
  message("Calculating year-over-year growth metrics...")

  # Overall YoY growth
  yoy_overall <- finance_data %>%
    group_by(year) %>%
    summarise(
      total_commitment = sum(commitment_usd_billions, na.rm = TRUE),
      total_disbursement = sum(disbursement_usd_billions, na.rm = TRUE),
      n_projects = n(),
      avg_project_size = mean(commitment_usd_billions, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(year) %>%
    mutate(
      commitment_growth = calculate_yoy_change(total_commitment, lag(total_commitment)),
      disbursement_growth = calculate_yoy_change(total_disbursement, lag(total_disbursement)),
      project_count_growth = calculate_yoy_change(n_projects, lag(n_projects)),
      project_size_growth = calculate_yoy_change(avg_project_size, lag(avg_project_size))
    )

  # YoY by investment type
  yoy_by_type <- finance_data %>%
    group_by(year, investment_type) %>%
    summarise(
      commitment = sum(commitment_usd_billions, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(investment_type) %>%
    arrange(year) %>%
    mutate(
      yoy_growth = calculate_yoy_change(commitment, lag(commitment)),
      yoy_absolute = commitment - lag(commitment)
    )

  # YoY by country (top 15)
  top_countries <- finance_data %>%
    group_by(country_code) %>%
    summarise(total = sum(commitment_usd_billions, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    head(15) %>%
    pull(country_code)

  yoy_by_country <- finance_data %>%
    filter(country_code %in% top_countries) %>%
    group_by(year, country_code, country) %>%
    summarise(
      commitment = sum(commitment_usd_billions, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(country_code, country) %>%
    arrange(year) %>%
    mutate(
      yoy_growth = calculate_yoy_change(commitment, lag(commitment))
    )

  return(list(
    overall = yoy_overall,
    by_type = yoy_by_type,
    by_country = yoy_by_country
  ))
}

# Cross-Country Flow Analysis ----
process_cross_country_flows <- function(flow_data) {
  message("Processing cross-country finance flows...")

  # Aggregate flows by donor and recipient
  flow_summary <- flow_data %>%
    group_by(year, donor_country, recipient_region) %>%
    summarise(
      commitment = sum(commitment_usd_billions, na.rm = TRUE),
      disbursement = sum(disbursement_usd_billions, na.rm = TRUE),
      gap = sum(commitment_gap_billions, na.rm = TRUE),
      .groups = "drop"
    )

  # Top donors
  top_donors <- flow_data %>%
    group_by(donor_country) %>%
    summarise(
      total_commitment = sum(commitment_usd_billions, na.rm = TRUE),
      total_disbursement = sum(disbursement_usd_billions, na.rm = TRUE),
      avg_disbursement_rate = mean(disbursement_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_commitment)) %>%
    head(15)

  # Top recipient regions
  top_recipients <- flow_data %>%
    group_by(recipient_region) %>%
    summarise(
      total_received_commitment = sum(commitment_usd_billions, na.rm = TRUE),
      total_received_disbursement = sum(disbursement_usd_billions, na.rm = TRUE),
      avg_disbursement_rate = mean(disbursement_rate, na.rm = TRUE),
      n_donors = n_distinct(donor_country),
      .groups = "drop"
    ) %>%
    arrange(desc(total_received_commitment))

  # Flow matrix for Sankey diagram
  flow_matrix <- flow_data %>%
    filter(year == max(year)) %>%
    group_by(donor_country, recipient_region) %>%
    summarise(
      flow_value = sum(disbursement_usd_billions, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(flow_value > 0.5) %>%  # Filter small flows for clarity
    arrange(desc(flow_value))

  # Temporal evolution
  flow_evolution <- flow_data %>%
    group_by(year) %>%
    summarise(
      total_commitment = sum(commitment_usd_billions, na.rm = TRUE),
      total_disbursement = sum(disbursement_usd_billions, na.rm = TRUE),
      n_donor_countries = n_distinct(donor_country),
      n_recipient_regions = n_distinct(recipient_region),
      .groups = "drop"
    ) %>%
    arrange(year) %>%
    mutate(
      commitment_growth = calculate_yoy_change(total_commitment, lag(total_commitment))
    )

  return(list(
    summary = flow_summary,
    top_donors = top_donors,
    top_recipients = top_recipients,
    matrix = flow_matrix,
    evolution = flow_evolution
  ))
}

# Create Geospatial Data ----
create_geospatial_data <- function(green_brown_analysis) {
  message("Creating geospatial datasets...")

  country_data <- green_brown_analysis$by_country %>%
    mutate(
      green_label = ifelse(green_share >= 50, "Green Leader", "Needs Improvement"),
      total_category = cut(total_investment,
                          breaks = c(0, 50, 200, 500, 1000, Inf),
                          labels = c("<50B", "50-200B", "200-500B", "500B-1T", ">1T"))
    )

  # If spatial packages available, create map data
  if (sf_available && rnaturalearth_available) {
    world <- ne_countries(scale = "medium", returnclass = "sf") %>%
      select(iso_a3, name, geometry) %>%
      rename(country_code = iso_a3)

    geo_data <- world %>%
      left_join(country_data, by = "country_code") %>%
      mutate(
        green_share = replace_na(green_share, 0),
        total_investment = replace_na(total_investment, 0)
      )

    return(geo_data)
  }

  return(country_data)
}

# Policy Impact Analysis ----
calculate_policy_impacts <- function(finance_data, policy_data) {
  message("Calculating policy impact correlations...")

  annual_finance <- finance_data %>%
    group_by(year) %>%
    summarise(
      total_commitment = sum(commitment_usd_billions, na.rm = TRUE),
      total_disbursement = sum(disbursement_usd_billions, na.rm = TRUE),
      green_share = sum(commitment_usd_billions[investment_type == "Green"]) /
                   sum(commitment_usd_billions) * 100,
      avg_climate_impact = mean(climate_impact_score, na.rm = TRUE),
      .groups = "drop"
    )

  policy_years <- policy_data %>%
    mutate(year = lubridate::year(date)) %>%
    group_by(year) %>%
    summarise(
      policy_count = n(),
      max_impact = max(impact_score, na.rm = TRUE),
      total_countries = sum(countries_involved, na.rm = TRUE),
      .groups = "drop"
    )

  impact_analysis <- annual_finance %>%
    left_join(policy_years, by = "year") %>%
    mutate(
      policy_count = replace_na(policy_count, 0),
      max_impact = replace_na(max_impact, 0),
      total_countries = replace_na(total_countries, 0)
    ) %>%
    arrange(year) %>%
    mutate(
      finance_growth = calculate_yoy_change(total_commitment, lag(total_commitment))
    )

  return(impact_analysis)
}

# Create Comprehensive Time Series ----
create_time_series_data <- function(all_data) {
  message("Creating comprehensive time series datasets...")

  # Finance time series
  finance_ts <- all_data$finance %>%
    group_by(year) %>%
    summarise(
      total_commitment = sum(commitment_usd_billions, na.rm = TRUE),
      total_disbursement = sum(disbursement_usd_billions, na.rm = TRUE),
      green_commitment = sum(commitment_usd_billions[investment_type == "Green"], na.rm = TRUE),
      brown_commitment = sum(commitment_usd_billions[investment_type == "Brown"], na.rm = TRUE),
      public_commitment = sum(commitment_usd_billions[finance_type == "Public"], na.rm = TRUE),
      private_commitment = sum(commitment_usd_billions[finance_type == "Private"], na.rm = TRUE),
      .groups = "drop"
    )

  # IEA renewable investment
  iea_ts <- all_data$iea %>%
    group_by(year) %>%
    summarise(
      renewable_investment = sum(investment_usd_billions, na.rm = TRUE),
      capacity_added = sum(capacity_added_gw, na.rm = TRUE),
      avg_lcoe = mean(lcoe_usd_mwh, na.rm = TRUE),
      .groups = "drop"
    )

  # OECD indicators
  oecd_ts <- all_data$oecd %>%
    filter(indicator %in% c("GHG_PROD", "REN_PERC", "ENV_TAXREV", "CARBON_PRICE")) %>%
    group_by(year, indicator) %>%
    summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = indicator, values_from = avg_value, values_fill = 0)

  # Combine all time series
  combined_ts <- finance_ts %>%
    left_join(iea_ts, by = "year") %>%
    left_join(oecd_ts, by = "year") %>%
    pivot_longer(
      cols = -year,
      names_to = "metric",
      values_to = "value"
    )

  return(combined_ts)
}

# Master Processing Function ----
process_all_data <- function(data_file = "data/processed/all_climate_finance_data.rds") {
  message("\n╔══════════════════════════════════════════════════╗")
  message("║  Starting Enhanced Data Processing Pipeline     ║")
  message("╚══════════════════════════════════════════════════╝\n")

  if (!file.exists(data_file)) {
    stop("❌ Data file not found. Please run data_acquisition.R first.")
  }

  all_data <- readRDS(data_file)

  # Process all analyses
  green_brown <- process_green_brown_analysis(all_data$finance)
  message("✓ Green vs Brown analysis complete\n")

  commitment_gap <- process_commitment_gap_analysis(all_data$finance, all_data$flows)
  message("✓ Commitment gap analysis complete\n")

  yoy_growth <- process_yoy_growth(all_data$finance)
  message("✓ Year-over-year growth analysis complete\n")

  cross_flows <- process_cross_country_flows(all_data$flows)
  message("✓ Cross-country flow analysis complete\n")

  geo_data <- create_geospatial_data(green_brown)
  message("✓ Geospatial data created\n")

  policy_impacts <- calculate_policy_impacts(all_data$finance, all_data$policies)
  message("✓ Policy impact analysis complete\n")

  time_series <- create_time_series_data(all_data)
  message("✓ Time series data created\n")

  # Compile processed data
  processed_data <- list(
    green_brown = green_brown,
    commitment_gap = commitment_gap,
    yoy_growth = yoy_growth,
    cross_country_flows = cross_flows,
    geo = geo_data,
    policy_impacts = policy_impacts,
    time_series = time_series,
    policies = all_data$policies,
    raw_finance = all_data$finance,
    raw_oecd = all_data$oecd,
    raw_iea = all_data$iea,
    metadata = list(
      process_date = Sys.Date(),
      n_countries = length(unique(all_data$finance$country_code)),
      year_range = range(all_data$finance$year),
      total_commitment = sum(all_data$finance$commitment_usd_billions, na.rm = TRUE),
      total_disbursement = sum(all_data$finance$disbursement_usd_billions, na.rm = TRUE),
      green_share = (sum(all_data$finance$commitment_usd_billions[all_data$finance$investment_type == "Green"], na.rm = TRUE) /
                    sum(all_data$finance$commitment_usd_billions, na.rm = TRUE)) * 100
    )
  )

  # Save processed data
  output_file <- "data/processed/processed_climate_finance.rds"
  saveRDS(processed_data, output_file)

  message("\n╔══════════════════════════════════════════════════╗")
  message("║  ✓ All data processing complete!                ║")
  message("╚══════════════════════════════════════════════════╝")
  message(glue::glue("  📁 Output: {output_file}"))
  message(glue::glue("  🌍 Countries: {processed_data$metadata$n_countries}"))
  message(glue::glue("  📅 Years: {paste(processed_data$metadata$year_range, collapse='-')}"))
  message(glue::glue("  💰 Total Commitments: {format_billions(processed_data$metadata$total_commitment)}"))
  message(glue::glue("  💵 Total Disbursements: {format_billions(processed_data$metadata$total_disbursement)}"))
  message(glue::glue("  🌱 Green Share: {round(processed_data$metadata$green_share, 1)}%"))
  message("\n  ➜ Next: Run 'source(\"R/visualizations.R\")' to create visualizations\n")

  return(processed_data)
}

# Run if executed directly
if (sys.nframe() == 0) {
  processed_data <- process_all_data()
  message("\n✓ Data processing complete!")
}
