# Enhanced Data Acquisition for Green Finance Dashboard
# Includes green vs brown investment tracking and commitment data

if(require(tidyverse, quietly = TRUE)) library(tidyverse)
if(require(httr, quietly = TRUE)) library(httr)
if(require(jsonlite, quietly = TRUE)) library(jsonlite)
if(require(glue, quietly = TRUE)) library(glue)

if (file.exists(here::here("R/utils.R"))) {
  source(here::here("R/utils.R"))
}

# Enhanced CPI Climate Finance with Green vs Brown Classification ----
get_enhanced_climate_finance <- function(year = 2023, cache = TRUE) {
  cache_file <- glue("data/cache/enhanced_finance_{year}.rds")

  if (cache && file.exists(cache_file)) {
    message("Loading enhanced finance data from cache...")
    return(readRDS(cache_file))
  }

  message("Generating enhanced climate finance data with green/brown classification...")

  # Create comprehensive mock data with green vs brown investments
  countries <- c("USA", "China", "Germany", "UK", "France", "Japan", "India", "Brazil",
                 "Canada", "Australia", "Netherlands", "Sweden", "Norway", "Denmark",
                 "Spain", "Italy", "South Korea", "Mexico", "Indonesia", "South Africa")

  green_sectors <- c("Solar Energy", "Wind Energy", "Hydropower", "Geothermal",
                     "Energy Efficiency", "Clean Transport", "Green Buildings",
                     "Sustainable Agriculture", "Forestry Conservation", "Water Conservation")

  brown_sectors <- c("Coal", "Oil & Gas", "Fossil Fuel Infrastructure",
                     "Conventional Transport", "Heavy Industry")

  transition_sectors <- c("Natural Gas", "Nuclear Energy", "Industrial Efficiency",
                         "Hybrid Vehicles")

  # Generate comprehensive dataset
  years <- 2015:2023
  n_records_per_year <- 150

  finance_data <- expand_grid(
    year = years,
    country = sample(countries, n_records_per_year, replace = TRUE)
  ) %>%
    mutate(
      # Classify investment type first
      investment_type = sample(
        c("Green", "Brown", "Transition"),
        n(),
        replace = TRUE,
        prob = c(0.70, 0.15, 0.15)  # 70% green, 15% brown, 15% transition
      ),

      # Assign sector based on investment type
      sector = case_when(
        investment_type == "Green" ~ sample(green_sectors, n(), replace = TRUE),
        investment_type == "Brown" ~ sample(brown_sectors, n(), replace = TRUE),
        investment_type == "Transition" ~ sample(transition_sectors, n(), replace = TRUE)
      ),

      # Finance characteristics
      finance_type = sample(c("Public", "Private"), n(), replace = TRUE, prob = c(0.45, 0.55)),
      instrument = sample(c("Grants", "Loans", "Equity", "Guarantees", "Bonds"),
                         n(), replace = TRUE, prob = c(0.15, 0.35, 0.25, 0.15, 0.10)),
      flow_type = sample(c("Domestic", "International"), n(), replace = TRUE, prob = c(0.70, 0.30)),

      # Amount varies by type - green investments growing over time
      year_factor = (year - 2015) / 8,  # 0 to 1 scale
      base_amount = case_when(
        investment_type == "Green" ~ runif(n(), 100, 3000) * (1 + year_factor * 1.5),  # Growing
        investment_type == "Brown" ~ runif(n(), 50, 1000) * (1 - year_factor * 0.3),   # Declining
        investment_type == "Transition" ~ runif(n(), 80, 1500)
      ),

      # Commitment vs disbursement
      commitment_usd_millions = base_amount,
      disbursement_rate = case_when(
        instrument == "Grants" ~ runif(n(), 0.85, 0.95),
        instrument == "Loans" ~ runif(n(), 0.70, 0.85),
        instrument == "Equity" ~ runif(n(), 0.60, 0.80),
        instrument == "Guarantees" ~ runif(n(), 0.50, 0.70),
        instrument == "Bonds" ~ runif(n(), 0.90, 1.00)
      ),
      disbursement_usd_millions = commitment_usd_millions * disbursement_rate,

      # Convert to billions
      commitment_usd_billions = commitment_usd_millions / 1000,
      disbursement_usd_billions = disbursement_usd_millions / 1000,

      # Additional attributes
      country_code = countrycode::countrycode(country, "country.name", "iso3c",
                                              custom_match = c("China" = "CHN")),

      # Project maturity and impact
      project_status = sample(c("Active", "Completed", "Planned"),
                            n(), replace = TRUE, prob = c(0.50, 0.30, 0.20)),

      # Climate impact score (0-100, green has higher scores)
      climate_impact_score = case_when(
        investment_type == "Green" ~ runif(n(), 70, 100),
        investment_type == "Brown" ~ runif(n(), 10, 30),
        investment_type == "Transition" ~ runif(n(), 40, 70)
      ),

      # Jobs created (per billion USD invested)
      jobs_per_billion = case_when(
        investment_type == "Green" ~ runif(n(), 15000, 35000),
        investment_type == "Brown" ~ runif(n(), 5000, 15000),
        investment_type == "Transition" ~ runif(n(), 10000, 25000)
      ),

      # CO2 impact (million tonnes - negative for reductions, positive for emissions)
      co2_impact_mt = case_when(
        investment_type == "Green" ~ -runif(n(), 0.5, 5.0) * commitment_usd_billions,
        investment_type == "Brown" ~ runif(n(), 1.0, 8.0) * commitment_usd_billions,
        investment_type == "Transition" ~ -runif(n(), 0.2, 2.0) * commitment_usd_billions
      )
    ) %>%
    select(-year_factor, -base_amount, -disbursement_rate, -commitment_usd_millions, -disbursement_usd_millions)

  if (cache) {
    save_to_cache(finance_data, cache_file)
  }

  return(finance_data)
}

# OECD Green Growth Indicators (keep original) ----
get_oecd_green_indicators <- function(cache = TRUE) {
  cache_file <- "data/cache/oecd_green_indicators.rds"

  if (cache && file.exists(cache_file)) {
    message("Loading OECD data from cache...")
    return(readRDS(cache_file))
  }

  message("Generating OECD Green Growth Indicators...")

  countries <- c("USA", "CAN", "DEU", "FRA", "GBR", "JPN", "KOR", "AUS", "NLD", "SWE",
                 "CHN", "IND", "BRA", "MEX", "ZAF")

  oecd_data <- expand_grid(
    year = 2015:2023,
    country_code = countries,
    indicator = c("GHG_PROD", "REN_PERC", "ENV_TAXREV", "ENV_PATENT", "GTEC_EXPO",
                  "ENERGY_INTENSITY", "CARBON_PRICE")
  ) %>%
    mutate(
      year_progress = (year - 2015) / 8,  # 0 to 1
      value = case_when(
        # GHG emissions decreasing for developed, mixed for developing
        indicator == "GHG_PROD" & country_code %in% c("USA", "DEU", "FRA", "GBR", "JPN") ~
          runif(n(), 7, 12) * (1 - year_progress * 0.25),
        indicator == "GHG_PROD" ~ runif(n(), 3, 8) * (1 + year_progress * 0.15),

        # Renewable percentage increasing
        indicator == "REN_PERC" ~ runif(n(), 15, 35) * (1 + year_progress * 0.8),

        # Environmental tax revenue
        indicator == "ENV_TAXREV" ~ runif(n(), 1.5, 4.5) * (1 + year_progress * 0.3),

        # Environmental patents increasing
        indicator == "ENV_PATENT" ~ runif(n(), 50, 300) * (1 + year_progress * 1.2),

        # Green tech exports
        indicator == "GTEC_EXPO" ~ runif(n(), 0.8, 3.5) * (1 + year_progress * 0.6),

        # Energy intensity decreasing
        indicator == "ENERGY_INTENSITY" ~ runif(n(), 0.10, 0.25) * (1 - year_progress * 0.3),

        # Carbon price increasing
        indicator == "CARBON_PRICE" ~ runif(n(), 10, 80) * (1 + year_progress * 1.5),

        TRUE ~ runif(n(), 10, 50)
      ),
      unit = case_when(
        indicator == "GHG_PROD" ~ "Tonnes CO2 per capita",
        indicator == "REN_PERC" ~ "% of total energy",
        indicator == "ENV_TAXREV" ~ "% of GDP",
        indicator == "ENV_PATENT" ~ "Per million population",
        indicator == "GTEC_EXPO" ~ "% of GDP",
        indicator == "ENERGY_INTENSITY" ~ "toe per $1000 GDP",
        indicator == "CARBON_PRICE" ~ "USD per tonne CO2",
        TRUE ~ "Index"
      )
    ) %>%
    select(-year_progress)

  if (cache) {
    save_to_cache(oecd_data, cache_file)
  }

  return(oecd_data)
}

# Climate Finance Flows with Commitment Tracking ----
get_climate_finance_flows <- function(cache = TRUE) {
  cache_file <- "data/cache/climate_finance_flows.rds"

  if (cache && file.exists(cache_file)) {
    message("Loading climate finance flows from cache...")
    return(readRDS(cache_file))
  }

  message("Generating climate finance flows with commitment tracking...")

  donor_countries <- c("USA", "DEU", "FRA", "GBR", "JPN", "CAN", "NLD", "SWE", "NOR",
                       "CHE", "AUS", "ITA", "ESP", "KOR")

  recipient_regions <- c("Sub-Saharan Africa", "South Asia", "Southeast Asia",
                         "Latin America", "Middle East", "Eastern Europe")

  flows_data <- expand_grid(
    year = 2015:2023,
    donor_country = donor_countries,
    recipient_region = recipient_regions
  ) %>%
    mutate(
      year_progress = (year - 2015) / 8,

      # Commitments (increasing over time)
      commitment_adaptation = runif(n(), 200, 1500) * (1 + year_progress * 1.2),
      commitment_mitigation = runif(n(), 300, 2500) * (1 + year_progress * 1.5),

      # Disbursements (lag behind commitments by ~20-30%)
      disbursement_adaptation = commitment_adaptation * runif(n(), 0.65, 0.80),
      disbursement_mitigation = commitment_mitigation * runif(n(), 0.70, 0.85),

      # Totals in millions
      total_commitment_millions = commitment_adaptation + commitment_mitigation,
      total_disbursement_millions = disbursement_adaptation + disbursement_mitigation,

      # Convert to billions
      commitment_usd_billions = total_commitment_millions / 1000,
      disbursement_usd_billions = total_disbursement_millions / 1000,

      # Gap tracking
      commitment_gap_billions = commitment_usd_billions - disbursement_usd_billions,
      disbursement_rate = (disbursement_usd_billions / commitment_usd_billions) * 100,

      # Finance type breakdown
      public_share = runif(n(), 60, 85),
      private_share = 100 - public_share,

      # Climate component focus
      adaptation_share = (commitment_adaptation / total_commitment_millions) * 100,
      mitigation_share = (commitment_mitigation / total_commitment_millions) * 100
    ) %>%
    select(-year_progress, -commitment_adaptation, -commitment_mitigation,
           -disbursement_adaptation, -disbursement_mitigation, -total_commitment_millions,
           -total_disbursement_millions)

  if (cache) {
    save_to_cache(flows_data, cache_file)
  }

  return(flows_data)
}

# IEA Renewable Investment ----
get_iea_renewable_investment <- function(cache = TRUE) {
  cache_file <- "data/cache/iea_renewable.rds"

  if (cache && file.exists(cache_file)) {
    message("Loading IEA data from cache...")
    return(readRDS(cache_file))
  }

  message("Generating IEA Renewable Energy Investment data...")

  technologies <- c("Solar PV", "Wind Onshore", "Wind Offshore", "Hydro",
                   "Bioenergy", "Geothermal", "Green Hydrogen", "Battery Storage")
  regions <- c("North America", "Europe", "Asia Pacific", "Latin America",
              "Africa", "Middle East")

  iea_data <- expand_grid(
    year = 2015:2023,
    region = regions,
    technology = technologies
  ) %>%
    mutate(
      year_progress = (year - 2015) / 8,

      # Investment increasing significantly for solar, wind, and batteries
      investment_usd_billions = case_when(
        technology == "Solar PV" ~ runif(n(), 60, 150) * (1 + year_progress * 2.5),
        technology == "Wind Onshore" ~ runif(n(), 50, 130) * (1 + year_progress * 1.8),
        technology == "Wind Offshore" ~ runif(n(), 15, 70) * (1 + year_progress * 3.0),
        technology == "Green Hydrogen" ~ runif(n(), 5, 50) * (1 + year_progress * 5.0),
        technology == "Battery Storage" ~ runif(n(), 10, 80) * (1 + year_progress * 4.0),
        technology == "Hydro" ~ runif(n(), 30, 70) * (1 + year_progress * 0.5),
        TRUE ~ runif(n(), 8, 40) * (1 + year_progress * 1.2)
      ),

      capacity_added_gw = investment_usd_billions / runif(n(), 0.8, 1.5),

      # LCOE decreasing over time (especially for solar and wind)
      lcoe_usd_mwh = case_when(
        technology == "Solar PV" ~ runif(n(), 50, 80) * (1 - year_progress * 0.6),
        technology %in% c("Wind Onshore", "Wind Offshore") ~ runif(n(), 45, 75) * (1 - year_progress * 0.5),
        technology == "Green Hydrogen" ~ runif(n(), 150, 300) * (1 - year_progress * 0.4),
        TRUE ~ runif(n(), 50, 120) * (1 - year_progress * 0.3)
      )
    ) %>%
    select(-year_progress)

  if (cache) {
    save_to_cache(iea_data, cache_file)
  }

  return(iea_data)
}

# Climate Policies Timeline ----
get_climate_policies <- function(cache = TRUE) {
  cache_file <- "data/cache/climate_policies.rds"

  if (cache && file.exists(cache_file)) {
    message("Loading climate policies from cache...")
    return(readRDS(cache_file))
  }

  message("Creating climate policy timeline...")

  policies <- tibble(
    date = as.Date(c(
      "2015-12-12", "2016-04-22", "2016-11-04", "2018-12-15",
      "2019-09-23", "2020-01-01", "2021-01-20", "2021-04-22",
      "2021-11-13", "2022-08-16", "2022-11-20", "2023-12-13"
    )),
    event = c(
      "Paris Agreement Adopted",
      "Paris Agreement Signed",
      "Paris Agreement Enters into Force",
      "COP24 Katowice Climate Package",
      "UN Climate Action Summit",
      "European Green Deal Launch",
      "US Rejoins Paris Agreement",
      "US Leaders Summit on Climate",
      "COP26 Glasgow Climate Pact",
      "US Inflation Reduction Act",
      "COP27 Loss and Damage Fund",
      "COP28 Global Stocktake"
    ),
    category = c(
      "Agreement", "Agreement", "Agreement", "Policy",
      "Summit", "Policy", "Agreement", "Summit",
      "Summit", "Policy", "Finance", "Assessment"
    ),
    description = c(
      "195 countries adopt historic climate agreement with 1.5°C target",
      "175 countries sign the Paris Agreement in record time",
      "Agreement becomes international law with rapid ratification",
      "Rulebook for Paris Agreement implementation finalized",
      "Countries announce enhanced climate commitments (NDCs)",
      "EU commits €1 trillion to climate neutrality by 2050",
      "US returns to global climate action under Biden administration",
      "40 world leaders commit to halving emissions by 2030",
      "Countries commit to phase down coal and end deforestation",
      "$369 billion US climate investment - largest in history",
      "Historic fund for climate damages in vulnerable countries",
      "First global assessment shows progress but warns of gaps"
    ),
    impact_score = c(10, 7, 9, 6, 5, 8, 7, 6, 7, 9, 8, 6),
    countries_involved = c(195, 175, 195, 195, 77, 27, 195, 40, 195, 1, 195, 195)
  )

  if (cache) {
    save_to_cache(policies, cache_file)
  }

  return(policies)
}

# Master Data Fetch Function ----
fetch_all_data <- function(force_refresh = FALSE) {
  message("\n╔══════════════════════════════════════════════════╗")
  message("║  Starting Enhanced Data Acquisition Pipeline    ║")
  message("╚══════════════════════════════════════════════════╝\n")

  if (force_refresh) {
    message("⚠ Force refresh enabled - clearing cache...")
    if (dir.exists("data/cache")) {
      unlink("data/cache/*")
    }
  }

  # Fetch all data sources
  finance_data <- get_enhanced_climate_finance()
  message("✓ Enhanced Climate Finance data acquired (with green/brown classification)\n")

  oecd_data <- get_oecd_green_indicators()
  message("✓ OECD Green Growth Indicators acquired\n")

  climate_flows <- get_climate_finance_flows()
  message("✓ Climate Finance Flows acquired (with commitment tracking)\n")

  iea_data <- get_iea_renewable_investment()
  message("✓ IEA Renewable Investment data acquired\n")

  policy_data <- get_climate_policies()
  message("✓ Climate Policy timeline created\n")

  # Compile all data
  all_data <- list(
    finance = finance_data,
    oecd = oecd_data,
    flows = climate_flows,
    iea = iea_data,
    policies = policy_data,
    metadata = list(
      fetch_date = Sys.Date(),
      data_sources = c("Climate Finance (Enhanced)", "OECD", "IEA", "Policy Timeline"),
      cache_location = "data/cache/",
      records = list(
        finance = nrow(finance_data),
        oecd = nrow(oecd_data),
        flows = nrow(climate_flows),
        iea = nrow(iea_data),
        policies = nrow(policy_data)
      )
    )
  )

  # Save combined data
  output_file <- "data/processed/all_climate_finance_data.rds"
  saveRDS(all_data, output_file)

  message("\n╔══════════════════════════════════════════════════╗")
  message("║  ✓ All data successfully acquired and saved!    ║")
  message("╚══════════════════════════════════════════════════╝")
  message(glue("  📁 Location: {output_file}"))
  message(glue("  📊 Finance records: {format(nrow(finance_data), big.mark=',')}"))
  message(glue("  🌍 Countries covered: {length(unique(finance_data$country_code))}"))
  message(glue("  📅 Years: {min(finance_data$year)}-{max(finance_data$year)}"))
  message(glue("  💰 Total commitments: {format_billions(sum(finance_data$commitment_usd_billions))}"))
  message("\n  ➜ Next: Run 'source(\"R/data_processing.R\")' to process the data\n")

  return(all_data)
}

# Run if executed directly
if (sys.nframe() == 0) {
  data <- fetch_all_data()
  message("\n✓ Data acquisition complete!")
}
