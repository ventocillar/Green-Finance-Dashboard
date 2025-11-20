# Comprehensive Visualization Functions for Green Finance Dashboard
# Beautiful, professional charts with modern aesthetics

library(tidyverse)
library(plotly)
library(leaflet)
library(scales)
library(viridis)

if (file.exists(here::here("R/utils.R"))) {
  source(here::here("R/utils.R"))
}

# Theme settings
theme_dashboard <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# 1. Green vs Brown Investment Comparison ----
create_green_brown_comparison <- function(green_brown_data) {
  message("Creating green vs brown investment comparison...")

  colors <- get_investment_colors()

  plot_data <- green_brown_data$trends %>%
    filter(investment_type %in% c("Green", "Brown"))

  p <- plot_ly(plot_data, x = ~year, y = ~commitment, color = ~investment_type,
               type = "scatter", mode = "lines+markers",
               colors = c("Green" = colors$green, "Brown" = colors$brown),
               marker = list(size = 10),
               line = list(width = 3),
               text = ~paste0("<b>", investment_type, " Investment</b><br>",
                             "Year: ", year, "<br>",
                             "Commitment: ", format_billions(commitment), "<br>",
                             "Growth: ", ifelse(!is.na(commitment_growth),
                                              paste0(round(commitment_growth, 1), "%"), "N/A")),
               hovertemplate = "%{text}<extra></extra>") %>%
    layout(
      title = list(
        text = "<b>Green vs Brown Investment Trends</b><br><sub>Global climate finance commitments over time</sub>",
        font = list(size = 18)
      ),
      xaxis = list(title = "<b>Year</b>", showgrid = TRUE, gridcolor = "#f0f0f0"),
      yaxis = list(title = "<b>Investment (USD Billions)</b>", showgrid = TRUE, gridcolor = "#f0f0f0"),
      legend = list(title = list(text = "<b>Investment Type</b>"), x = 0.5, xanchor = "center", y = -0.15, orientation = "h"),
      hovermode = "x unified",
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(t = 80)
    )

  return(p)
}

# 2. Stacked Area Chart - Investment Types Over Time ----
create_investment_area_chart <- function(green_brown_data) {
  message("Creating investment type area chart...")

  plot_data <- green_brown_data$trends %>%
    select(year, investment_type, commitment) %>%
    pivot_wider(names_from = investment_type, values_from = commitment, values_fill = 0)

  colors <- get_color_palette(3, "categorical")

  p <- plot_ly(plot_data, x = ~year, type = "scatter", mode = "none", stackgroup = "one",
               fillcolor = colors[1], name = "Green",
               y = ~Green,
               text = ~paste0("<b>Green Investment</b><br>", format_billions(Green)),
               hovertemplate = "%{text}<extra></extra>") %>%
    add_trace(y = ~Transition, fillcolor = colors[3], name = "Transition",
              text = ~paste0("<b>Transition Investment</b><br>", format_billions(Transition)),
              hovertemplate = "%{text}<extra></extra>") %>%
    add_trace(y = ~Brown, fillcolor = colors[5], name = "Brown",
              text = ~paste0("<b>Brown Investment</b><br>", format_billions(Brown)),
              hovertemplate = "%{text}<extra></extra>") %>%
    layout(
      title = list(text = "<b>Investment Composition Over Time</b><br><sub>Stacked area view of climate finance by type</sub>", font = list(size = 18)),
      xaxis = list(title = "<b>Year</b>"),
      yaxis = list(title = "<b>Cumulative Investment (USD Billions)</b>"),
      hovermode = "x unified",
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      legend = list(x = 0.5, xanchor = "center", y = -0.15, orientation = "h")
    )

  return(p)
}

# 3. Commitment vs Disbursement Gap Analysis ----
create_commitment_gap_chart <- function(gap_data) {
  message("Creating commitment-disbursement gap chart...")

  colors <- c("#2E7D32", "#1976D2", "#C62828")

  p <- plot_ly(gap_data$overall, x = ~year) %>%
    add_trace(y = ~total_commitment, name = "Commitments",
              type = "bar", marker = list(color = colors[1]),
              text = ~paste0("Commitments: ", format_billions(total_commitment)),
              hovertemplate = "%{text}<extra></extra>") %>%
    add_trace(y = ~total_disbursement, name = "Disbursements",
              type = "bar", marker = list(color = colors[2]),
              text = ~paste0("Disbursements: ", format_billions(total_disbursement)),
              hovertemplate = "%{text}<extra></extra>") %>%
    add_trace(y = ~gap, name = "Gap",
              type = "scatter", mode = "lines+markers",
              line = list(color = colors[3], width = 3, dash = "dash"),
              marker = list(size = 10, color = colors[3]),
              yaxis = "y2",
              text = ~paste0("Gap: ", format_billions(gap), "<br>Rate: ", round(disbursement_rate, 1), "%"),
              hovertemplate = "%{text}<extra></extra>") %>%
    layout(
      title = list(text = "<b>Commitment vs Disbursement Gap</b><br><sub>Tracking the delivery of climate finance promises</sub>", font = list(size = 18)),
      xaxis = list(title = "<b>Year</b>"),
      yaxis = list(title = "<b>Commitments & Disbursements (USD Billions)</b>", side = "left"),
      yaxis2 = list(title = "<b>Gap (USD Billions)</b>", overlaying = "y", side = "right"),
      barmode = "group",
      hovermode = "x unified",
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      legend = list(x = 0.5, xanchor = "center", y = -0.15, orientation = "h")
    )

  return(p)
}

# 4. Cumulative Gap Visualization ----
create_cumulative_gap_chart <- function(gap_data) {
  message("Creating cumulative gap visualization...")

  p <- plot_ly(gap_data$overall, x = ~year) %>%
    add_trace(y = ~cumulative_commitment, name = "Cumulative Commitments",
              type = "scatter", mode = "lines+markers", fill = "tozeroy",
              fillcolor = "rgba(46, 125, 50, 0.2)",
              line = list(color = "#2E7D32", width = 3),
              marker = list(size = 8, color = "#2E7D32"),
              text = ~paste0("Cumulative Commitments: ", format_billions(cumulative_commitment)),
              hovertemplate = "%{text}<extra></extra>") %>%
    add_trace(y = ~cumulative_disbursement, name = "Cumulative Disbursements",
              type = "scatter", mode = "lines+markers", fill = "tozeroy",
              fillcolor = "rgba(25, 118, 210, 0.2)",
              line = list(color = "#1976D2", width = 3),
              marker = list(size = 8, color = "#1976D2"),
              text = ~paste0("Cumulative Disbursements: ", format_billions(cumulative_disbursement)),
              hovertemplate = "%{text}<extra></extra>") %>%
    layout(
      title = list(text = "<b>Cumulative Climate Finance</b><br><sub>Total commitments vs disbursements over time</sub>", font = list(size = 18)),
      xaxis = list(title = "<b>Year</b>"),
      yaxis = list(title = "<b>Cumulative Amount (USD Billions)</b>"),
      hovermode = "x unified",
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      legend = list(x = 0.5, xanchor = "center", y = -0.15, orientation = "h")
    )

  return(p)
}

# 5. Year-over-Year Growth Chart ----
create_yoy_growth_chart <- function(yoy_data) {
  message("Creating year-over-year growth chart...")

  colors <- get_investment_colors()

  p <- plot_ly(yoy_data$by_type, x = ~year, y = ~yoy_growth,
               color = ~investment_type, type = "bar",
               colors = c("Green" = colors$green, "Brown" = colors$brown, "Transition" = colors$transition),
               text = ~paste0("<b>", investment_type, "</b><br>",
                             "Growth: ", ifelse(!is.na(yoy_growth), paste0(round(yoy_growth, 1), "%"), "N/A")),
               hovertemplate = "%{text}<extra></extra>") %>%
    layout(
      title = list(text = "<b>Year-over-Year Growth by Investment Type</b><br><sub>Annual percentage change in commitments</sub>", font = list(size = 18)),
      xaxis = list(title = "<b>Year</b>"),
      yaxis = list(title = "<b>YoY Growth (%)</b>", zeroline = TRUE, zerolinecolor = "#333", zerolinewidth = 2),
      barmode = "group",
      hovermode = "x unified",
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      legend = list(title = list(text = "<b>Investment Type</b>"), x = 0.5, xanchor = "center", y = -0.15, orientation = "h")
    )

  return(p)
}

# 6. Sankey Diagram - Cross-Country Flows ----
create_flow_sankey <- function(flow_data) {
  message("Creating Sankey diagram for cross-country flows...")

  # Prepare data for Sankey
  sankey_data <- flow_data$matrix %>%
    filter(flow_value > 1) %>%  # Only significant flows
    arrange(desc(flow_value)) %>%
    head(50)  # Top 50 flows

  # Create node labels
  sources <- unique(sankey_data$donor_country)
  targets <- unique(sankey_data$recipient_region)
  all_nodes <- c(sources, targets)

  # Create indices
  sankey_data <- sankey_data %>%
    mutate(
      source_idx = match(donor_country, all_nodes) - 1,
      target_idx = match(recipient_region, all_nodes) - 1
    )

  # Color palette
  node_colors <- colorRampPalette(c("#2E7D32", "#1976D2", "#F57C00", "#7B1FA2"))(length(all_nodes))

  p <- plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = all_nodes,
      color = node_colors,
      pad = 15,
      thickness = 20,
      line = list(color = "white", width = 0.5)
    ),
    link = list(
      source = sankey_data$source_idx,
      target = sankey_data$target_idx,
      value = sankey_data$flow_value,
      color = "rgba(46, 125, 50, 0.3)"
    )
  ) %>%
    layout(
      title = list(text = "<b>International Climate Finance Flows</b><br><sub>Donor countries to recipient regions (USD Billions)</sub>", font = list(size = 18)),
      font = list(size = 12),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )

  return(p)
}

# 7. Choropleth Map - Green Investment Share ----
create_green_share_map <- function(geo_data) {
  message("Creating green investment share choropleth map...")

  if ("sf" %in% class(geo_data)) {
    # Spatial data available - create Leaflet map
    pal <- colorNumeric(
      palette = c("#C62828", "#FDD835", "#2E7D32"),
      domain = geo_data$green_share,
      na.color = "#CCCCCC"
    )

    labels <- sprintf(
      "<strong>%s</strong><br/>Green Share: %s<br/>Total Investment: %s",
      geo_data$name,
      paste0(round(geo_data$green_share, 1), "%"),
      format_billions(geo_data$total_investment)
    ) %>% lapply(htmltools::HTML)

    map <- leaflet(geo_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(green_share),
        weight = 1.5,
        opacity = 1,
        color = "#666",
        fillOpacity = 0.75,
        highlight = highlightOptions(
          weight = 3,
          color = "#333",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", padding = "5px 10px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~green_share,
        opacity = 0.8,
        title = "Green Investment<br>Share (%)",
        position = "bottomright",
        labFormat = labelFormat(suffix = "%")
      ) %>%
      addControl(
        html = "<div style='background: white; padding: 10px; border-radius: 5px;'>
                <h4 style='margin: 0;'>Global Green Investment Distribution</h4>
                <p style='margin: 5px 0 0 0; font-size: 12px; color: #666;'>
                Countries colored by percentage of green climate finance</p></div>",
        position = "topleft"
      )

    return(map)
  } else {
    # Fallback to simple bar chart
    plot_data <- geo_data %>%
      arrange(desc(total_investment)) %>%
      head(20)

    p <- plot_ly(plot_data, x = ~reorder(country_code, green_share), y = ~green_share,
                 type = "bar",
                 marker = list(color = ~green_share,
                             colorscale = list(c(0, "#C62828"), c(0.5, "#FDD835"), c(1, "#2E7D32")),
                             showscale = TRUE,
                             colorbar = list(title = "Green<br>Share (%)")),
                 text = ~paste0("<b>", country, "</b><br>Green Share: ", round(green_share, 1), "%<br>",
                              "Total: ", format_billions(total_investment)),
                 hovertemplate = "%{text}<extra></extra>") %>%
      layout(
        title = list(text = "<b>Green Investment Share by Country</b>", font = list(size = 18)),
        xaxis = list(title = "<b>Country</b>"),
        yaxis = list(title = "<b>Green Investment Share (%)</b>"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )

    return(p)
  }
}

# 8. Sector Breakdown Sunburst ----
create_sector_sunburst <- function(green_brown_data, raw_finance) {
  message("Creating sector breakdown sunburst...")

  sunburst_data <- raw_finance %>%
    group_by(investment_type, sector, instrument) %>%
    summarise(value = sum(commitment_usd_billions, na.rm = TRUE), .groups = "drop") %>%
    filter(value > 0)

  # Create hierarchical structure
  labels <- c("Total")
  parents <- c("")
  values <- c(sum(sunburst_data$value))

  # Level 1: Investment type
  for (inv_type in unique(sunburst_data$investment_type)) {
    labels <- c(labels, inv_type)
    parents <- c(parents, "Total")
    values <- c(values, sum(sunburst_data$value[sunburst_data$investment_type == inv_type]))
  }

  # Level 2: Sectors
  for (i in 1:nrow(sunburst_data)) {
    sector_label <- paste(sunburst_data$investment_type[i], "-", sunburst_data$sector[i])
    labels <- c(labels, sector_label)
    parents <- c(parents, sunburst_data$investment_type[i])

    sector_value <- sum(sunburst_data$value[sunburst_data$investment_type == sunburst_data$investment_type[i] &
                                           sunburst_data$sector == sunburst_data$sector[i]])
    values <- c(values, sector_value)
  }

  colors <- get_color_palette(length(labels), "categorical")

  p <- plot_ly(
    labels = labels,
    parents = parents,
    values = values,
    type = "sunburst",
    branchvalues = "total",
    marker = list(colors = colors),
    hovertemplate = "<b>%{label}</b><br>Amount: %{value:.1f}B USD<br>Percentage: %{percentParent}<extra></extra>"
  ) %>%
    layout(
      title = list(text = "<b>Investment Breakdown by Type, Sector & Instrument</b>", font = list(size = 18)),
      margin = list(l = 0, r = 0, b = 50, t = 80),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )

  return(p)
}

# 9. Top Countries Bar Chart with Comparison ----
create_top_countries_comparison <- function(green_brown_data) {
  message("Creating top countries comparison chart...")

  plot_data <- green_brown_data$by_country %>%
    arrange(desc(total_investment)) %>%
    head(15) %>%
    select(country, total_commitment_Green, total_commitment_Brown, total_commitment_Transition) %>%
    pivot_longer(cols = starts_with("total_commitment"), names_to = "type", values_to = "value") %>%
    mutate(type = str_remove(type, "total_commitment_"))

  colors <- get_investment_colors()
  color_map <- c("Green" = colors$green, "Brown" = colors$brown, "Transition" = colors$transition)

  p <- plot_ly(plot_data, x = ~reorder(country, value), y = ~value,
               color = ~type, type = "bar",
               colors = color_map,
               text = ~paste0("<b>", type, "</b><br>", format_billions(value)),
               hovertemplate = "%{text}<extra></extra>") %>%
    layout(
      title = list(text = "<b>Top 15 Countries by Climate Finance</b><br><sub>Breakdown by investment type</sub>", font = list(size = 18)),
      xaxis = list(title = ""),
      yaxis = list(title = "<b>Investment (USD Billions)</b>"),
      barmode = "stack",
      hovermode = "x unified",
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      legend = list(title = list(text = "<b>Type</b>"), x = 0.5, xanchor = "center", y = -0.2, orientation = "h"),
      margin = list(b = 100)
    ) %>%
    config(displayModeBar = TRUE)

  return(p)
}

# 10. Policy Impact Timeline ----
create_policy_timeline_chart <- function(policy_data, policy_impacts) {
  message("Creating policy timeline with finance trends...")

  p <- plot_ly() %>%
    add_trace(
      data = policy_impacts,
      x = ~as.Date(paste0(year, "-07-01")),
      y = ~total_commitment,
      type = "scatter",
      mode = "lines+markers",
      name = "Climate Finance",
      line = list(color = "#2E7D32", width = 3),
      marker = list(size = 10, color = "#2E7D32"),
      yaxis = "y",
      text = ~paste0("Year: ", year, "<br>Finance: ", format_billions(total_commitment)),
      hovertemplate = "%{text}<extra></extra>"
    )

  # Add policy events
  for (i in 1:nrow(policy_data)) {
    p <- p %>%
      add_annotations(
        x = policy_data$date[i],
        y = max(policy_impacts$total_commitment) * 0.9,
        text = paste0("📍 ", policy_data$event[i]),
        showarrow = TRUE,
        arrowhead = 2,
        arrowsize = 1,
        arrowwidth = 2,
        arrowcolor = "#FF6B6B",
        ax = 0,
        ay = -30 - (i %% 3) * 20,
        font = list(size = 10, color = "#333"),
        bgcolor = "rgba(255, 255, 255, 0.9)",
        bordercolor = "#FF6B6B",
        borderwidth = 2,
        borderpad = 4
      )
  }

  p <- p %>%
    layout(
      title = list(text = "<b>Climate Policy Events & Finance Trends</b><br><sub>Major policy milestones and their correlation with finance flows</sub>", font = list(size = 18)),
      xaxis = list(title = "<b>Date</b>", type = "date"),
      yaxis = list(title = "<b>Climate Finance Commitment (USD Billions)</b>", side = "left"),
      hovermode = "x unified",
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      showlegend = FALSE,
      margin = list(t = 100)
    )

  return(p)
}

# 11. Instrument Performance Heatmap ----
create_instrument_heatmap <- function(gap_data) {
  message("Creating instrument disbursement rate heatmap...")

  heatmap_data <- gap_data$by_instrument %>%
    select(year, instrument, disbursement_rate) %>%
    pivot_wider(names_from = instrument, values_from = disbursement_rate, values_fill = 0)

  years <- heatmap_data$year
  heatmap_matrix <- as.matrix(heatmap_data[, -1])

  p <- plot_ly(
    x = colnames(heatmap_matrix),
    y = years,
    z = heatmap_matrix,
    type = "heatmap",
    colorscale = list(
      c(0, "#C62828"),
      c(0.5, "#FDD835"),
      c(1, "#2E7D32")
    ),
    colorbar = list(title = "Disbursement<br>Rate (%)", titleside = "right"),
    hovertemplate = "Year: %{y}<br>Instrument: %{x}<br>Disbursement Rate: %{z:.1f}%<extra></extra>"
  ) %>%
    layout(
      title = list(text = "<b>Disbursement Rate by Instrument Type</b><br><sub>Performance heatmap showing delivery efficiency</sub>", font = list(size = 18)),
      xaxis = list(title = "<b>Financial Instrument</b>", side = "bottom"),
      yaxis = list(title = "<b>Year</b>"),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )

  return(p)
}

# 12. Renewable Investment by Technology ----
create_renewable_tech_chart <- function(iea_data) {
  message("Creating renewable technology investment chart...")

  plot_data <- iea_data %>%
    group_by(year, technology) %>%
    summarise(investment = sum(investment_usd_billions, na.rm = TRUE), .groups = "drop") %>%
    arrange(year, desc(investment))

  top_techs <- plot_data %>%
    group_by(technology) %>%
    summarise(total = sum(investment)) %>%
    arrange(desc(total)) %>%
    head(6) %>%
    pull(technology)

  plot_data <- plot_data %>%
    filter(technology %in% top_techs)

  colors <- get_color_palette(length(top_techs), "categorical")

  p <- plot_ly(plot_data, x = ~year, y = ~investment, color = ~technology,
               type = "scatter", mode = "lines+markers",
               colors = colors,
               line = list(width = 3),
               marker = list(size = 8),
               text = ~paste0("<b>", technology, "</b><br>Investment: ", format_billions(investment)),
               hovertemplate = "%{text}<extra></extra>") %>%
    layout(
      title = list(text = "<b>Renewable Energy Investment by Technology</b><br><sub>Top 6 technologies by total investment</sub>", font = list(size = 18)),
      xaxis = list(title = "<b>Year</b>"),
      yaxis = list(title = "<b>Investment (USD Billions)</b>"),
      hovermode = "x unified",
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      legend = list(title = list(text = "<b>Technology</b>"), x = 0.5, xanchor = "center", y = -0.2, orientation = "h"),
      margin = list(b = 100)
    )

  return(p)
}

# Generate All Visualizations ----
generate_all_visualizations <- function(data_file = "data/processed/processed_climate_finance.rds") {
  message("\n╔══════════════════════════════════════════════════╗")
  message("║  Generating Beautiful Visualizations            ║")
  message("╚══════════════════════════════════════════════════╝\n")

  if (!file.exists(data_file)) {
    stop("❌ Processed data not found. Please run data_processing.R first.")
  }

  data <- readRDS(data_file)

  viz_list <- list()

  viz_list$green_brown_comparison <- create_green_brown_comparison(data$green_brown)
  message("✓ Green vs Brown comparison created\n")

  viz_list$investment_area <- create_investment_area_chart(data$green_brown)
  message("✓ Investment area chart created\n")

  viz_list$commitment_gap <- create_commitment_gap_chart(data$commitment_gap)
  message("✓ Commitment gap chart created\n")

  viz_list$cumulative_gap <- create_cumulative_gap_chart(data$commitment_gap)
  message("✓ Cumulative gap chart created\n")

  viz_list$yoy_growth <- create_yoy_growth_chart(data$yoy_growth)
  message("✓ Year-over-year growth chart created\n")

  viz_list$flow_sankey <- create_flow_sankey(data$cross_country_flows)
  message("✓ Sankey flow diagram created\n")

  viz_list$green_share_map <- create_green_share_map(data$geo)
  message("✓ Green share map created\n")

  viz_list$sector_sunburst <- create_sector_sunburst(data$green_brown, data$raw_finance)
  message("✓ Sector sunburst created\n")

  viz_list$top_countries <- create_top_countries_comparison(data$green_brown)
  message("✓ Top countries comparison created\n")

  viz_list$policy_timeline <- create_policy_timeline_chart(data$policies, data$policy_impacts)
  message("✓ Policy timeline created\n")

  viz_list$instrument_heatmap <- create_instrument_heatmap(data$commitment_gap)
  message("✓ Instrument heatmap created\n")

  viz_list$renewable_tech <- create_renewable_tech_chart(data$raw_iea)
  message("✓ Renewable technology chart created\n")

  # Save visualizations
  output_file <- "data/processed/visualizations.rds"
  saveRDS(viz_list, output_file)

  message("\n╔══════════════════════════════════════════════════╗")
  message("║  ✓ All visualizations generated successfully!   ║")
  message("╚══════════════════════════════════════════════════╝")
  message(glue::glue("  📁 Output: {output_file}"))
  message(glue::glue("  📊 Total visualizations: {length(viz_list)}"))
  message("\n  ➜ Next: Run the Shiny app with 'shiny::runApp(\"app\")'\n")

  return(viz_list)
}

# Run if executed directly
if (sys.nframe() == 0) {
  visualizations <- generate_all_visualizations()
  message("\n✓ Visualizations complete!")
}
