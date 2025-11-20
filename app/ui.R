# UI for Green Finance Dashboard
# Modern, professional interface with comprehensive visualizations

ui <- page_navbar(
  title = "🌱 Green Finance Dashboard",
  theme = app_theme,
  header = tags$head(custom_css),
  fillable = TRUE,
  window_title = "Green Finance Dashboard",

  # Overview Tab ----
  nav_panel(
    title = "Overview",
    icon = icon("chart-line"),

    layout_column_wrap(
      width = 1/4,
      height = "150px",
      create_value_box_html(
        value = kpis$total_commitment,
        title = "Total Climate Finance",
        icon = "💰",
        change = kpis$commitment_change,
        change_positive = TRUE
      ),
      create_value_box_html(
        value = kpis$green_investment,
        title = "Green Investment",
        icon = "🌱",
        change = kpis$green_change,
        change_positive = TRUE
      ),
      create_value_box_html(
        value = kpis$disbursement_rate,
        title = "Disbursement Rate",
        icon = "📊",
        change = NULL
      ),
      create_value_box_html(
        value = kpis$n_countries,
        title = "Countries",
        icon = "🌍",
        change = NULL
      )
    ),

    br(),

    layout_column_wrap(
      width = 1/2,
      height = "500px",

      card(
        card_header("Green vs Brown Investment Trends"),
        card_body(
          withSpinner(
            plotlyOutput("green_brown_plot", height = "400px"),
            type = 6,
            color = "#2E7D32"
          )
        )
      ),

      card(
        card_header("Investment Composition Over Time"),
        card_body(
          withSpinner(
            plotlyOutput("investment_area_plot", height = "400px"),
            type = 6,
            color = "#2E7D32"
          )
        )
      )
    ),

    br(),

    card(
      card_header("Top Countries by Climate Finance"),
      card_body(
        withSpinner(
          plotlyOutput("top_countries_plot", height = "500px"),
          type = 6,
          color = "#2E7D32"
        )
      )
    )
  ),

  # Commitment Gap Tab ----
  nav_panel(
    title = "Commitment & Delivery",
    icon = icon("handshake"),

    div(
      class = "section-header",
      h4("Tracking Climate Finance Promises vs Delivery")
    ),

    layout_column_wrap(
      width = 1/2,
      height = "500px",

      card(
        card_header("Commitment vs Disbursement Gap"),
        card_body(
          withSpinner(
            plotlyOutput("commitment_gap_plot", height = "400px"),
            type = 6,
            color = "#2E7D32"
          )
        )
      ),

      card(
        card_header("Cumulative Finance Tracking"),
        card_body(
          withSpinner(
            plotlyOutput("cumulative_gap_plot", height = "400px"),
            type = 6,
            color = "#2E7D32"
          )
        )
      )
    ),

    br(),

    card(
      card_header("Instrument Performance Heatmap"),
      card_body(
        withSpinner(
          plotlyOutput("instrument_heatmap_plot", height = "500px"),
          type = 6,
          color = "#2E7D32"
        )
      )
    )
  ),

  # Growth Analysis Tab ----
  nav_panel(
    title = "Growth & Trends",
    icon = icon("chart-area"),

    div(
      class = "section-header",
      h4("Year-over-Year Growth Analysis")
    ),

    card(
      card_header("Year-over-Year Growth by Investment Type"),
      card_body(
        withSpinner(
          plotlyOutput("yoy_growth_plot", height = "500px"),
          type = 6,
          color = "#2E7D32"
        )
      )
    ),

    br(),

    card(
      card_header("Renewable Energy Investment by Technology"),
      card_body(
        withSpinner(
          plotlyOutput("renewable_tech_plot", height = "500px"),
          type = 6,
          color = "#2E7D32"
        )
      )
    )
  ),

  # Geographic Flows Tab ----
  nav_panel(
    title = "Geographic Flows",
    icon = icon("globe"),

    div(
      class = "section-header",
      h4("Cross-Country Climate Finance Flows")
    ),

    card(
      card_header("International Finance Flow Network"),
      card_body(
        withSpinner(
          plotlyOutput("flow_sankey_plot", height = "600px"),
          type = 6,
          color = "#2E7D32"
        )
      )
    ),

    br(),

    card(
      card_header("Global Green Investment Distribution"),
      card_body(
        withSpinner(
          uiOutput("green_share_map_output"),
          type = 6,
          color = "#2E7D32"
        )
      )
    )
  ),

  # Sector Analysis Tab ----
  nav_panel(
    title = "Sector Analysis",
    icon = icon("industry"),

    div(
      class = "section-header",
      h4("Investment Breakdown by Sector & Instrument")
    ),

    card(
      card_header("Hierarchical Sector Breakdown"),
      card_body(
        withSpinner(
          plotlyOutput("sector_sunburst_plot", height = "650px"),
          type = 6,
          color = "#2E7D32"
        )
      )
    )
  ),

  # Policy Impact Tab ----
  nav_panel(
    title = "Policy Impact",
    icon = icon("landmark"),

    div(
      class = "section-header",
      h4("Climate Policy Events & Finance Correlation")
    ),

    card(
      card_header("Policy Timeline & Finance Trends"),
      card_body(
        withSpinner(
          plotlyOutput("policy_timeline_plot", height = "600px"),
          type = 6,
          color = "#2E7D32"
        )
      )
    ),

    br(),

    card(
      card_header("Policy Events Database"),
      card_body(
        DTOutput("policy_table")
      )
    )
  ),

  # Data Explorer Tab ----
  nav_panel(
    title = "Data Explorer",
    icon = icon("table"),

    div(
      class = "section-header",
      h4("Browse & Export Climate Finance Data")
    ),

    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h5("Filters"),
        selectInput(
          "filter_year",
          "Year",
          choices = c("All", sort(unique(processed_data$raw_finance$year), decreasing = TRUE)),
          selected = "All"
        ),
        selectInput(
          "filter_investment_type",
          "Investment Type",
          choices = c("All", unique(processed_data$raw_finance$investment_type)),
          selected = "All"
        ),
        selectInput(
          "filter_country",
          "Country",
          choices = c("All", sort(unique(processed_data$raw_finance$country))),
          selected = "All"
        ),
        hr(),
        downloadButton("download_data", "Download Filtered Data", class = "btn-success w-100")
      ),

      card(
        card_header("Climate Finance Data"),
        card_body(
          DTOutput("data_table")
        )
      )
    )
  ),

  # About Tab ----
  nav_panel(
    title = "About",
    icon = icon("info-circle"),

    layout_column_wrap(
      width = 1,

      card(
        card_header("About This Dashboard"),
        card_body(
          h5("🌱 Green Finance Dashboard"),
          p("This interactive dashboard provides comprehensive analysis of global climate finance flows,
            tracking investments in green and brown energy, commitment vs disbursement gaps, and the impact
            of climate policies on finance flows."),

          h6("Key Features:", class = "mt-4"),
          tags$ul(
            tags$li("Real-time tracking of green vs brown investments"),
            tags$li("Commitment-disbursement gap analysis"),
            tags$li("Year-over-year growth metrics"),
            tags$li("Cross-country finance flow visualization"),
            tags$li("Sector and instrument performance analysis"),
            tags$li("Policy impact correlation"),
            tags$li("Interactive data explorer with export capabilities")
          ),

          h6("Data Sources:", class = "mt-4"),
          tags$ul(
            tags$li("Climate Policy Initiative (CPI) - Global Climate Finance"),
            tags$li("OECD - Green Growth Indicators & Climate Finance Flows"),
            tags$li("IEA - Renewable Energy Investment Data"),
            tags$li("International Climate Policy Database")
          ),

          h6("Methodology:", class = "mt-4"),
          p("Investment classification follows international standards:"),
          tags$ul(
            tags$li(tags$b("Green:"), " Renewable energy, energy efficiency, sustainable transport, conservation"),
            tags$li(tags$b("Brown:"), " Fossil fuels, coal, oil & gas infrastructure"),
            tags$li(tags$b("Transition:"), " Natural gas, nuclear, hybrid technologies")
          )
        )
      ),

      card(
        card_header("Dashboard Statistics"),
        card_body(
          layout_column_wrap(
            width = 1/3,
            value_box(
              title = "Total Data Points",
              value = format(nrow(processed_data$raw_finance), big.mark = ","),
              showcase = icon("database"),
              theme = "success"
            ),
            value_box(
              title = "Countries Covered",
              value = length(unique(processed_data$raw_finance$country_code)),
              showcase = icon("flag"),
              theme = "primary"
            ),
            value_box(
              title = "Years of Data",
              value = paste(min(processed_data$raw_finance$year), "-",
                          max(processed_data$raw_finance$year)),
              showcase = icon("calendar"),
              theme = "info"
            )
          )
        )
      )
    )
  ),

  # Footer ----
  nav_spacer(),

  nav_item(
    tags$a(
      icon("github"),
      "Source",
      href = "https://github.com",
      target = "_blank",
      class = "nav-link"
    )
  )
)
