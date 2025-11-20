# Server logic for Green Finance Dashboard

server <- function(input, output, session) {

  # Overview Tab Outputs ----

  output$green_brown_plot <- renderPlotly({
    if (!is.null(visualizations) && !is.null(visualizations$green_brown_comparison)) {
      visualizations$green_brown_comparison
    } else {
      create_green_brown_comparison(processed_data$green_brown)
    }
  })

  output$investment_area_plot <- renderPlotly({
    if (!is.null(visualizations) && !is.null(visualizations$investment_area)) {
      visualizations$investment_area
    } else {
      create_investment_area_chart(processed_data$green_brown)
    }
  })

  output$top_countries_plot <- renderPlotly({
    if (!is.null(visualizations) && !is.null(visualizations$top_countries)) {
      visualizations$top_countries
    } else {
      create_top_countries_comparison(processed_data$green_brown)
    }
  })

  # Commitment Gap Tab Outputs ----

  output$commitment_gap_plot <- renderPlotly({
    if (!is.null(visualizations) && !is.null(visualizations$commitment_gap)) {
      visualizations$commitment_gap
    } else {
      create_commitment_gap_chart(processed_data$commitment_gap)
    }
  })

  output$cumulative_gap_plot <- renderPlotly({
    if (!is.null(visualizations) && !is.null(visualizations$cumulative_gap)) {
      visualizations$cumulative_gap
    } else {
      create_cumulative_gap_chart(processed_data$commitment_gap)
    }
  })

  output$instrument_heatmap_plot <- renderPlotly({
    if (!is.null(visualizations) && !is.null(visualizations$instrument_heatmap)) {
      visualizations$instrument_heatmap
    } else {
      create_instrument_heatmap(processed_data$commitment_gap)
    }
  })

  # Growth Analysis Tab Outputs ----

  output$yoy_growth_plot <- renderPlotly({
    if (!is.null(visualizations) && !is.null(visualizations$yoy_growth)) {
      visualizations$yoy_growth
    } else {
      create_yoy_growth_chart(processed_data$yoy_growth)
    }
  })

  output$renewable_tech_plot <- renderPlotly({
    if (!is.null(visualizations) && !is.null(visualizations$renewable_tech)) {
      visualizations$renewable_tech
    } else {
      create_renewable_tech_chart(processed_data$raw_iea)
    }
  })

  # Geographic Flows Tab Outputs ----

  output$flow_sankey_plot <- renderPlotly({
    if (!is.null(visualizations) && !is.null(visualizations$flow_sankey)) {
      visualizations$flow_sankey
    } else {
      create_flow_sankey(processed_data$cross_country_flows)
    }
  })

  output$green_share_map_output <- renderUI({
    if ("sf" %in% class(processed_data$geo)) {
      # Leaflet map
      tagList(
        leafletOutput("green_share_map", height = "600px")
      )
    } else {
      # Plotly fallback
      tagList(
        plotlyOutput("green_share_plot", height = "600px")
      )
    }
  })

  output$green_share_map <- renderLeaflet({
    if (!is.null(visualizations) && !is.null(visualizations$green_share_map)) {
      if ("leaflet" %in% class(visualizations$green_share_map)) {
        visualizations$green_share_map
      } else {
        NULL
      }
    } else {
      if ("sf" %in% class(processed_data$geo)) {
        create_green_share_map(processed_data$geo)
      } else {
        NULL
      }
    }
  })

  output$green_share_plot <- renderPlotly({
    if (!is.null(visualizations) && !is.null(visualizations$green_share_map)) {
      if ("plotly" %in% class(visualizations$green_share_map)) {
        visualizations$green_share_map
      } else {
        NULL
      }
    } else {
      if (!"sf" %in% class(processed_data$geo)) {
        create_green_share_map(processed_data$geo)
      } else {
        NULL
      }
    }
  })

  # Sector Analysis Tab Outputs ----

  output$sector_sunburst_plot <- renderPlotly({
    if (!is.null(visualizations) && !is.null(visualizations$sector_sunburst)) {
      visualizations$sector_sunburst
    } else {
      create_sector_sunburst(processed_data$green_brown, processed_data$raw_finance)
    }
  })

  # Policy Impact Tab Outputs ----

  output$policy_timeline_plot <- renderPlotly({
    if (!is.null(visualizations) && !is.null(visualizations$policy_timeline)) {
      visualizations$policy_timeline
    } else {
      create_policy_timeline_chart(processed_data$policies, processed_data$policy_impacts)
    }
  })

  output$policy_table <- renderDT({
    processed_data$policies %>%
      select(Date = date, Event = event, Category = category,
             Description = description, `Impact Score` = impact_score,
             `Countries Involved` = countries_involved) %>%
      datatable(
        options = list(
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        class = 'table table-striped table-hover',
        filter = 'top'
      ) %>%
      formatDate('Date', method = 'toLocaleDateString')
  })

  # Data Explorer Tab ----

  # Filtered data reactive
  filtered_data <- reactive({
    data <- processed_data$raw_finance

    if (input$filter_year != "All") {
      data <- data %>% filter(year == as.numeric(input$filter_year))
    }

    if (input$filter_investment_type != "All") {
      data <- data %>% filter(investment_type == input$filter_investment_type)
    }

    if (input$filter_country != "All") {
      data <- data %>% filter(country == input$filter_country)
    }

    data %>%
      select(
        Year = year,
        Country = country,
        `Investment Type` = investment_type,
        Sector = sector,
        Instrument = instrument,
        `Finance Type` = finance_type,
        `Commitment (USD B)` = commitment_usd_billions,
        `Disbursement (USD B)` = disbursement_usd_billions,
        `Climate Impact Score` = climate_impact_score,
        `Flow Type` = flow_type,
        `Project Status` = project_status
      )
  })

  output$data_table <- renderDT({
    filtered_data() %>%
      datatable(
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "500px",
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf'),
          autoWidth = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = c(0, 1, 2, 3, 4, 5, 9, 10))
          )
        ),
        rownames = FALSE,
        class = 'table table-striped table-hover compact',
        filter = 'top',
        extensions = 'Buttons'
      ) %>%
      formatRound(columns = c('Commitment (USD B)', 'Disbursement (USD B)'), digits = 2) %>%
      formatRound(columns = 'Climate Impact Score', digits = 1)
  })

  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("climate_finance_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )

  # Session info
  session$onSessionEnded(function() {
    message("Dashboard session ended")
  })
}
