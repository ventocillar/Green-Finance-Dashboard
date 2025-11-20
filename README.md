# Green Finance Flows Visualization Project

## Overview
Interactive visualization dashboard and scrollytelling report showcasing global green finance flows and analyzing their effectiveness in achieving climate objectives.

## Features

### 1. Shiny Dashboard
- **Interactive Maps**: Choropleth maps showing climate finance inflows/outflows
- **Financial Instruments**: Stacked bar charts and sunburst visualizations
- **Policy Timeline**: Interactive timeline linking policies to finance trends
- **Data Explorer**: Browse and download processed data

### 2. Quarto Scrollytelling Report
- **Narrative-driven**: Story-based exploration of climate finance
- **Scroll-triggered animations**: Content reveals as you scroll
- **Interactive visualizations**: Embedded Plotly and Leaflet charts
- **Progress tracking**: Visual progress bar and section indicators

## Installation

### Prerequisites
- R (version 4.0 or higher)
- RStudio (recommended)
- Quarto (for the scrollytelling report)

### Setup Instructions

1. **Clone or download the project**
```bash
cd green-finance-viz
```

2. **Install required packages**
```r
source("setup.R")
```

3. **Run data pipeline**
```r
# Fetch data from sources
source("R/data_acquisition.R")

# Process and clean data
source("R/data_processing.R")

# Generate visualizations
source("R/visualizations.R")
```

## Usage

### Running the Shiny Dashboard
```r
shiny::runApp("app")
```
The dashboard will open in your browser at `http://localhost:XXXX`

### Generating the Quarto Report
```bash
# From command line
quarto render reports/climate-finance-story.qmd

# Or from R
quarto::quarto_render("reports/climate-finance-story.qmd")
```
The report will be generated as `climate-finance-story.html`

## Project Structure
```
green-finance-viz/
├── app/                    # Shiny dashboard
│   ├── global.R           # Global settings
│   ├── ui.R              # User interface
│   └── server.R          # Server logic
├── R/                     # Core R scripts
│   ├── data_acquisition.R # Data fetching
│   ├── data_processing.R  # Data processing
│   ├── utils.R           # Utility functions
│   └── visualizations.R  # Visualization functions
├── reports/              # Quarto reports
│   ├── climate-finance-story.qmd
│   ├── scrollytelling.css
│   └── scrollytelling.js
├── data/                 # Data storage
│   ├── raw/             # Original data
│   ├── processed/       # Processed data
│   └── cache/          # Cached API responses
└── setup.R              # Package installation

```

## Data Sources
- **Climate Policy Initiative (CPI)**: Global Landscape of Climate Finance
- **OECD**: Green Growth Indicators
- **IEA**: Renewable Energy Investment Data

## Key Visualizations

### 1. Geographic Flows
- Choropleth maps with country-level finance data
- Flow arrows showing international transfers
- Regional aggregations and comparisons

### 2. Financing Instruments
- Stacked bar charts by year
- Hierarchical sunburst charts
- Instrument effectiveness analysis

### 3. Policy Timeline
- Interactive timeline with policy milestones
- Correlation analysis with finance trends
- Impact assessment metrics

## Customization

### Updating Data
To refresh with latest data:
```r
source("R/data_acquisition.R")
data <- fetch_all_data(force_refresh = TRUE)
```

### Modifying Visualizations
Edit `R/visualizations.R` to customize:
- Color schemes
- Chart types
- Aggregation levels

### Dashboard Themes
Modify `app/ui.R`:
```r
dashboardPage(
  skin = "green",  # Options: "blue", "black", "purple", "green", "red", "yellow"
  ...
)
```

## Troubleshooting

### Common Issues

1. **Package installation fails**
   - Ensure you have write permissions
   - Try installing packages individually
   - Check CRAN mirror settings

2. **Data not loading**
   - Run data acquisition scripts first
   - Check internet connection for API access
   - Verify data/cache directory exists

3. **Shiny app won't start**
   - Ensure all packages are installed
   - Check for port conflicts
   - Verify data files exist

4. **Quarto report errors**
   - Install/update Quarto
   - Check all data dependencies
   - Verify JavaScript/CSS files are present

## Contributing
Contributions welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Submit a pull request

## License
MIT License - See LICENSE file for details

## Contact
For questions or support, please open an issue on GitHub.

## Acknowledgments
- Climate Policy Initiative for financial data
- OECD for green growth indicators
- IEA for renewable investment data

---
*Last updated: 2024*
