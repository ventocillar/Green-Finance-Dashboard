# 🌱 Green Finance Dashboard

A comprehensive, professional dashboard for analyzing global climate finance flows, tracking green vs brown investments, commitment gaps, and policy impacts.

![Dashboard](https://img.shields.io/badge/Status-Production%20Ready-success)
![R](https://img.shields.io/badge/R-4.0%2B-blue)
![License](https://img.shields.io/badge/License-MIT-green)

## ✨ Features

### 📊 Comprehensive Analytics
- **Green vs Brown Investment Tracking**: Real-time comparison of sustainable vs fossil fuel investments
- **Commitment-Disbursement Gap Analysis**: Track the delivery of climate finance promises
- **Year-over-Year Growth Metrics**: Analyze investment trends and growth rates
- **Cross-Country Finance Flows**: Sankey diagrams showing international climate finance
- **Sector & Instrument Analysis**: Detailed breakdown by financing mechanisms
- **Policy Impact Correlation**: Link major climate policies to finance trends

### 🎨 Beautiful Visualizations
- Interactive Plotly charts with modern aesthetics
- Responsive Leaflet maps for geographic analysis
- Sankey diagrams for flow visualization
- Sunburst charts for hierarchical data
- Heatmaps for performance tracking
- Professional color schemes (green-focused palette)

### 💼 Professional Design
- Modern UI built with `bslib` (Bootstrap 5)
- Responsive layout for all screen sizes
- Interactive data tables with filtering and export
- Real-time KPI cards with trend indicators
- Smooth animations and transitions
- Custom CSS for polished appearance

## 🚀 Quick Start

### Prerequisites
- R version 4.0 or higher
- RStudio (recommended)

### Installation

1. **Clone the repository**
```bash
cd Green-Finance-Dashboard
```

2. **Install required packages**
```r
source("setup.R")
```

This will automatically install all dependencies including:
- tidyverse, plotly, leaflet, shiny, bslib, DT
- sf, rnaturalearth (for spatial analysis)
- httr, jsonlite (for data acquisition)
- And more...

3. **Generate data and visualizations**
```r
# Step 1: Fetch climate finance data
source("R/data_acquisition.R")

# Step 2: Process and analyze data
source("R/data_processing.R")

# Step 3: Create visualizations
source("R/visualizations.R")
```

4. **Launch the dashboard**
```r
shiny::runApp("app")
```

The dashboard will open in your default browser at `http://localhost:XXXX`

## 📁 Project Structure

```
Green-Finance-Dashboard/
├── app/                          # Shiny dashboard application
│   ├── app.R                    # Main app file
│   ├── global.R                 # Global settings and data loading
│   ├── ui.R                     # User interface definition
│   └── server.R                 # Server logic
│
├── R/                           # Core R scripts
│   ├── utils.R                  # Utility functions and helpers
│   ├── data_acquisition.R       # Data fetching and generation
│   ├── data_processing.R        # Data analysis and transformation
│   └── visualizations.R         # Chart and map creation functions
│
├── data/                        # Data storage (created on first run)
│   ├── raw/                     # Original data files
│   ├── processed/               # Processed datasets
│   │   ├── all_climate_finance_data.rds
│   │   ├── processed_climate_finance.rds
│   │   └── visualizations.rds
│   └── cache/                   # Cached API responses
│
├── reports/                     # Reports and documentation
│
├── setup.R                      # Package installation script
├── README.md                    # This file
└── data-gathering              # Legacy files (for reference)
    data-processing
    visualizations
```

## 📊 Dashboard Tabs

### 1. **Overview**
- Key performance indicators (KPIs)
- Green vs Brown investment trends
- Investment composition over time
- Top countries by climate finance

### 2. **Commitment & Delivery**
- Commitment vs disbursement gap analysis
- Cumulative finance tracking
- Instrument performance heatmap

### 3. **Growth & Trends**
- Year-over-year growth by investment type
- Renewable energy investment by technology
- Growth rate comparisons

### 4. **Geographic Flows**
- International finance flow network (Sankey diagram)
- Global green investment distribution map
- Regional analysis

### 5. **Sector Analysis**
- Hierarchical sector breakdown (sunburst chart)
- Investment by sector and instrument
- Sector performance metrics

### 6. **Policy Impact**
- Policy timeline with finance correlations
- Major climate policy events
- Impact assessment

### 7. **Data Explorer**
- Interactive data table with filters
- Export capabilities (CSV, Excel, PDF)
- Advanced search and filtering

### 8. **About**
- Dashboard information
- Data sources and methodology
- Statistics and credits

## 📈 Key Improvements Over Previous Version

### Data Enhancements
✅ **Green vs Brown Classification**: All investments categorized by sustainability
✅ **Commitment Tracking**: Separate tracking of commitments vs disbursements
✅ **Enhanced Metrics**: Year-over-year growth, CAGR, cumulative gaps
✅ **Cross-Country Flows**: Detailed donor-recipient mapping
✅ **Climate Impact Scores**: Quantitative assessment of investment impact

### Visualization Improvements
✅ **12 Professional Charts**: Up from 6 basic visualizations
✅ **Sankey Diagrams**: Beautiful flow visualizations
✅ **Interactive Maps**: Leaflet choropleth with tooltips
✅ **Modern Color Schemes**: Professional green-focused palette
✅ **Responsive Design**: Works on all screen sizes
✅ **Better Interactivity**: Hover details, zooming, filtering

### UI/UX Enhancements
✅ **Modern Bootstrap 5 Theme**: Using bslib for contemporary design
✅ **KPI Cards**: Real-time metrics with trend indicators
✅ **Smooth Animations**: Loading spinners and transitions
✅ **Better Navigation**: Clear tab structure with icons
✅ **Professional Typography**: Google Fonts (Inter)
✅ **Custom CSS**: Polished, cohesive appearance

## 🎨 Color Palette

The dashboard uses a carefully selected color scheme:

- **Primary Green**: `#2E7D32` (sustainable investments)
- **Primary Blue**: `#1976D2` (public finance)
- **Brown**: `#5D4037` (fossil fuel investments)
- **Orange**: `#F57C00` (transition investments)
- **Purple**: `#7B1FA2` (private finance)

## 📊 Data Sources

This dashboard integrates data from multiple authoritative sources:

1. **Climate Policy Initiative (CPI)**
   - Global Landscape of Climate Finance
   - Investment flows by country and sector

2. **OECD**
   - Green Growth Indicators
   - Climate Finance Flows (DAC)
   - Carbon pricing data

3. **International Energy Agency (IEA)**
   - Renewable Energy Investment
   - Technology-specific data
   - LCOE trends

4. **International Climate Policy Database**
   - Major policy events and milestones
   - Policy implementation tracking

## 🔬 Methodology

### Investment Classification

**Green Investments** include:
- Solar, wind, hydro, and other renewables
- Energy efficiency projects
- Clean transportation
- Sustainable agriculture
- Conservation and biodiversity

**Brown Investments** include:
- Coal infrastructure
- Oil & gas projects
- Fossil fuel subsidies
- Conventional heavy industry

**Transition Investments** include:
- Natural gas (as bridge fuel)
- Nuclear energy
- Industrial efficiency improvements
- Hybrid technologies

### Metrics Calculated

- **Commitment-Disbursement Gap**: Difference between promised and delivered funds
- **Year-over-Year Growth**: Annual percentage change in investments
- **CAGR**: Compound Annual Growth Rate over the data period
- **Green Share**: Percentage of total investments in green projects
- **Disbursement Rate**: Percentage of commitments actually disbursed
- **Climate Impact Score**: 0-100 scale based on emission reductions

## 🛠️ Customization

### Modifying Color Schemes

Edit `R/utils.R`:
```r
get_investment_colors <- function() {
  list(
    green = "#YOUR_COLOR",
    brown = "#YOUR_COLOR",
    ...
  )
}
```

### Adding New Visualizations

1. Create function in `R/visualizations.R`
2. Add output in `app/server.R`
3. Add UI element in `app/ui.R`

### Updating Data Sources

Modify `R/data_acquisition.R` to add new data sources or update API endpoints.

## 📱 Screenshots

*(Screenshots would go here in a real deployment)*

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

### Development Guidelines
- Follow tidyverse style guide
- Comment complex logic
- Test visualizations with different data
- Ensure responsive design

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🙏 Acknowledgments

- Climate Policy Initiative for financial data frameworks
- OECD for green growth indicators
- IEA for renewable investment data
- The R community for amazing packages

## 📞 Support

For questions or issues:
- Open an issue on GitHub
- Check the documentation
- Review the code comments

## 🔮 Future Enhancements

Planned features:
- [ ] Real-time data API integration
- [ ] Machine learning predictions
- [ ] Comparative scenario analysis
- [ ] PDF report generation
- [ ] Mobile app version
- [ ] Multi-language support

---

**Last Updated**: 2024
**Version**: 2.0
**Status**: Production Ready

Made with ❤️ for climate action
