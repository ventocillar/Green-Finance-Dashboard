# Green Finance Dashboard - Main App File
# Modern, comprehensive dashboard for climate finance analysis

# Load global settings and dependencies
source("global.R", local = TRUE)

# Load UI
source("ui.R", local = TRUE)

# Load server logic
source("server.R", local = TRUE)

# Run app
shinyApp(ui = ui, server = server)
