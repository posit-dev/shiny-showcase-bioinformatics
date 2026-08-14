# app.R: single-file entrypoint.
#
# GeneScout's canonical app definition lives in global.R / ui.R / server.R. Some
# deployment targets (for example certain Posit Connect and shinyapps.io
# workflows) expect a single-file app.R, so this shim reuses those files verbatim:
# one source of truth, no duplicated app definition. Local shiny::runApp() uses
# this file too, and behaves identically to the multi-file layout.
#
# In the multi-file layout Shiny sources global.R automatically; with app.R present
# it does not, so we source it here (into the global environment, exactly where it
# runs normally), then build the ui and server objects from their own files.

source("global.R")

ui <- source("ui.R", local = TRUE)$value
server <- source("server.R", local = TRUE)$value

shinyApp(ui = ui, server = server)
