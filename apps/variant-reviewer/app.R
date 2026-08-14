# Single entry point, used by Posit Connect Cloud (which asks for a primary
# file, and expects app.R) and by shiny::runApp() locally.
#
# The app is still written in three parts: global.R for setup, app_ui.R for the
# page, app_server.R for the server. Two things to know about why they are wired
# up by hand here rather than left to Shiny:
#
#   * Shiny auto-loads global.R only when there is NO app.R. Once this file
#     exists it becomes the entry point and global.R is skipped, so it is
#     sourced explicitly. Without that line there are no libraries, no modules
#     from R/load_components.R and no theming, and the app dies on first use.
#
#   * The UI and server files are deliberately NOT called ui.R and server.R. A
#     directory holding app.R alongside ui.R/server.R is ambiguous: Shiny picks
#     app.R, but the tooling objects, and shinytest2 refuses to start such a
#     directory at all, which takes the end-to-end tests with it.
source("global.R")

# Each file evaluates to a value: the UI object, and the server function.
# local = TRUE keeps this environment as their parent, so they still see
# everything global.R placed in the global environment.
ui <- source("app_ui.R", local = TRUE)$value
server <- source("app_server.R", local = TRUE)$value

shinyApp(ui, server)
