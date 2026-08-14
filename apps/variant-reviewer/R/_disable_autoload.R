# Turn off Shiny's automatic sourcing of R/.
#
# Shiny autoloads every file in R/ before the app entry point. That is wrong for
# this app in two ways:
#
#   * Order. R/load_components.R does not just define helpers, it sources
#     modules/ and userInterface/, and those build UI objects at source time
#     using bslib. Autoload runs before app.R, so global.R has not attached
#     bslib yet and sourcing dies on "could not find function layout_columns".
#
#   * Duplication. global.R already sources R/ deliberately, via
#     load_components.R, so autoload was sourcing every one of those files a
#     second time.
#
# The file's presence is the switch; its contents are never used.
