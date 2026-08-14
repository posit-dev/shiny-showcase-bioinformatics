navbarPage(
  # The mascot + wordmark lockup in the navbar brand.
  title = tagList(
    genescout_mascot(28, class = "gs-brand-mark", alt = ""),
    tags$span(class = "gs-brand-name", "GeneScout")
  ),
  windowTitle = "GeneScout",
  collapsible = TRUE,
  # Branding (colors, fonts) from _brand.yml — a single warm, organic maroon
  # identity that every page follows. brand = TRUE requires the file to exist.
  theme = bslib::bs_theme(brand = TRUE),
  # A shared footer (version, copyright, license, GitHub) under every tab.
  footer = genescout_footer(),
  # Review and Chat are the functional pages. The reference/explainer pages are
  # grouped under a "Docs" dropdown; About stays top-level (it is the app's front
  # door, not a doc).
  tabPanel("Review", review_page),
  tabPanel("Chat", chat_page),
  navbarMenu(
    "Docs",
    tabPanel("Reading results", guide_page),
    tabPanel("Connectors", connectors_page),
    tabPanel("AI Agents", agents_page)
  ),
  tabPanel("About", about_page)
)
