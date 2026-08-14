# Two-page navbar: the Home dashboard and an About page. Content is wrapped in a
# horizontally padded container so cards don't sit flush against the viewport
# edges. fillable = FALSE keeps the long dashboard scrolling normally (rather
# than every card stretching to fill the viewport).
page_navbar(
  title = "Variant Reviewer",
  # Apply branding from _brand.yml (colors, fonts). brand = TRUE requires the
  # file to exist; switch to bslib::bs_theme() to make it optional.
  #
  # bslib defaults card-bg to the page's own body-bg, so with the brand's
  # "paper" background a card is the exact same color as the page behind it.
  # Lifting cards to white gives them a visible edge against the page; the
  # card-header wash in app.css (a light tint of the brand primary) still
  # separates header from body on top of that.
  theme = bslib::bs_theme(brand = TRUE) |>
    bslib::bs_add_variables("card-bg" = "#ffffff"),
  # Charcoal navbar (see _brand.yml's `charcoal`) so it reads as a distinct
  # band above the page rather than blending into the "paper" background;
  # theme = "dark" switches the nav text/icons to light for contrast on it.
  navbar_options = bslib::navbar_options(bg = "#2c2a25", theme = "dark"),
  fillable = FALSE,
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css"),
    # Attach cicerone's assets when installed; the Demo button drives a guided
    # walkthrough with it (see R/demo_tour.R and the demo handler in server.R).
    if (requireNamespace("cicerone", quietly = TRUE)) cicerone::use_cicerone()
  ),
  nav_panel(
    title = "Home",
    icon = icon("dna"),
    div(
      class = "px-2 px-lg-4 py-3",
      # Results (width 9) on the left scroll with the page; the chat (width 3)
      # on the right stays pinned in view (see .vr-chat-col in app.css).
      layout_columns(
        # vr-home-grid pins the assistant to a fixed-width track on wide screens
        # (see app.css); col_widths still governs narrower ones.
        class = "vr-home-grid",
        col_widths = c(9, 3),
        div(
          tags$p(
            class = "text-muted",
            "A lightweight gene and variant interpretation companion."
          ),
          dashboard_page
        ),
        div(id = "tour_chat", class = "vr-chat-col", chat_panel)
      )
    )
  ),
  nav_panel(
    title = "About",
    icon = icon("circle-info"),
    div(class = "px-2 px-lg-4 py-3", about_page)
  ),
  # Right-aligned demo button: loads a worked example into the dashboard and
  # explains how to use the app and the assistant (wired in server.R).
  #
  # btn-outline-light, not -primary: the navbar is dark now, and an
  # outline-primary button's green border/text has too little contrast against
  # charcoal at rest (it only stood out on hover, when the fill kicks in).
  # outline-light matches the rest of the nav's light-on-dark text.
  nav_spacer(),
  nav_item(
    actionButton(
      "demo",
      "Demo",
      icon = icon("wand-magic-sparkles"),
      class = "btn-sm btn-outline-light"
    )
  ),
  footer = tags$footer(
    class = "border-top text-center text-muted small py-3 px-2",
    sprintf("Variant Reviewer v%s", app_version()),
    " · Built by Samuel Bharti · ",
    tags$a(
      href = "https://github.com/samuelbharti/variant-reviewer",
      target = "_blank",
      rel = "noopener",
      "Source"
    ),
    tags$span(class = "mx-1", "·"),
    tags$a(
      href = "https://github.com/samuelbharti/variant-reviewer/blob/main/LICENSE",
      target = "_blank",
      rel = "noopener",
      "MIT License"
    )
  )
)
