# --- Theme: "Cream & Crimson" warm Sass pass ----------------------------------
# The palette lives in _brand.yml and reaches this Sass as `$brand-<name>`
# variables, so there are NO literal hex codes below. Adding a colour means
# adding a palette entry there, not a hex here.
#
# Built on four supplied colours: cream ground, camel secondary, crimson
# primary, dusty rose. Two rules drive the rest:
#   1. No white. Bootstrap defaults cards, inputs, dropdowns, modals, tables and
#      popovers to #FFF, so each is overridden to the cream tint $brand-card.
#   2. Surfaces are tinted, not flat: the card sits lighter than the ground so
#      hierarchy reads without the border doing all the work.
.glb_theme <- bslib::bs_add_rules(
  bslib::bs_theme(brand = TRUE),
  "
  /* Cards + hairline border + a whisper of shadow = clear hierarchy. The card
     is a sand tint, never white, so it reads as a warm raised surface against
     the greige ground ($brand-canvas is Bootstrap's $body-bg via _brand.yml). */
  .card {
    background-color: $brand-card;
    border: 1px solid $brand-border;
    border-radius: 0.6rem;
    box-shadow: 0 1px 2px rgba($brand-ink, 0.06);
  }
  .card-header {
    background-color: $brand-card;
    border-bottom: 1px solid $brand-border;
    font-weight: 600;
  }

  /* Sweep up every remaining surface Bootstrap paints white. Without this the
     inputs, dropdowns and popovers punch white holes in a warm page. */
  .form-control, .form-select, .selectize-input, .selectize-dropdown,
  .dropdown-menu, .modal-content, .popover, .accordion-button,
  .accordion-body, .list-group-item, .input-group-text, .form-check-input {
    background-color: $brand-card;
    border-color: $brand-border;
    color: $brand-ink;
  }
  .accordion-button:not(.collapsed) {
    background-color: mix($brand-crimson, $brand-card, 8%);
    color: $brand-ink;
  }
  .bslib-sidebar-layout > .sidebar { background-color: $brand-card; }
  .bslib-sidebar-layout { --bslib-sidebar-bg: #{$brand-card}; }

  /* Value boxes: solid semantic fills read as heavy slabs, but a bare edge on
     a card reads flat. Tint the surface toward the status colour and keep the
     edge, so status is obvious and the page gains colour without shouting.
     Written per semantic class so it survives whatever status -> theme mapping
     the status module ends up using. */
  .bslib-value-box { border-radius: 0.6rem; }
  .bslib-value-box[class*='bg-'] {
    color: $brand-ink !important;
    border: 1px solid $brand-border;
    border-left-width: 4px !important;
    box-shadow: 0 1px 2px rgba($brand-ink, 0.06);
  }
  .bslib-value-box[class*='bg-'] .value-box-title {
    color: $brand-muted !important;
  }
  .bslib-value-box.bg-success {
    background-color: mix($brand-olive, $brand-card, 16%) !important;
    border-left-color: $brand-olive !important;
  }
  .bslib-value-box.bg-danger {
    background-color: mix($brand-brick, $brand-card, 14%) !important;
    border-left-color: $brand-brick !important;
  }
  .bslib-value-box.bg-warning {
    background-color: mix($brand-amber, $brand-card, 16%) !important;
    border-left-color: $brand-amber !important;
  }
  .bslib-value-box.bg-secondary {
    background-color: mix($brand-muted, $brand-card, 10%) !important;
    border-left-color: $brand-muted !important;
  }
  .bslib-value-box.bg-primary {
    background-color: mix($brand-crimson, $brand-card, 14%) !important;
    border-left-color: $brand-crimson !important;
  }

  /* Bootstrap 5.3 drives .text-muted from its own --bs-secondary-color, which
     lands as a washed-out salmon on this cream ground and fails contrast.
     Point it at the palette's muted brown instead. */
  .text-muted, .text-body-secondary {
    color: $brand-muted !important;
  }

  /* shinychat renders the greeting and its example prompts using
     --bs-secondary, which is the camel accent here: camel text on a cream
     ground is close to unreadable. Give the dock ink text and lift the prompt
     cards onto the card surface so they read as clickable. */
  #assistant_dock, .shiny-chat-container { color: $brand-ink; }
  /* .shiny-chat-greeting hard-sets --bs-secondary on itself and its children,
     so the opening message renders camel-on-cream. Confirmed by computed
     style; overridden here rather than guessed at. */
  .shiny-chat-greeting,
  .shiny-chat-greeting-content,
  .shiny-chat-greeting p { color: $brand-ink !important; }
  /* The visible label is a CHILD div, not the .suggestion span itself, and it
     carries its own camel colour: styling only the span left the text
     unreadable while reporting the right colour on the parent. */
  .suggestion, .shiny-chat-suggestion-list-item,
  .shiny-chat-suggestion-list-item-body {
    background-color: $brand-card !important;
    border-radius: 0.5rem;
    color: $brand-ink !important;
  }
  .suggestion, .shiny-chat-suggestion-list-item {
    border: 1px solid $brand-border !important;
  }
  .suggestion:hover, .shiny-chat-suggestion-list-item:hover {
    background-color: mix($brand-rose, $brand-card, 30%) !important;
    border-color: $brand-crimson !important;
  }
  /* No visible scrollbar inside the chat. It still scrolls when a
     conversation runs long; the bar itself is just hidden, because an
     always-present track in a mostly-empty panel looked broken. The global
     themed scrollbar rules below would otherwise apply here too. */
  #assistant_dock .shiny-chat-messages,
  #assistant_dock .shiny-chat-container,
  #assistant_dock .shiny-chat-messages-wrapper {
    scrollbar-width: none;
  }
  #assistant_dock .shiny-chat-messages::-webkit-scrollbar,
  #assistant_dock .shiny-chat-container::-webkit-scrollbar,
  #assistant_dock .shiny-chat-messages-wrapper::-webkit-scrollbar {
    width: 0;
    height: 0;
    display: none;
  }

  /* Gently rounded, tactile controls. Secondary stays a filled camel so the
     page carries two distinct accent colours rather than one. */
  .btn { border-radius: 0.5rem; }
  .form-control, .form-select, .selectize-input { border-radius: 0.5rem; }

  /* Navbar sits on the same surface as the cards. It previously carried a
     faint crimson wash to separate it, which just read as a different colour
     from the rest of the app; the bottom hairline does that job on its own. */
  .navbar {
    background-color: $brand-card !important;
    border-bottom: 1px solid $brand-border;
    box-shadow: none;
  }
  .navbar .navbar-brand, .navbar .nav-link { color: $brand-ink !important; }
  .navbar .nav-link:hover,
  .navbar .nav-link.active { color: $brand-tan !important; }
  #demo_tour { border-radius: 2rem; font-weight: 600; }

  /* Slim, themed scrollbars (the default white track looked jarring on the
     warm ground). */
  * { scrollbar-width: thin; scrollbar-color: $brand-border transparent; }
  ::-webkit-scrollbar { width: 10px; height: 10px; }
  ::-webkit-scrollbar-track { background: transparent; }
  ::-webkit-scrollbar-thumb {
    background-color: darken($brand-border, 8%);
    border-radius: 8px;
    border: 2px solid $brand-canvas;
  }
  ::-webkit-scrollbar-thumb:hover { background-color: darken($brand-border, 20%); }

  /* Guided tour (cicerone/driver.js) ships its own stock skin, which clashes
     with the palette. Pull the popover and highlight ring into the theme. */
  .driver-popover {
    background-color: $brand-card;
    color: $brand-ink;
    border: 1px solid $brand-border;
    border-radius: 0.6rem;
    box-shadow: 0 4px 14px rgba($brand-ink, 0.12);
    font-family: inherit;
  }
  .driver-popover-title { color: $brand-ink; font-weight: 600; }
  .driver-popover-description { color: $brand-muted; }
  .driver-popover-progress-text { color: $brand-muted; }
  .driver-popover-arrow-side-top.driver-popover-arrow { border-top-color: $brand-card; }
  .driver-popover-arrow-side-bottom.driver-popover-arrow { border-bottom-color: $brand-card; }
  .driver-popover-arrow-side-left.driver-popover-arrow { border-left-color: $brand-card; }
  .driver-popover-arrow-side-right.driver-popover-arrow { border-right-color: $brand-card; }
  .driver-popover-navigation-btns button {
    background-color: $brand-crimson;
    color: $brand-card;
    border: none;
    border-radius: 0.5rem;
    text-shadow: none;
    font-weight: 500;
  }
  .driver-popover-navigation-btns button:hover {
    background-color: darken($brand-crimson, 8%);
  }
  .driver-popover-close-btn { color: $brand-muted; }

  /* DT ships an unbranded skin; theme its controls so paging and the
     copy/CSV buttons match the rest of the app. */
  .dataTables_wrapper .dataTables_paginate .paginate_button.current,
  .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
    background: $brand-crimson !important;
    border-color: $brand-crimson !important;
    color: $brand-card !important;
  }
  .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
    background: $brand-border !important;
    border-color: $brand-border !important;
    color: $brand-ink !important;
  }
  /* DT's Buttons extension inherits btn-secondary, so the copy/CSV pair
     rendered as one heavy slab competing with the real action buttons. Match
     on the classes DataTables always sets (.dt-button alone does not match in
     this DT build) and force a quiet outline. */
  .dt-buttons .dt-button,
  .dataTables_wrapper .dt-button,
  .dt-button.btn-secondary,
  .buttons-copy, .buttons-csv, .buttons-html5,
  .dt-buttons .btn, .dt-buttons button {
    background: $brand-card !important;
    background-image: none !important;
    border: 1px solid $brand-border !important;
    border-radius: 0.5rem !important;
    color: $brand-ink !important;
    margin-right: 0.35rem;
  }
  .dt-buttons .dt-button:hover,
  .dataTables_wrapper .dt-button:hover,
  .buttons-copy:hover, .buttons-csv:hover, .buttons-html5:hover,
  .dt-buttons .btn:hover, .dt-buttons button:hover {
    background: $brand-canvas !important;
    border-color: $brand-tan !important;
    color: $brand-ink !important;
  }
  /* Table rows. DT renders with Bootstrap's .table-striped/.table-hover here,
     so the stripe comes from the --bs-table-* custom properties applied to the
     CELLS, not from any DataTables rule and not from the row background.
     Setting the variables is the supported hook and avoids a specificity
     fight; targeting tr background-color silently loses to the td.
     The stripe is a HALF-strength canvas tint: at full strength this cream
     ground is saturated enough that alternating rows read as banding rather
     than as a quiet reading aid. */
  table.dataTable, .table {
    --bs-table-bg: #{$brand-card};
    --bs-table-color: #{$brand-ink};
    --bs-table-striped-bg: #{mix($brand-canvas, $brand-card, 45%)};
    --bs-table-striped-color: #{$brand-ink};
    --bs-table-hover-bg: #{mix($brand-rose, $brand-card, 35%)};
    --bs-table-hover-color: #{$brand-ink};
    --bs-table-border-color: #{$brand-border};
  }
  /* Header sits slightly deeper than the body so the column names anchor. */
  table.dataTable thead th {
    background-color: mix($brand-tan, $brand-card, 22%);
    border-bottom: 1px solid $brand-border;
    color: $brand-ink;
  }
  .dataTables_wrapper .dataTables_info,
  .dataTables_wrapper .dataTables_length,
  .dataTables_wrapper .dataTables_filter { color: $brand-muted; }
  /* Paging: default Bootstrap greys clashed with the warm palette. */
  .dataTables_wrapper .dataTables_paginate .paginate_button {
    background: transparent !important;
    border: 1px solid transparent !important;
    color: $brand-ink !important;
  }
  .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
    color: $brand-muted !important;
    opacity: 0.6;
  }

  /* Consistent horizontal gutter on each page's content (not the navbar). */
  .bslib-page-navbar > .container-fluid > .tab-content > .tab-pane {
    padding-left: 4vw;
    padding-right: 4vw;
  }

  /* Tighten the build sidebar: no title, minimal top padding, so the Disease
     input sits near the top instead of below a big empty gap. */
  .glb-build-sidebar > .sidebar-content { padding-top: 0.5rem !important; }
  .glb-build-sidebar > .sidebar-content > div:first-child { margin-top: 0; }

  /* Compact single-line footer. */
  .glb-footer {
    font-size: 0.78rem;
    line-height: 1.4;
    background-color: $brand-card;
  }
  "
)

bslib::page_navbar(
  id = "main_nav",
  title = "Gene List Builder",
  # Branding from _brand.yml + the clean minimal Sass pass above.
  theme = .glb_theme,
  # Load shinyjs (demo auto-clicks the Resolve button) and cicerone (guided
  # tour) JS/CSS once for the whole app.
  header = tagList(
    shinyjs::useShinyjs(),
    cicerone::use_cicerone(),
    # Let the server click a control by DOM id (used by the assistant's tools to
    # drive Resolve / Build, and reused generally). Fire-and-forget from any
    # context via session$sendCustomMessage("glb_click", "<id>").
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('glb_click', function(id){",
      "var el=document.getElementById(id); if(el){el.click();}});"
    ))
  ),
  # App-wide "Gene List Builder assistant": the chat lives in a left-hand
  # sidebar available on every page. It starts CLOSED so it never blocks the
  # workflow; users open it from the "Assistant" button in the navbar (which
  # toggles it client-side). Its model / key controls sit in a collapsed
  # section inside the dock (byok_chat_ui).
  sidebar = bslib::sidebar(
    id = "assistant_dock",
    # No title: the robot icon and "Assistant" heading were redundant next to
    # the navbar control that opens this, and dropping them gives the
    # conversation back the vertical space they occupied.
    position = "left",
    open = "closed",
    width = 600,
    byok_chat_ui(
      "chat",
      title = NULL,
      greeting = glb_chat_greeting,
      # Fill the dock: the input box then sits at the BOTTOM rather than
      # floating mid-panel with dead space under it. 55vh was too short and
      # did exactly that. Offset covers the navbar, the "Model & key"
      # accordion above the chat, and the footer.
      height = "calc(100vh - 190px)"
    )
  ),
  bslib::nav_panel("Build", builder_page),
  bslib::nav_panel("About", about_page),
  # Right-aligned navbar actions (nav_spacer pushes what follows to the right):
  # an assistant toggle (opens/closes the chat sidebar via server.R) and a
  # guided-demo launcher that prefills breast cancer and starts the tour.
  bslib::nav_spacer(),
  bslib::nav_item(
    # Toggle the assistant sidebar CLIENT-SIDE by clicking its built-in
    # collapse-toggle. A server-side sidebar_toggle would queue behind a running
    # build (Shiny is single-threaded), so the panel would appear frozen; this
    # opens instantly regardless of what the server is doing.
    actionButton(
      "toggle_assistant",
      "Assistant",
      icon = icon("comments"),
      class = "btn-sm btn-outline-primary my-1",
      onclick = paste0(
        "document.querySelector(",
        "\"button.collapse-toggle[aria-controls='assistant_dock']\"",
        ")?.click()"
      )
    )
  ),
  bslib::nav_item(
    actionButton(
      "demo_tour",
      "Demo",
      icon = icon("circle-play"),
      class = "btn-sm btn-primary my-1"
    )
  ),
  # Site-wide footer, matching the other apps in this family. The research-use
  # note is deliberate: this tool ranks candidate genes to inform panel design,
  # it does not make clinical calls.
  # Kept to a single compact line: the research-use note runs inline rather
  # than on its own row, which halves the footer height.
  footer = tags$footer(
    class = "glb-footer border-top text-center text-muted py-1 px-2",
    sprintf("Gene List Builder v%s", app_version()),
    tags$span(class = "mx-1", "·"),
    "Samuel Bharti",
    tags$span(class = "mx-1", "·"),
    tags$a(
      href = "https://github.com/samuelbharti/gene-list-builder",
      target = "_blank",
      rel = "noopener",
      "Source"
    ),
    tags$span(class = "mx-1", "·"),
    tags$a(
      href = paste0(
        "https://github.com/samuelbharti/gene-list-builder",
        "/blob/main/LICENSE"
      ),
      target = "_blank",
      rel = "noopener",
      "MIT"
    ),
    tags$span(class = "mx-1", "·"),
    tags$strong("Research use only"),
    ", not a diagnostic result."
  )
)
