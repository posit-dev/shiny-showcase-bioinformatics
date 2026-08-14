# App server. Each registered page mounts its own module server(s).
function(input, output, session) {
  mount_page_servers()

  # Let the assistant know the active page and drive tab navigation (see
  # R/agent_bridge.R). Both live in session$userData, shared with the chat
  # module's session. The page filter bridges register themselves per page.
  session$userData[[.tahoe_nav_key]] <- function(page) {
    bslib::nav_select("main_nav", page, session = session)
  }
  observeEvent(
    input$main_nav,
    session$userData[[.tahoe_active_page_key]] <- input$main_nav,
    ignoreNULL = FALSE
  )

  # App-wide assistant sidebar (see ui.R), driven by the shared chat server. It
  # starts closed; the navbar "Assistant" button toggles it open/closed so it
  # never blocks the workflow.
  chat_agent_server("chat_dock", active_page = reactive(input$main_nav))
  observeEvent(input$toggle_assistant, {
    bslib::sidebar_toggle("assistant_dock", session = session)
  })

  # The assistant (600px) and a page's filter sidebar are too much side by side,
  # so couple them: while the assistant is open a page's filter sidebar collapses,
  # and it reopens when the assistant closes. input$assistant_dock is the
  # assistant sidebar's collapse state (TRUE = open); filters_open() is the state
  # the visible page's filter sidebar should be in.
  #
  # We only ever toggle the sidebar of the CURRENTLY VISIBLE page. Toggling a
  # sidebar that lives in a hidden tab leaves it blank when that tab is next
  # shown (bslib can't lay it out while the pane is display:none), which is the
  # bug where filters came up empty after using the assistant. So on a tab
  # change we re-apply the desired state to the newly shown page's sidebar while
  # it is visible, and on an assistant toggle we act on the active page only.
  .page_filter_sidebars <- list(
    drugs = "drugs-filters_sidebar",
    cell_lines = "cell_lines-filters_sidebar",
    coverage = "coverage-filters_sidebar",
    subset = "subset-filters_sidebar"
  )
  filters_open <- reactive(!isTRUE(input$assistant_dock))
  # The filter sidebar(s) currently VISIBLE for the active page. Samples & cells
  # has an inner tabset whose two sub-tabs each own a sidebar, so we pick the one
  # for the visible sub-tab -- never touch the hidden sub-tab's sidebar.
  active_filter_sidebars <- function() {
    page <- input$main_nav %||% ""
    if (identical(page, "obs")) {
      view <- input[["obs-view"]] %||% "samples"
      return(
        if (identical(view, "cells")) {
          "obs-query_sidebar"
        } else {
          "obs-filters_sidebar"
        }
      )
    }
    .page_filter_sidebars[[page]]
  }
  apply_filter_state <- function() {
    for (sb in active_filter_sidebars()) {
      bslib::sidebar_toggle(
        sb,
        open = isolate(filters_open()),
        session = session
      )
    }
  }
  # Re-apply the desired state whenever the visible sidebar context changes: the
  # assistant opening/closing, a tab change, or the obs sub-tab change. Acting
  # only on the visible sidebar keeps a sidebar from blanking out (bslib can't
  # lay one out while its tab pane is hidden).
  observeEvent(input$assistant_dock, ignoreInit = TRUE, apply_filter_state())
  observeEvent(input$main_nav, apply_filter_state())
  observeEvent(input[["obs-view"]], apply_filter_state())

  # Guided demo -- one connected walkthrough across pages. Every page has a
  # cicerone tour (R/tour.R); the navbar "Demo" button starts the tour for the
  # page the user is on, and when a page's tour ends -- whether they click
  # through to the end or hit "Skip" -- the next page opens and its tour begins,
  # so the whole app is one flowing tour. (Esc closes the driver overlay without
  # advancing, so it is the way to leave the demo.) Guides are built and
  # initialised once per session.
  guides <- lapply(tahoe_tours(), function(build) build())
  for (g in guides) {
    g$init()
  }
  # Pages that have a tour, in navbar order.
  demo_order <- intersect(names(app_pages()), names(guides))
  # The page whose tour is currently playing (NULL when the demo is not active).
  demo_page <- reactiveVal(NULL)

  # Open a page, make its tour's anchors visible, and start the tour. obs shows
  # its first sub-tab; any page with a filter sidebar has it opened so sidebar
  # anchors are on screen. The start is deferred so the pane (and sub-tab) is
  # laid out before cicerone measures its first target.
  start_page_demo <- function(page) {
    guide <- guides[[page]]
    if (is.null(guide)) {
      return(invisible())
    }
    demo_page(page)
    bslib::nav_select("main_nav", page, session = session)
    if (identical(page, "obs")) {
      bslib::nav_select("obs-view", "samples", session = session)
      bslib::sidebar_toggle(
        "obs-filters_sidebar",
        open = TRUE,
        session = session
      )
    }
    for (sb in .page_filter_sidebars[[page]]) {
      bslib::sidebar_toggle(sb, open = TRUE, session = session)
    }
    later::later(function() guide$start(session = session), delay = 0.5)
  }

  # Move to the next page's tour, if any. `from_page` guards against stale events
  # (a tour that already handed off) firing a second advance.
  advance_demo <- function(from_page) {
    cur <- isolate(demo_page())
    if (is.null(cur) || !identical(cur, from_page)) {
      return(invisible())
    }
    idx <- match(from_page, demo_order)
    if (is.na(idx) || idx >= length(demo_order)) {
      demo_page(NULL)
      return(invisible())
    }
    start_page_demo(demo_order[[idx + 1L]])
  }

  observeEvent(input$demo_tour, {
    page <- input$main_nav %||% ""
    if (is.null(guides[[page]])) {
      showNotification(
        "No demo is available for this page yet.",
        type = "message"
      )
      return()
    }
    start_page_demo(page)
  })

  # A page tour ends one of two ways. cicerone reports each "Next" click on
  # <guide-id>_cicerone_next with a has_next flag; has_next == FALSE means the
  # user finished the last step -> advance. "Skip" (the driver close button)
  # has no cicerone event, so a small JS listener (see ui.R) fires
  # input$tahoe_demo_skip; both paths hand off to the next page.
  for (pg in names(guides)) {
    local({
      page <- pg
      # cicerone keeps the guide id in a private field; read it to build the
      # `<id>_cicerone_next` input name it reports each Next click on.
      gid <- guides[[page]]$.__enclos_env__$private$id
      next_input <- paste0(gid, "_cicerone_next")
      observeEvent(input[[next_input]], {
        val <- input[[next_input]]
        if (!is.null(val) && isFALSE(val$has_next)) {
          advance_demo(page)
        }
      })
    })
  }
  observeEvent(input$tahoe_demo_skip, {
    cur <- isolate(demo_page())
    if (!is.null(cur)) {
      advance_demo(cur)
    }
  })
}
