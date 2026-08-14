# App UI. Tabs self-register via register_page() (see R/app_pages.R), so this
# file rarely changes when features are added.
do.call(
  bslib::page_navbar,
  c(
    list(
      title = "Tahoe Explorer",
      id = "main_nav",
      # Apply branding from _brand.yml (colors, fonts). brand = TRUE requires
      # the file to exist; switch to bslib::bs_theme() to make it optional.
      # The Sass below layers an "organic" pass over the Alpine Lake palette:
      # a soft misted-water background, rounded corners, and gentle teal-tinted
      # depth, so the app reads as natural rather than flat/boxy.
      theme = bslib::bs_add_rules(
        bslib::bs_theme(brand = TRUE),
        "
        /* Organic palette anchors (mirror _brand.yml). */
        :root {
          --tahoe-teal: #0B7285;
          --tahoe-green: #2F9E44;
          --tahoe-snow: #F8FAFB;
        }
        /* Misted-water backdrop: two faint colour pools over snow, fixed so it
           stays put while content scrolls. */
        body {
          background:
            radial-gradient(1100px 600px at 100% -8%,
              rgba(47, 158, 68, 0.06), transparent 60%),
            radial-gradient(1000px 520px at -8% 2%,
              rgba(11, 114, 133, 0.07), transparent 58%),
            var(--tahoe-snow);
          background-attachment: fixed;
        }
        /* Cards: softer corners, a hairline teal edge, and layered teal-tinted
           shadow that deepens gently on hover (organic lift, no jump). */
        .card {
          border: 1px solid rgba(11, 114, 133, 0.07);
          border-radius: 0.9rem;
          box-shadow: 0 1px 2px rgba(33, 37, 41, 0.04),
                      0 8px 22px rgba(11, 114, 133, 0.06);
          transition: box-shadow 0.25s ease;
        }
        .card:hover {
          box-shadow: 0 2px 4px rgba(33, 37, 41, 0.05),
                      0 14px 32px rgba(11, 114, 133, 0.10);
        }
        .card-header {
          background-color: transparent;
          border-bottom: 1px solid rgba(11, 114, 133, 0.10);
          font-weight: 600;
        }
        .bslib-value-box {
          border-radius: 0.9rem;
          box-shadow: 0 8px 22px rgba(11, 114, 133, 0.08);
        }
        /* Rounded, tactile controls. */
        .btn { border-radius: 0.6rem; }
        .form-control, .form-select, .selectize-input {
          border-radius: 0.6rem;
        }
        /* Frosted navbar so the backdrop shows through faintly. */
        .navbar {
          background-color: rgba(248, 250, 251, 0.85) !important;
          backdrop-filter: saturate(1.15) blur(8px);
          box-shadow: 0 1px 0 rgba(11, 114, 133, 0.08);
        }
        /* The navbar Demo button: a rounded, on-brand call to action. */
        #demo_tour {
          border-radius: 2rem;
          font-weight: 600;
        }
        /* Tahoe-tinted scrollbars (teal thumb on a faint track). */
        * { scrollbar-width: thin; scrollbar-color: rgba(11,114,133,0.5) rgba(11,114,133,0.08); }
        ::-webkit-scrollbar { width: 10px; height: 10px; }
        ::-webkit-scrollbar-track { background: rgba(11,114,133,0.06); border-radius: 8px; }
        ::-webkit-scrollbar-thumb {
          background: rgba(11,114,133,0.45);
          border-radius: 8px;
          border: 2px solid transparent;
          background-clip: content-box;
        }
        ::-webkit-scrollbar-thumb:hover { background: rgba(11,114,133,0.7); background-clip: content-box; }
        /* Consistent horizontal gutter on every page's content (not the navbar
           menu). Applied to the tab-pane so fillable pages (Chat) match too. */
        .bslib-page-navbar > .container-fluid > .tab-content > .tab-pane {
          padding-left: 4vw;
          padding-right: 4vw;
        }
        /* Themed loading states (Alpine Lake). A soft snow veil with a teal /
           green ring spins over any chart, table, or widget while it recomputes;
           a full-screen loader covers the first paint; a thin bar pulses at the
           top of the page whenever Shiny is busy. */
        @keyframes tahoe-spin { to { transform: rotate(360deg); } }
        @keyframes tahoe-pulse-bar {
          0% { transform: translateX(-100%); }
          100% { transform: translateX(250%); }
        }
        .html-widget-output.recalculating,
        .shiny-plot-output.recalculating {
          opacity: 1 !important;
          position: relative;
        }
        .html-widget-output.recalculating::before,
        .shiny-plot-output.recalculating::before {
          content: ''; position: absolute; inset: 0; z-index: 5;
          background: rgba(248, 250, 251, 0.62);
          border-radius: inherit;
        }
        .html-widget-output.recalculating::after,
        .shiny-plot-output.recalculating::after {
          content: ''; position: absolute; inset: 0; margin: auto; z-index: 6;
          width: 2.4rem; height: 2.4rem;
          border: 3px solid rgba(11, 114, 133, 0.18);
          border-top-color: var(--tahoe-teal, #0B7285);
          border-right-color: var(--tahoe-green, #2F9E44);
          border-radius: 50%;
          animation: tahoe-spin 0.8s linear infinite;
        }
        #tahoe-app-loader {
          position: fixed; inset: 0; z-index: 2000;
          display: flex; flex-direction: column;
          align-items: center; justify-content: center; gap: 1rem;
          background:
            radial-gradient(900px 500px at 100% -8%,
              rgba(47, 158, 68, 0.07), transparent 60%),
            radial-gradient(820px 440px at -8% 2%,
              rgba(11, 114, 133, 0.08), transparent 58%),
            var(--tahoe-snow, #F8FAFB);
          transition: opacity 0.35s ease;
        }
        #tahoe-app-loader.tahoe-hide { opacity: 0; pointer-events: none; }
        #tahoe-app-loader .tahoe-loader-ring {
          width: 3.2rem; height: 3.2rem;
          border: 4px solid rgba(11, 114, 133, 0.16);
          border-top-color: var(--tahoe-teal, #0B7285);
          border-right-color: var(--tahoe-green, #2F9E44);
          border-radius: 50%;
          animation: tahoe-spin 0.85s linear infinite;
        }
        #tahoe-app-loader .tahoe-loader-label {
          color: var(--tahoe-teal, #0B7285);
          font-weight: 600; letter-spacing: 0.02em;
        }
        #tahoe-busy-bar {
          position: fixed; top: 0; left: 0; right: 0; height: 3px;
          z-index: 1998; overflow: hidden; opacity: 0;
          transition: opacity 0.2s ease;
        }
        body.tahoe-busy #tahoe-busy-bar { opacity: 1; }
        #tahoe-busy-bar::before {
          content: ''; display: block; height: 100%; width: 30%;
          background: linear-gradient(90deg, transparent,
            var(--tahoe-teal, #0B7285), var(--tahoe-green, #2F9E44), transparent);
          animation: tahoe-pulse-bar 1.1s ease-in-out infinite;
        }
        "
      ),
      # Load the guided-tour (cicerone) JS/CSS dependency once for the whole app,
      # plus a handler that redraws every plot to its container on request (the
      # plot-card refresh buttons; see R/theme.R tahoe_plot_refresh_*).
      header = tagList(
        cicerone::use_cicerone(),
        # A resize handler for the plot-card refresh buttons, plus a nudge that
        # fires a resize whenever a navbar tab is shown. Inputs (and their filter
        # sidebar) laid out while their tab was hidden can come up empty or zero
        # width; the resize makes bslib and the selectize widgets reflow to full
        # size on show, so switching tabs after toggling the assistant no longer
        # leaves the filter sidebar blank.
        tags$script(HTML(
          "Shiny.addCustomMessageHandler('tahoe_resize_plots', function(msg){",
          "  window.dispatchEvent(new Event('resize'));",
          "});",
          "document.addEventListener('shown.bs.tab', function(){",
          "  setTimeout(function(){",
          "    window.dispatchEvent(new Event('resize'));",
          "  }, 60);",
          "});"
        )),
        # First-paint loader + a top progress bar shown while Shiny is busy. The
        # loader is removed once Shiny first goes idle (initial outputs computed),
        # with a timeout fallback so it can never get stuck.
        tags$div(id = "tahoe-busy-bar"),
        tags$div(
          id = "tahoe-app-loader",
          tags$div(class = "tahoe-loader-ring"),
          tags$div(class = "tahoe-loader-label", "Loading Tahoe Explorer...")
        ),
        tags$script(HTML(
          "(function(){",
          "  function hide(){",
          "    var el=document.getElementById('tahoe-app-loader');",
          "    if(el){el.classList.add('tahoe-hide');",
          "      setTimeout(function(){if(el)el.remove();},450);}",
          "  }",
          "  if(window.jQuery){",
          "    $(document).one('shiny:idle', hide);",
          "    $(document).on('shiny:busy', function(){",
          "      document.body.classList.add('tahoe-busy');});",
          "    $(document).on('shiny:idle', function(){",
          "      document.body.classList.remove('tahoe-busy');});",
          "  }",
          "  setTimeout(hide, 12000);",
          "  /* Demo 'Skip' (driver close) -> Shiny input so the demo can hand",
          "     off to the next page; cicerone has no close event of its own. */",
          "  document.addEventListener('click', function(ev){",
          "    var t = ev.target;",
          "    if(t && t.closest && t.closest('.driver-close-btn')){",
          "      if(window.Shiny && Shiny.setInputValue){",
          "        Shiny.setInputValue('tahoe_demo_skip', Date.now(),",
          "          {priority: 'event'});",
          "      }",
          "    }",
          "  }, true);",
          "})();"
        ))
      ),
      # App-wide "Tahoe assistant": the chat lives in a left-hand sidebar
      # available on every page. It starts CLOSED so it never blocks the
      # workflow; users open it from the "Assistant" button in the navbar (which
      # toggles it via server.R) or the sidebar's own toggle. The model / key
      # controls sit in a collapsed section inside the sidebar (chat_agent_ui).
      sidebar = bslib::sidebar(
        id = "assistant_dock",
        # No sidebar title: the "Model & key" settings sit at the very top, with
        # the "Tahoe assistant" heading below them, inside chat_agent_ui.
        position = "left",
        open = "closed",
        width = 600,
        chat_agent_ui("chat_dock")
      ),
      # No page opts into a fillable panel (a fillable panel would force a fixed
      # viewport height and pin the footer mid-page); returns FALSE.
      fillable = app_fillable_pages(),
      # App-wide attribution footer (4vw gutter matches the page content).
      footer = tags$footer(
        class = "text-muted small border-top mt-3",
        style = "padding: 0.75rem 4vw;",
        div(
          class = paste(
            "d-flex flex-wrap justify-content-between align-items-center gap-2"
          ),
          span(HTML(
            "<strong>Tahoe Explorer</strong> &middot; built by Samuel Bharti",
            "&middot; MIT &copy; 2024&ndash;2026"
          )),
          span(
            "Data: ",
            tags$a(
              href = "https://huggingface.co/datasets/tahoebio/Tahoe-100M",
              target = "_blank",
              rel = "noopener",
              "Tahoe-100M"
            ),
            " · ",
            tags$a(
              href = "https://github.com/samuelbharti/tahoe-explorer",
              target = "_blank",
              rel = "noopener",
              "Source"
            )
          )
        )
      )
    ),
    app_nav_panels(),
    # Right-aligned navbar actions (nav_spacer pushes everything after it right):
    # a Tahoe-assistant toggle (opens/closes the chat sidebar via server.R; its
    # model/key settings live in a collapsed section inside it), a guided-demo
    # launcher that starts the active tab's cicerone tour (server.R / R/tour.R),
    # and a data-provenance chip showing whether the numbers are real or fixtures.
    list(
      bslib::nav_spacer(),
      bslib::nav_item(
        actionButton(
          "toggle_assistant",
          "Assistant",
          icon = icon("comments"),
          class = "btn-sm btn-outline-primary my-1"
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
      bslib::nav_item(tahoe_provenance_badge())
    )
  )
)
