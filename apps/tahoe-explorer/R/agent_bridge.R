# Cross-module bridge: let the Chat assistant know which page the user is on and
# drive the input controls (filters / selections) on the interactive pages.
#
# Each interactive page (Drugs, Cell lines, Subset builder, Coverage, Samples &
# cells) registers a tiny bridge -- get()/set() closures over ITS OWN module
# session -- into session$userData, which Shiny shares across module session
# proxies (the root session and every module see the same environment). server.R
# records the active page and a navigate closure there too. The Chat server looks
# these up LAZILY at tool-call time, so page mount order does not matter.
#
# The get()/set() closures run from the chat module's async tool-call
# continuation, NOT inside the page module's reactive context, so each page
# isolates every reactive read and targets its own captured session for input
# writes.

.tahoe_page_bridges_key <- "tahoe_page_bridges"
.tahoe_active_page_key <- "tahoe_active_page"
.tahoe_nav_key <- "tahoe_nav_select"

#' Register an interactive page's control bridge for this session. `page_id` is
#' the register_page id (navbar value). `bridge` is a list with:
#'   * title -- human label for the page
#'   * get() -> list(filters = <named list of field -> list(current, options)>,
#'       ...optional extras like estimated_cells)
#'   * set(request) -> list(applied, ...); `request` is the canonical union of
#'       filter fields (organs, driver_genes, variant_types, cell_lines, drugs,
#'       doses, plates, moa, moa_fine, approval, trials, target, name); a page
#'       reads only the fields it supports.
#' No-op outside a Shiny session, so callers need no guard.
tahoe_register_page_bridge <- function(session, page_id, bridge) {
  if (is.null(session) || is.null(session$userData)) {
    return(invisible(FALSE))
  }
  reg <- session$userData[[.tahoe_page_bridges_key]]
  if (is.null(reg)) {
    reg <- list()
  }
  reg[[page_id]] <- bridge
  session$userData[[.tahoe_page_bridges_key]] <- reg
  invisible(TRUE)
}

#' The control bridge for `page_id`, or NULL if that page is not mounted.
tahoe_get_page_bridge <- function(session, page_id) {
  if (is.null(session) || is.null(session$userData) || is.null(page_id)) {
    return(NULL)
  }
  reg <- session$userData[[.tahoe_page_bridges_key]]
  if (is.null(reg)) NULL else reg[[page_id]]
}

#' Ids of every page that has registered a control bridge this session.
tahoe_page_bridge_ids <- function(session) {
  if (is.null(session) || is.null(session$userData)) {
    return(character())
  }
  reg <- session$userData[[.tahoe_page_bridges_key]]
  if (is.null(reg)) character() else names(reg)
}

#' The page id the user is currently viewing (recorded by server.R), or NULL.
tahoe_active_page <- function(session) {
  if (is.null(session) || is.null(session$userData)) {
    return(NULL)
  }
  session$userData[[.tahoe_active_page_key]]
}

# Switch the app to `page_id` via the root-session navigate closure (server.R).
.tahoe_page_navigate <- function(session, page_id) {
  if (is.null(session) || is.null(session$userData)) {
    return(invisible(NULL))
  }
  nav <- session$userData[[.tahoe_nav_key]]
  if (is.function(nav)) {
    tryCatch(nav(page_id), error = function(e) NULL)
  }
  invisible(NULL)
}

#' Validate requested values for a multi-select against its allowed `domain`.
#' Returns list(good, bad); `good` is NULL when `vals` is NULL (leave untouched).
#' Shared by the page bridges so every page reports invalid values the same way.
tahoe_bridge_validate <- function(vals, domain, numeric = FALSE) {
  if (is.null(vals)) {
    return(list(good = NULL, bad = character()))
  }
  vals <- if (numeric) {
    suppressWarnings(as.numeric(unlist(vals)))
  } else {
    as.character(unlist(vals))
  }
  vals <- vals[!is.na(vals)]
  list(
    good = unique(vals[vals %in% domain]),
    bad = unique(vals[!(vals %in% domain)])
  )
}

#' Session-aware tools that let the assistant read the active page and drive the
#' filters/selections on any interactive page. Kept separate from
#' tahoe_agent_tools() because they need the Shiny `session` to reach the page
#' bridges in userData; the chat server appends them to the base suite. Each
#' looks bridges up at CALL time and degrades to a friendly message (never an
#' error) when a page is not present.
tahoe_page_control_tools <- function(session) {
  str_arr <- function(desc) {
    list(type = "array", items = "string", desc = desc, required = FALSE)
  }
  num_arr <- function(desc) {
    list(type = "array", items = "number", desc = desc, required = FALSE)
  }

  # Resolve the page to act on: an explicit, controllable page id wins; else the
  # user's active page (if controllable); else NULL.
  resolve_page <- function(page) {
    ids <- tahoe_page_bridge_ids(session)
    if (!is.null(page) && nzchar(page) && page %in% ids) {
      return(page)
    }
    ap <- tahoe_active_page(session)
    if (!is.null(ap) && ap %in% ids) ap else NULL
  }

  list(
    list(
      name = "get_active_page",
      description = paste(
        "Report which app page (tab) the user is currently viewing and which",
        "pages the assistant can control. Call this before applying filters so",
        "you target the right page -- the user's ACTIVE page by default, unless",
        "they name another."
      ),
      arguments = list(),
      fun = function() {
        list(
          active_page = tahoe_active_page(session) %||% "unknown",
          controllable_pages = tahoe_page_bridge_ids(session)
        )
      }
    ),
    list(
      name = "get_page_controls",
      description = paste(
        "Read a page's current filter/selection controls and the valid options",
        "for each. `page` defaults to the user's active page. Use this to learn",
        "the exact option values before calling set_page_controls."
      ),
      arguments = list(
        page = list(
          type = "string",
          desc = paste(
            "Page id (drugs, cell_lines, subset, coverage, obs). Omit for the",
            "active page."
          ),
          required = FALSE
        )
      ),
      fun = function(page = NULL) {
        pid <- resolve_page(page)
        if (is.null(pid)) {
          return(list(
            available = FALSE,
            message = "That page has no controllable filters, or is not loaded.",
            active_page = tahoe_active_page(session) %||% "unknown",
            controllable_pages = tahoe_page_bridge_ids(session)
          ))
        }
        bridge <- tahoe_get_page_bridge(session, pid)
        info <- tryCatch(bridge$get(), error = function(e) NULL)
        if (is.null(info)) {
          return(list(
            available = FALSE,
            page = pid,
            message = "Could not read this page's controls."
          ))
        }
        c(
          list(available = TRUE, page = pid, title = bridge$title %||% pid),
          info
        )
      }
    ),
    list(
      name = "set_page_controls",
      description = paste(
        "Apply filters / selections to an app page's controls and switch to that",
        "page so the user sees the result. `page` defaults to the user's ACTIVE",
        "page; pass it only when the user names a different page. Pass just the",
        "fields that page supports (each page uses a subset):",
        "drugs -> moa, moa_fine, approval, trials, target, name;",
        "cell_lines -> organs, driver_genes, variant_types, name;",
        "subset (Subset builder) -> organs, driver_genes, cell_lines, drugs,",
        "doses, plates; coverage -> drugs, organs; obs (Samples & cells) ->",
        "drugs, plates. Only the fields you pass change; omit one to leave it,",
        "or pass an empty array to clear it. Values not in the dataset are",
        "ignored and reported in `ignored`, so check get_page_controls first.",
        "`target` and `name` are free-text 'contains' filters. Doses are uM",
        "(0.05 / 0.5 / 5)."
      ),
      arguments = list(
        page = list(
          type = "string",
          desc = "Target page id. Omit to use the active page.",
          required = FALSE
        ),
        organs = str_arr("Tissues / organs."),
        driver_genes = str_arr("Driver-mutation genes."),
        variant_types = str_arr("Variant types (cell_lines page)."),
        cell_lines = str_arr("Exact cell-line names (subset page)."),
        drugs = str_arr("Exact drug names."),
        doses = num_arr("Doses in uM (0.05 / 0.5 / 5)."),
        plates = str_arr("Plate ids."),
        moa = str_arr("Mechanism of action, broad (drugs page)."),
        moa_fine = str_arr("Mechanism of action, fine (drugs page)."),
        approval = str_arr("Human-approved status values (drugs page)."),
        trials = str_arr("Clinical-trial status values (drugs page)."),
        target = list(
          type = "string",
          desc = "Target-gene 'contains' text (drugs page).",
          required = FALSE
        ),
        name = list(
          type = "string",
          desc = "Name 'contains' text (drugs / cell_lines pages).",
          required = FALSE
        )
      ),
      fun = function(
        page = NULL,
        organs = NULL,
        driver_genes = NULL,
        variant_types = NULL,
        cell_lines = NULL,
        drugs = NULL,
        doses = NULL,
        plates = NULL,
        moa = NULL,
        moa_fine = NULL,
        approval = NULL,
        trials = NULL,
        target = NULL,
        name = NULL
      ) {
        pid <- resolve_page(page)
        if (is.null(pid)) {
          return(list(
            applied = FALSE,
            message = paste(
              "No controllable page is active. Ask the user to open one, or",
              "name the page to act on."
            ),
            controllable_pages = tahoe_page_bridge_ids(session)
          ))
        }
        bridge <- tahoe_get_page_bridge(session, pid)
        .tahoe_page_navigate(session, pid)
        request <- list(
          organs = organs,
          driver_genes = driver_genes,
          variant_types = variant_types,
          cell_lines = cell_lines,
          drugs = drugs,
          doses = doses,
          plates = plates,
          moa = moa,
          moa_fine = moa_fine,
          approval = approval,
          trials = trials,
          target = target,
          name = name
        )
        res <- tryCatch(
          bridge$set(request),
          error = function(e) {
            list(applied = FALSE, message = "Could not update this page.")
          }
        )
        c(list(page = pid, title = bridge$title %||% pid), res)
      }
    )
  )
}
