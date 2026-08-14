/* GeneScout review workbench — client behaviour.
   The app commits to a single warm/maroon theme (no light/dark or palette
   toggles), so this file carries only the interactions the layout needs:
   1. Ranked-table row selection: click/Enter sets a Shiny input and highlights
      the row without a server round-trip.
   2. Setup section collapse/expand driven by data attributes, plus the intro
      hero (hidden once a ranking exists). */
(function () {
  // ---- ranked-table row selection --------------------------------------
  function selectRow(tr) {
    var tbody = tr.parentNode;
    Array.prototype.forEach.call(tbody.children, function (x) {
      x.classList.remove("sel");
    });
    tr.classList.add("sel");
    var input = tr.getAttribute("data-input");
    var symbol = tr.getAttribute("data-symbol");
    if (window.Shiny && input) {
      Shiny.setInputValue(input, symbol, { priority: "event" });
    }
  }
  function rowFrom(target) {
    return target.closest ? target.closest(".gs-table tbody tr[data-symbol]") : null;
  }
  document.addEventListener("click", function (e) {
    var tr = rowFrom(e.target);
    if (tr) selectRow(tr);
  });
  document.addEventListener("keydown", function (e) {
    if (e.key !== "Enter" && e.key !== " ") return;
    var tr = rowFrom(e.target);
    if (!tr) return;
    e.preventDefault();
    selectRow(tr);
  });

  // ---- reset --------------------------------------------------------------
  // The results toolbar's Reset button carries data-gs-reset; route its click to
  // the setup's Clear button (data-gs-reset-target), which owns the server input,
  // then reopen the setup so the user lands on a blank form.
  document.addEventListener("click", function (e) {
    var t = e.target.closest ? e.target.closest("[data-gs-reset]") : null;
    if (!t) return;
    var target = document.querySelector("[data-gs-reset-target]");
    if (target) target.click();
    var setup = document.getElementById("gs-setup");
    if (setup) {
      setup.classList.add("open");
      setup.scrollIntoView({ behavior: "smooth", block: "start" });
    }
  });

  // ---- setup collapse / expand + pre-run chrome ------------------------
  // The intro hero is shown only BEFORE the first ranking (no context strip yet).
  // Once a strip exists the strip's own "Edit setup" reopens the setup and the
  // intro gives way to the results. Recomputed on toggle and whenever Shiny
  // renders/clears the strip. (Pre-run, the intro's "Set up your own" button is
  // the reopen affordance, so no separate reopen bar is needed.)
  function updateChrome() {
    var intro = document.querySelector(".gs-intro");
    var hasStrip = !!document.querySelector(".gs-strip");
    if (intro) intro.style.display = hasStrip ? "none" : "flex";
  }

  document.addEventListener("click", function (e) {
    var trigger = e.target.closest ? e.target.closest("[data-gs-setup]") : null;
    if (!trigger) return;
    var action = trigger.getAttribute("data-gs-setup"); // open | close | toggle
    var id = trigger.getAttribute("data-gs-target") || "gs-setup";
    var setup = document.getElementById(id);
    if (!setup) return;
    if (action === "open") setup.classList.add("open");
    else if (action === "close") setup.classList.remove("open");
    else setup.classList.toggle("open");
    if (setup.classList.contains("open")) {
      setup.scrollIntoView({ behavior: "smooth", block: "start" });
    }
    updateChrome();
  });

  // Watch the review container so the chrome reacts when the strip renders.
  function watchChrome() {
    updateChrome();
    var gs = document.querySelector(".gs");
    if (!gs || gs.__gsChromeWatched) return;
    gs.__gsChromeWatched = true;
    new MutationObserver(updateChrome).observe(gs, {
      childList: true,
      subtree: true,
    });
  }
  if (document.readyState !== "loading") watchChrome();
  else document.addEventListener("DOMContentLoaded", watchChrome);
  var tries = 0;
  var iv = setInterval(function () {
    watchChrome();
    if (document.querySelector(".gs") || ++tries > 25) clearInterval(iv);
  }, 150);
})();
