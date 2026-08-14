/* GeneScout guided demo — a click-by-click product tour.
   Triggered by any [data-gs-tour="start"] element (the "Guided demo" button). It
   loads the bundled 4-source NF1 example, then walks the user through the setup,
   the deterministic ranking (which THEY click to run), the anti-bias veto money
   shot, the grounded evidence, the optional AI layer, and the Chat tab.

   A single spotlight ring (a positioned box with a huge box-shadow) dims the page
   around the current target; a coach-mark card explains the step. Nothing is
   click-blocked, so at the "Rank" step the user really clicks the button and the
   tour auto-advances once the results render. No external dependencies. */
(function () {
  var ring, card, active = false, idx = 0, watcher = null;
  var showSeq = 0, pendingTimer = null; // guard stale locate() retries across steps

  // Resolve a step's target element (a CSS selector or a function returning one).
  function targetOf(step) {
    if (typeof step.el === "function") return step.el();
    return step.sel ? document.querySelector(step.sel) : null;
  }

  function navLinkByText(text) {
    var links = document.querySelectorAll(".navbar .nav-link");
    for (var i = 0; i < links.length; i++) {
      if ((links[i].textContent || "").trim() === text) return links[i];
    }
    return null;
  }

  function clickRow(symbol) {
    var tr = document.querySelector('.gs-table tbody tr[data-symbol="' + symbol + '"]');
    if (tr) tr.click();
  }

  var steps = [
    {
      sel: ".gs-card-genes",
      waitFor: "#review-input-sources_anchor .border",
      title: "Your candidate lists",
      body: "We loaded four assay-tagged NF1 lists — WES, RNA-seq, ATAC-seq and a CRISPR screen. A gene found across more of your own lists is rewarded (cross-source corroboration), automatically."
    },
    {
      sel: ".gs-card-genes .gs-weight",
      title: "Weight each list",
      body: "Trust one assay more than another? Give its list a higher weight. It only affects ranking when you have two or more lists — and a high weight can never fabricate support from a single one."
    },
    {
      sel: ".gs-card-context",
      title: "Study context",
      body: "The NF1 context is applied: relevant pathways, known drivers, tissues of interest, and the FLAGS artifact-gene veto. The ranking becomes disease-aware rather than measuring general prominence."
    },
    {
      sel: "#review-input-run",
      cta: true,
      advanceOn: ".gs-strip",
      title: "Rank the genes",
      body: "Your turn: click <b>Rank genes</b>. GeneScout pulls a cited signal per gene from public sources and ranks them by a transparent composite — no API key needed."
    },
    {
      el: function () {
        return (
          document.querySelector('.gs-table tbody tr[data-symbol="TTN"]') ||
          document.querySelector(".gs-table")
        );
      },
      title: "The anti-bias veto",
      body: "TTN is huge and famous, so it <i>looks</i> compelling — but it is vetoed to the bottom as a recurrent sequencing artifact, while NF1 grades High. Familiar-gene bias, defeated by design."
    },
    {
      sel: ".gs-detail",
      onEnter: function () { clickRow("NF1"); },
      title: "Every signal is grounded",
      body: "Select any gene to open its evidence here — each value traces to a database accession or a PMID you can click through. Nothing in a GeneScout result is a claim you cannot trace to a source."
    },
    {
      sel: ".gs-toolbar .actions",
      title: "Add the AI layer (optional)",
      body: "With your own API key, <b>Curate with AI</b> compacts the list and the <b>Specialists</b> add a per-gene plausibility verdict and a suggested next experiment. <b>Export</b> gives an auditable report or CSV."
    },
    {
      el: function () { return navLinkByText("Chat"); },
      title: "Ask the grounded assistant",
      body: "The <b>Chat</b> tab answers questions about this exact run — grounded in the cited evidence, never ungrounded and never clinical. That's the whole tour — explore freely!"
    }
  ];

  function build() {
    ring = document.createElement("div");
    ring.className = "gs-tour-ring";
    card = document.createElement("div");
    card.className = "gs-tour-card";
    card.setAttribute("role", "dialog");
    card.setAttribute("aria-modal", "true");
    card.setAttribute("aria-labelledby", "gs-tour-title");
    card.setAttribute("tabindex", "-1");
    card.innerHTML =
      '<div class="gs-tour-count"></div>' +
      '<h4 class="gs-tour-title" id="gs-tour-title"></h4>' +
      '<div class="gs-tour-body"></div>' +
      '<div class="gs-tour-nav">' +
      '<button type="button" class="gs-tour-skip">Skip tour</button>' +
      '<span class="gs-tour-spacer"></span>' +
      '<button type="button" class="gs-tour-back">Back</button>' +
      '<button type="button" class="gs-tour-next">Next</button>' +
      "</div>";
    document.body.appendChild(ring);
    document.body.appendChild(card);
    card.querySelector(".gs-tour-skip").addEventListener("click", end);
    card.querySelector(".gs-tour-back").addEventListener("click", function () {
      if (idx > 0) show(idx - 1);
    });
    card.querySelector(".gs-tour-next").addEventListener("click", function () {
      if (idx < steps.length - 1) show(idx + 1);
      else end();
    });
  }

  function place(el, cta) {
    var pad = 8;
    var r = el.getBoundingClientRect();
    ring.style.display = "block";
    ring.style.left = r.left - pad + "px";
    ring.style.top = r.top - pad + "px";
    ring.style.width = r.width + pad * 2 + "px";
    ring.style.height = r.height + pad * 2 + "px";
    ring.classList.toggle("cta", !!cta);

    // Card: below the target if there is room, else above; clamped to viewport.
    var cw = Math.min(360, window.innerWidth - 24);
    card.style.width = cw + "px";
    var below = r.bottom + 14;
    var cardH = card.offsetHeight || 180;
    var top = below + cardH < window.innerHeight ? below : Math.max(12, r.top - cardH - 14);
    var left = Math.min(
      Math.max(12, r.left + r.width / 2 - cw / 2),
      window.innerWidth - cw - 12
    );
    card.style.top = top + "px";
    card.style.left = left + "px";
  }

  function centerCard() {
    ring.style.display = "none";
    var cw = Math.min(360, window.innerWidth - 24);
    card.style.width = cw + "px";
    card.style.left = window.innerWidth / 2 - cw / 2 + "px";
    card.style.top = Math.max(24, window.innerHeight / 2 - 120) + "px";
  }

  var reposition = null;

  function show(i) {
    idx = i;
    var step = steps[i];
    var gen = ++showSeq; // any earlier step's pending retries/watcher become stale
    if (watcher) { clearInterval(watcher); watcher = null; }
    if (pendingTimer) { clearTimeout(pendingTimer); pendingTimer = null; }

    card.querySelector(".gs-tour-count").textContent =
      "Step " + (i + 1) + " of " + steps.length;
    card.querySelector(".gs-tour-title").textContent = step.title;
    card.querySelector(".gs-tour-body").innerHTML = step.body;
    card.querySelector(".gs-tour-back").style.visibility = i === 0 ? "hidden" : "visible";
    var nextBtn = card.querySelector(".gs-tour-next");
    nextBtn.textContent = i === steps.length - 1 ? "Done" : "Next";

    if (step.onEnter) { try { step.onEnter(); } catch (e) {} }

    // For a step that waits on the user's own click (e.g. Rank), soften Next to a
    // hint and auto-advance when the expected result appears.
    if (step.advanceOn) {
      nextBtn.textContent = "Waiting…";
      nextBtn.classList.add("waiting");
    } else {
      nextBtn.classList.remove("waiting");
    }
    // Move focus into the coach-mark so keyboard / screen-reader users follow.
    try { card.focus({ preventScroll: true }); } catch (e) { card.focus(); }

    var attempts = 0;
    (function locate() {
      if (gen !== showSeq || !ring) return; // superseded by a newer step, or ended
      var el = targetOf(step);
      var ready = el && (!step.waitFor || document.querySelector(step.waitFor));
      if (ready) {
        el.scrollIntoView({ behavior: "smooth", block: "center" });
        pendingTimer = setTimeout(function () {
          if (gen !== showSeq || !ring) return;
          place(el, step.cta);
          reposition = function () { var t = targetOf(step); if (t) place(t, step.cta); };
        }, step.waitFor ? 260 : 160);
        if (step.advanceOn) {
          watcher = setInterval(function () {
            if (gen !== showSeq) { clearInterval(watcher); return; }
            if (document.querySelector(step.advanceOn)) {
              clearInterval(watcher); watcher = null;
              nextBtn.classList.remove("waiting");
              show(idx + 1);
            }
          }, 250);
        }
      } else if (++attempts < 40) {
        pendingTimer = setTimeout(locate, 150);
      } else {
        centerCard(); // target never showed — keep the tour usable
      }
    })();
  }

  function start() {
    if (active) return;
    active = true;
    if (!ring) build();
    document.body.classList.add("gs-tour-on");
    // Open the setup and load the 4-source example, then begin.
    var setup = document.getElementById("gs-setup");
    if (setup) setup.classList.add("open");
    var load = document.getElementById("review-input-load_multisource");
    if (load) load.click();
    reposition = null;
    window.addEventListener("scroll", onMove, true);
    window.addEventListener("resize", onMove);
    setTimeout(function () { show(0); }, 350);
  }

  function onMove() { if (reposition) reposition(); }

  function end() {
    active = false;
    showSeq++; // invalidate any in-flight locate()/watcher
    if (watcher) { clearInterval(watcher); watcher = null; }
    if (pendingTimer) { clearTimeout(pendingTimer); pendingTimer = null; }
    document.body.classList.remove("gs-tour-on");
    window.removeEventListener("scroll", onMove, true);
    window.removeEventListener("resize", onMove);
    if (ring) ring.remove();
    if (card) card.remove();
    ring = null;
    card = null;
  }

  document.addEventListener("click", function (e) {
    var t = e.target.closest ? e.target.closest('[data-gs-tour="start"]') : null;
    if (!t) return;
    e.preventDefault();
    start();
  });
  document.addEventListener("keydown", function (e) {
    if (active && e.key === "Escape") end();
  });
})();
