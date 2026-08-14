# Biological Visualizations - a Shiny React (ui.tsx) app.
#
# The React client (www/app.js, built from srcts/*.tsx) owns all UI. This server
# is compute-only: it publishes one reactive_output() feed per visualization
# (both the plotomics data contract for the React engine AND a ggplot2 PNG
# data-URI for the classic engine), reacting to the client's shared controls.
#
# Run:  R -e "shiny::runApp('.', port = 8000)"

library(shiny)
library(shinyreact)
library(ggplot2)

# Pure logic + rendering layers (no shiny:: inside these).
source("R/palettes.R", local = TRUE)
source("R/data.R", local = TRUE)
source("R/plots.R", local = TRUE)
source("R/chat.R", local = TRUE)

ui <- page_react_html("www/index.html")

server <- function(input, output, session) {

  # ---- shared controls (defaults mirror the plotomics defaults) ----
  fc      <- reactive(if (is.null(input$fc)) 1 else input$fc)
  pcut    <- reactive(if (is.null(input$p)) 0.05 else input$p)
  n_genes <- reactive(if (is.null(input$n_genes)) 40 else input$n_genes)
  zscore  <- reactive(if (is.null(input$zscore)) TRUE else isTRUE(input$zscore))
  umap_by <- reactive(if (is.null(input$umap_color)) "cell_type" else input$umap_color)
  nd_ch <- reactive(if (is.null(input$nd_channel)) 15L else as.integer(input$nd_channel))
  nd_px <- reactive(if (is.null(input$nd_px)) 28L else as.integer(input$nd_px))
  nd_py <- reactive(if (is.null(input$nd_py)) 30L else as.integer(input$nd_py))
  igv_gene <- reactive(if (is.null(input$igv_gene)) "TP53" else input$igv_gene)
  prot_acc <- reactive(if (is.null(input$protein_uniprot)) "P04637" else input$protein_uniprot)
  prot_res <- reactive(input$protein_residue)
  onco_n <- reactive(if (is.null(input$onco_genes)) 25L else as.integer(input$onco_genes))
  lolli_gene <- reactive(if (is.null(input$lolli_gene)) "TP53" else input$lolli_gene)
  sbs_which <- reactive(if (is.null(input$sbs_profile)) "catalogue" else input$sbs_profile)
  vis_gene <- reactive(if (is.null(input$visium_gene)) "ERBB2" else input$visium_gene)
  vis_by <- reactive(if (is.null(input$visium_by)) "cluster" else input$visium_by)
  xen_by <- reactive(if (is.null(input$xenium_color)) "class" else input$xenium_color)
  surv_by <- reactive(if (is.null(input$surv_group)) "stage" else input$surv_group)
  surv_gene <- reactive(if (is.null(input$surv_gene)) "TP53" else input$surv_gene)
  dot_scale <- reactive(if (is.null(input$dot_scale)) "gene" else input$dot_scale)
  upset_n <- reactive(if (is.null(input$upset_genes)) 8L else as.integer(input$upset_genes))
  violin_n <- reactive(if (is.null(input$violin_genes)) 8L else as.integer(input$violin_genes))
  pae_acc <- reactive(if (is.null(input$pae_uniprot)) "P04637" else input$pae_uniprot)
  pae_res <- reactive(if (is.null(input$pae_residue)) NA_integer_ else as.integer(input$pae_residue))
  pca_view <- reactive(if (is.null(input$pca_view)) "scores" else input$pca_view)
  pca_x <- reactive(if (is.null(input$pca_x)) 1L else as.integer(input$pca_x))
  pca_y <- reactive(if (is.null(input$pca_y)) 2L else as.integer(input$pca_y))
  pca_load_n <- reactive(if (is.null(input$pca_load_n)) 20L else as.integer(input$pca_load_n))

  # ---- VOLCANO -------------------------------------------------------------
  output$volcano_data <- reactive_output({
    v <- biov_volcano(fc = fc(), p = pcut())
    list(columns = list(x = v$logFC, y = v$neg_log10_p, label = v$gene))
  })
  output$volcano_png <- reactive_output({
    plot_volcano_gg(fc = fc(), p = pcut())
  })
  output$volcano_stats <- reactive_output({
    v <- biov_volcano(fc = fc(), p = pcut())
    tab <- table(v$status)
    list(n = length(v$gene), up = unname(tab["Up"]), down = unname(tab["Down"]))
  })

  # ---- HEATMAP -------------------------------------------------------------
  output$heatmap_data <- reactive_output({
    h <- biov_heatmap(n_genes = n_genes())
    list(
      columns = list(values = h$values),
      meta = list(nrows = h$nrows, ncols = h$ncols,
                  rowLabels = h$rowLabels, colLabels = h$colLabels)
    )
  })
  output$heatmap_png <- reactive_output({
    plot_heatmap_gg(n_genes = n_genes(), z_score = zscore())
  })

  # ---- CLUSTERMAP (clustered heatmap + dendrograms) ------------------------
  output$clustermap_data <- reactive_output({
    h <- biov_heatmap(n_genes = n_genes())
    list(
      columns = list(values = h$values),
      meta = list(nrows = h$nrows, ncols = h$ncols,
                  rowLabels = h$rowLabels, colLabels = h$colLabels)
    )
  })
  output$clustermap_png <- reactive_output({
    plot_clustermap_gg(n_genes = n_genes(), z_score = zscore())
  })

  # ---- HI-C ----------------------------------------------------------------
  # Rendered with the plotomics heatmap factory (a contact map IS a heatmap);
  # this avoids the hic factory's OES_texture_float requirement, unavailable in
  # current Chrome. Values are log1p-transformed so the structure pops.
  output$hic_data <- reactive_output({
    hic <- biov_hic()
    list(columns = list(values = log1p(hic$values)),
         meta = list(nrows = hic$n, ncols = hic$n))
  })
  output$hic_png <- reactive_output({ plot_hic_gg() })
  output$hic_stats <- reactive_output({
    hic <- biov_hic(); list(n = hic$n, chrom = hic$chrom)
  })

  # ---- TAHOE perturbation coverage (real Tahoe-100M data) ------------------
  output$tahoe_data <- reactive_output({
    t <- biov_tahoe()
    list(columns = list(values = t$values),
         meta = list(nrows = t$nrows, ncols = t$ncols,
                     rowLabels = t$rowLabels, colLabels = t$colLabels))
  })
  output$tahoe_png <- reactive_output({ plot_tahoe_gg() })
  output$tahoe_stats <- reactive_output({
    t <- biov_tahoe(); list(drugs = t$nrows, cells = t$ncols)
  })

  # ---- TREEMAP -------------------------------------------------------------
  output$treemap_data <- reactive_output({
    tm <- biov_treemap()
    list(
      columns = list(id = tm$id, parent = tm$parent, value = tm$value),
      meta = list(labels = tm$labels)
    )
  })
  output$treemap_png <- reactive_output({
    plot_treemap_gg()
  })

  # ---- UMAP ----------------------------------------------------------------
  # The React side fetches the binary blobs in www/data directly; the server
  # only renders the (deliberately capped) ggplot2 counterpart.
  output$umap_png <- reactive_output({
    uri <- plot_umap_gg(colour_by = umap_by())
    list(uri = unclass(uri), n = attr(uri, "n"), secs = attr(uri, "secs"))
  })

  # ---- STACKED VIOLIN ------------------------------------------------------
  # Densities are estimated once here on a per-gene grid; both engines draw
  # those curves rather than re-estimating with their own bandwidth.
  output$violin_data <- reactive_output({
    v <- biov_violin(violin_n())
    list(
      columns = list(feature = v$feature, group = v$cluster),
      meta = list(grid = v$grid, grids = v$grids, density = v$density,
                  features = v$genes, groups = v$clusters,
                  groupColors = v$clusterColors, median = v$median)
    )
  })
  output$violin_png <- reactive_output({
    plot_violin_gg(violin_n())
  })
  output$violin_stats <- reactive_output({
    v <- biov_violin(violin_n())
    list(genes = v$nGenes, clusters = v$nClusters, spots = v$nSpots,
         gridN = v$gridN, dataset = v$dataset)
  })

  # ---- UPSET ---------------------------------------------------------------
  # Intersection selection and ordering happen once in biov_upset(); both
  # engines draw those columns in that order.
  output$upset_data <- reactive_output({
    u <- biov_upset(upset_n())
    list(
      columns = list(size = u$size),
      meta = list(sets = u$sets, setSizes = u$setSizes,
                  membership = u$membership, total = u$total)
    )
  })
  output$upset_png <- reactive_output({
    plot_upset_gg(upset_n())
  })
  output$upset_stats <- reactive_output({
    u <- biov_upset(upset_n())
    list(total = u$total, altered = u$altered, unaltered = u$unaltered,
         intersections = u$nIntersections, shown = u$shown,
         pairs = u$pairs)
  })

  # ---- PCA EXPLORER --------------------------------------------------------
  # One decomposition, three views. The view is server-side state so the PNG
  # and the interactive component can never end up showing different panels.
  # The loadings view keys off the x-axis component, which is what makes
  # "which genes drive this axis" answerable without a second control.
  output$pca_data <- reactive_output({
    v <- pca_view()
    if (identical(v, "scree")) {
      s <- biov_pca_scree(10L)
      list(columns = list(value = s$value, label = s$label), meta = list())
    } else if (identical(v, "loadings")) {
      l <- biov_pca_loadings(pca_x(), pca_load_n())
      list(
        columns = list(value = l$value, label = l$label, group = l$group),
        meta = list(groups = l$groups,
                    groupColors = unname(biov_categorical(4)[c(1, 4)]))
      )
    } else {
      s <- biov_pca_scores(pca_x(), pca_y())
      list(columns = list(x = s$x, y = s$y, color = s$color, label = s$label))
    }
  })
  output$pca_png <- reactive_output({
    plot_pca_gg(pca_view(), pca_x(), pca_y(), pca_x(), pca_load_n())
  })
  output$pca_stats <- reactive_output({
    p <- biov_pca()
    s <- biov_pca_scree(10L)
    sc <- biov_pca_scores(pca_x(), pca_y())
    list(
      view = pca_view(), npc = p$npc, nGenes = length(p$genes),
      nSamples = length(p$samples),
      pcX = sc$pcX, pcY = sc$pcY,
      xLabel = sc$xLabel, yLabel = sc$yLabel,
      varX = round(100 * p$var_exp[sc$pcX], 1),
      varY = round(100 * p$var_exp[sc$pcY], 1),
      cum2 = round(s$cumulative[min(2L, length(s$cumulative))], 1)
    )
  })

  # ---- MARKER DOT PLOT -----------------------------------------------------
  # Gene ordering is the whole readability of this figure, so it is computed
  # once in biov_dotplot() and both engines plot that order.
  output$dotplot_data <- reactive_output({
    d <- biov_dotplot(dot_scale())
    list(
      columns = list(gene = d$gene, cluster = d$cluster,
                     pct = d$pct, value = d$value),
      meta = list(genes = d$genes, clusters = d$clusters,
                  valueLabel = d$valueLabel, sizeLabel = "% expressing")
    )
  })
  output$dotplot_png <- reactive_output({
    plot_dotplot_gg(dot_scale())
  })
  output$dotplot_stats <- reactive_output({
    d <- biov_dotplot(dot_scale())
    list(genes = d$nGenes, clusters = d$nClusters, spots = d$nSpots,
         dots = length(d$value), valueLabel = d$valueLabel,
         dataset = d$dataset)
  })

  # ---- SURVIVAL ------------------------------------------------------------
  # Every number on this page is estimated once here and sent to both engines:
  # the curves, the Greenwood band, the censoring times, the at-risk table, the
  # medians and the log-rank p.
  output$survival_data <- reactive_output({
    s <- biov_survival(surv_by(), surv_gene())
    list(
      columns = list(time = s$time, surv = s$surv, lower = s$lower,
                     upper = s$upper, group = s$group),
      meta = list(groups = s$levels, groupColors = s$colors,
                  censorTime = s$censorTime, censorSurv = s$censorSurv,
                  censorGroup = s$censorGroup,
                  riskTimes = s$riskTimes, riskCounts = s$riskCounts,
                  pLabel = s$pLabel)
    )
  })
  output$survival_png <- reactive_output({
    plot_km_gg(surv_by(), surv_gene())
  })
  output$survival_stats <- reactive_output({
    s <- biov_survival(surv_by(), surv_gene())
    list(n = s$n, events = s$nEvents, strata = length(s$levels),
         p = if (is.na(s$p)) NULL else signif(s$p, 3),
         levels = s$levels, counts = s$counts, eventsPer = s$events,
         # NA median means the curve never reached 50%, which is a real answer.
         medians = ifelse(is.na(s$medians), -1, round(s$medians, 1)),
         geneList = biov_survival_genes())
  })

  # ---- XENIUM --------------------------------------------------------------
  # Same split as the UMAP page: the React side streams the blobs itself, the
  # server only renders the capped ggplot2 counterpart.
  output$xenium_png <- reactive_output({
    uri <- plot_xenium_gg(colour_by = xen_by())
    list(uri = unclass(uri), n = attr(uri, "n"), secs = attr(uri, "secs"))
  })

  # ---- MANHATTAN + QQ (GWAS) -----------------------------------------------
  output$gwas_data <- reactive_output({
    g <- biov_gwas()
    list(
      columns = list(x = g$x, y = g$neglog10p, chr = g$chr),
      meta = list(chrCentres = g$chr_centres, chrBounds = g$chr_bounds,
                  genomeLen = g$genome_len, sig = g$sig, n = g$n)
    )
  })
  output$manhattan_png <- reactive_output({ plot_manhattan_gg() })
  output$qq_data <- reactive_output({
    q <- biov_qq(); list(columns = list(x = q$expected, y = q$observed), meta = list(lambda = q$lambda))
  })
  output$qq_png <- reactive_output({ plot_qq_gg() })

  # ---- eQTL / pQTL ---------------------------------------------------------
  output$eqtl_data <- reactive_output({
    e <- biov_eqtl()
    list(columns = list(values = e$values),
         meta = list(nrows = e$nrows, ncols = e$ncols,
                     rowLabels = e$rowLabels, colLabels = e$colLabels))
  })
  output$eqtl_png <- reactive_output({ plot_eqtl_gg() })

  # ---- scATAC coverage-by-cluster ------------------------------------------
  output$atac_data <- reactive_output({
    a <- biov_atac()
    list(
      columns = list(signal = a$signal),
      meta = list(nClusters = a$n_clusters, nBins = a$n_bins, positions = a$positions,
                  clusters = a$clusters, chrom = a$chrom, start = a$start, end = a$end)
    )
  })
  output$atac_png <- reactive_output({ plot_atac_gg() })

  # ---- N-D array (hyperspectral) -------------------------------------------
  # React fetches the cube blob directly; the server renders the ggplot slice +
  # spectrum for the same channel / probe pixel the client selects.
  output$nd_meta <- reactive_output({
    nd <- biov_ndarray(); list(ny = nd$ny, nx = nd$nx, nch = nd$nch)
  })
  output$nd_slice_png <- reactive_output({ plot_ndslice_gg(nd_ch()) })
  output$nd_spectrum_png <- reactive_output({ plot_ndspectrum_gg(nd_px(), nd_py()) })

  # ---- IGV -----------------------------------------------------------------
  output$igv_genes <- reactive_output({ biov_mutation_genes() })
  output$igv_config <- reactive_output({ biov_igv_config(igv_gene()) })
  output$igv_needle_png <- reactive_output({ plot_igv_needle_gg(igv_gene()) })

  # ---- NETWORK -------------------------------------------------------------
  output$network_data <- reactive_output({
    net <- biov_network()
    list(
      columns = list(
        id = net$id, x = net$x, y = net$y, size = net$size,
        source = net$source, target = net$target
      ),
      meta = list(nodeGroup = net$group, nodeLabels = net$id)
    )
  })
  output$network_png <- reactive_output({
    uri <- plot_network_gg()
    list(uri = unclass(uri), nodes = attr(uri, "nodes"),
         edges = attr(uri, "edges"), secs = attr(uri, "secs"))
  })
  output$network_stats <- reactive_output({
    net <- biov_network()
    list(nodes = net$n_nodes, edges = net$n_edges)
  })

  # ---- PROTEIN (classic pLDDT profile; React side is 3Dmol) ----------------
  output$protein_plddt_png <- reactive_output({
    plot_protein_plddt_gg(prot_acc(), prot_res())
  })

  # ---- ONCOPLOT ------------------------------------------------------------
  # The memo sort, per-sample burden and per-gene frequency are all computed in
  # biov_oncoplot() and shipped as-is. The React component is a renderer: it
  # indexes, it does not sort or aggregate, so the two engines cannot drift
  # apart on a tie.
  output$oncoplot_data <- reactive_output({
    o <- biov_oncoplot(n_genes = onco_n())
    list(
      columns = list(codes = o$codes, tmb = o$tmb, freq = o$freq),
      meta = list(nrows = o$nrows, ncols = o$ncols,
                  genes = o$genes, samples = o$samples,
                  classes = o$classes, classColors = o$classColors,
                  annotations = o$annotations)
    )
  })
  output$oncoplot_png <- reactive_output({ plot_oncoplot_gg(n_genes = onco_n()) })
  output$oncoplot_stats <- reactive_output({
    o <- biov_oncoplot(n_genes = onco_n())
    list(genes = o$nrows, samples = o$ncols, cohort = o$cohort,
         altered = o$altered, events = sum(o$tmb),
         medianTmb = stats::median(o$tmb))
  })

  # ---- VISIUM spatial transcriptomics --------------------------------------
  # The selected gene's per-spot vector is computed here and sent, so the canvas
  # never derives expression itself and the two engines cannot disagree.
  output$visium_data <- reactive_output({
    v <- biov_visium(vis_gene(), vis_by())
    colour <- if (identical(vis_by(), "gene")) v$expr else v$cluster
    list(
      columns = list(x = v$x, y = v$y, color = colour, label = v$barcode),
      meta = list(image = v$image, imgWidth = v$imgWidth,
                  imgHeight = v$imgHeight, spotDiameter = v$spotDiameter,
                  levels = if (identical(vis_by(), "gene")) NULL else v$clusterLevels,
                  colors = if (identical(vis_by(), "gene")) NULL else v$clusterColors)
    )
  })
  output$visium_png <- reactive_output({ plot_visium_gg(vis_gene(), vis_by()) })
  output$visium_stats <- reactive_output({
    v <- biov_visium(vis_gene(), vis_by())
    list(spots = v$nSpots, genes = v$nGenes,
         clusters = length(v$clusterLevels), gene = v$gene,
         geneList = v$genes, exprMax = round(v$exprMax, 2),
         dataset = v$dataset)
  })

  # ---- MUTATIONAL SIGNATURES (SBS96) ---------------------------------------
  output$sbs_data <- reactive_output({
    s <- biov_sbs96_profile(sbs_which())
    list(
      columns = list(value = s$value, group = s$sub, label = s$trinuc),
      meta = list(groups = s$subLevels, groupColors = s$subColors,
                  title = if (s$isCatalogue) "Observed cohort catalogue"
                          else paste("De novo signature", s$profile))
    )
  })
  output$sbs_png <- reactive_output({ plot_sbs96_gg(sbs_which()) })
  output$sbs_stats <- reactive_output({
    s <- biov_sbs96_profile(sbs_which())
    list(profile = s$profile, choices = s$choices, yLabel = s$yLabel,
         isCatalogue = s$isCatalogue, tumours = s$nTumours, snv = s$nSnv,
         share = s$share)
  })

  # ---- PROTEIN DOMAIN LOLLIPOP ---------------------------------------------
  # labelIndex is resolved in biov_lollipop() and sent to the client, so the
  # canvas and ggrepel label the same variants rather than each picking a top-N.
  output$lollipop_genes <- reactive_output({ I(biov_lollipop_genes()) })
  output$lollipop_data <- reactive_output({
    l <- biov_lollipop(lolli_gene())
    if (is.null(l)) return(NULL)
    list(
      columns = list(position = l$position, count = l$count,
                     class = l$class, label = l$label),
      meta = list(length = l$length, gene = l$gene, uniprot = l$uniprot,
                  classes = l$classes, classColors = l$classColors,
                  domains = l$domains, domainColors = l$domainColors,
                  ptms = l$ptms, labelIndex = l$labelIndex)
    )
  })
  output$lollipop_png <- reactive_output({ plot_lollipop_gg(lolli_gene()) })
  output$lollipop_stats <- reactive_output({
    l <- biov_lollipop(lolli_gene())
    if (is.null(l)) return(NULL)
    list(gene = l$gene, uniprot = l$uniprot, length = l$length,
         variants = l$nVariants, samples = l$nSamples,
         domains = length(l$domains), ptms = length(l$ptms))
  })

  # ---- ALPHAFOLD PAE -------------------------------------------------------
  # Both engines read the SAME binned matrix from biov_pae(); the client never
  # talks to AlphaFold directly, so it cannot end up plotting the unbinned one.
  output$pae_data <- reactive_output({
    p <- biov_pae(pae_acc())
    if (is.null(p)) return(NULL)
    list(columns = list(values = p$values),
         meta = list(nrows = p$nrows, ncols = p$ncols,
                     rowLabels = p$rowLabels, colLabels = p$colLabels))
  })
  output$pae_profile_data <- reactive_output({
    p <- biov_pae(pae_acc())
    if (is.null(p)) return(NULL)
    pos <- as.integer(p$rowLabels)
    res <- pae_res()
    i <- if (is.null(res) || is.na(res)) 1L else which.min(abs(pos - res))
    list(columns = list(values = unname(p$matrix[i, ])),
         meta = list(residue = pos[i], maxPae = p$maxPae))
  })
  output$pae_png <- reactive_output({ plot_pae_gg(pae_acc(), pae_res()) })
  output$pae_profile_png <- reactive_output({
    plot_pae_profile_gg(pae_acc(), pae_res())
  })
  output$pae_stats <- reactive_output({
    p <- biov_pae(pae_acc())
    if (is.null(p)) return(list(ok = FALSE))
    list(ok = TRUE, residues = p$residues, binned = p$nrows, bin = p$bin,
         cells = p$residues * p$residues, maxPae = p$maxPae,
         mean = round(mean(p$matrix), 2))
  })

  # ---- CHAT ASSISTANT (bring-your-own-key, advisory only) ------------------
  # The React chat box sends a request { id, provider, key, model, message }
  # when a key is set; we build a per-session ellmer client (rebuilt when the
  # provider/key/model change), call it, and publish { id, text | error }. With
  # no key the React side answers from its own knowledge base and never calls
  # here. The client and key live only in this session's memory.
  chat_state <- new.env(parent = emptyenv())
  chat_state$client <- NULL
  chat_state$provider <- NULL
  chat_state$model <- NULL
  chat_state$key <- NULL
  chat_state$turns <- 0L
  chat_reply <- reactiveVal(NULL)

  observeEvent(input$chat_request, {
    reqd <- input$chat_request
    if (is.null(reqd) || is.null(reqd$id)) return()
    id <- reqd$id
    fail <- function(text) chat_reply(list(id = id, error = text))
    msg <- reqd$message %||% ""
    if (!nzchar(trimws(msg))) return()

    if (!requireNamespace("ellmer", quietly = TRUE)) {
      return(fail("The chat backend (ellmer) is not installed on this server."))
    }
    provider <- reqd$provider %||% "gemini"
    if (!provider %in% BIOV_CHAT_PROVIDERS) provider <- "gemini"
    model <- trimws(reqd$model %||% "")
    key <- trimws(reqd$key %||% "")
    if (!nzchar(key)) key <- biov_chat_env_key(provider)
    if (!nzchar(key)) {
      return(fail("No API key set. Paste a provider key to chat with the model."))
    }

    # (Re)build the client when the provider, key or model changes; a new client
    # starts a fresh conversation, so the turn counter resets with it.
    if (!identical(provider, chat_state$provider) ||
        !identical(model, chat_state$model) ||
        !identical(key, chat_state$key) ||
        is.null(chat_state$client)) {
      cl <- tryCatch(biov_chat_build(provider, key, model), error = function(e) e)
      if (inherits(cl, "condition")) {
        return(fail(biov_chat_friendly_error(conditionMessage(cl), key)))
      }
      chat_state$client <- cl
      chat_state$provider <- provider
      chat_state$model <- model
      chat_state$key <- key
      chat_state$turns <- 0L
    }

    if (chat_state$turns >= 25L) {
      return(fail("Turn limit reached for this session. Reload the page to start a new conversation."))
    }
    chat_state$turns <- chat_state$turns + 1L

    reply <- tryCatch(
      as.character(chat_state$client$chat(msg, echo = "none")),
      error = function(e) structure("", condition = conditionMessage(e))
    )
    cond <- attr(reply, "condition")
    if (!is.null(cond)) {
      return(fail(biov_chat_friendly_error(cond, key)))
    }
    chat_reply(list(id = id, text = paste(reply, collapse = "\n")))
  })

  output$chat_response <- reactive_output({ chat_reply() })
}

shinyApp(ui = ui, server = server)
