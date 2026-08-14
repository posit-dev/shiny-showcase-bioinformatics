# Advisory chat backend for the Plotomics Live guide (bring-your-own-key).
#
# The chat box in the UI is a React component; this file is only the server
# side. It builds an ellmer Chat client from a user-supplied API key and answers
# questions about the app and its visualizations. It is advisory only: no tools
# are registered, so the assistant cannot change any input or control the UI.
#
# The key is passed per request from the client, used to build a per-session
# ellmer client, and never written to disk. It is held in server memory only for
# the life of the session and redacted from any surfaced error. When no key is
# set the React side answers from its built-in knowledge base instead, so the
# app stays useful offline. Pattern adapted from the lifescience-shiny-gallery
# byok-chat module, minus its bslib / shinychat UI.

BIOV_CHAT_PROVIDERS <- c("gemini", "openai", "anthropic")

# provider -> ellmer constructor
.biov_chat_ctor <- function(provider) {
  switch(
    provider,
    gemini = ellmer::chat_google_gemini,
    openai = ellmer::chat_openai,
    anthropic = ellmer::chat_anthropic,
    NULL
  )
}

# provider -> environment variables to fall back on (operator-funded mode)
.biov_chat_env <- list(
  gemini = c("GEMINI_API_KEY", "GOOGLE_API_KEY"),
  openai = "OPENAI_API_KEY",
  anthropic = "ANTHROPIC_API_KEY"
)

# First non-empty env var for a provider, or "" if none.
biov_chat_env_key <- function(provider) {
  for (v in .biov_chat_env[[provider]] %||% character()) {
    val <- Sys.getenv(v, "")
    if (nzchar(val)) return(val)
  }
  ""
}

# Replace the key with a marker anywhere it appears, so it cannot leak through a
# surfaced or logged error message.
biov_chat_redact <- function(msg, key) {
  msg <- paste(as.character(msg), collapse = "\n")
  if (nzchar(key)) msg <- gsub(key, "<redacted-key>", msg, fixed = TRUE)
  msg
}

# Map a raw provider error to a short, safe message. Classification runs on the
# raw text; the returned text is redacted.
biov_chat_friendly_error <- function(raw, key = "") {
  low <- tolower(paste(as.character(raw), collapse = " "))
  msg <- if (grepl("401|403|invalid.*key|unauthor|permission denied|api key", low)) {
    "The provider rejected that API key. Check the key and that it matches the selected provider."
  } else if (grepl("402|billing|credit|payment|insufficient", low)) {
    "The provider reports a billing or credit problem on this key."
  } else if (grepl("429|rate.?limit|too many requests|quota", low)) {
    "Rate limited by the provider. Wait a moment and try again."
  } else if (grepl("503|overloaded|unavailable|timeout|timed out|econnrefused|could not resolve", low)) {
    "The provider is unreachable or temporarily unavailable. Check your connection and try again."
  } else if (grepl("404|not found|no such model|unknown model|does not exist", low)) {
    "That model was not found for this provider. Leave the model field blank to use the provider default."
  } else {
    paste0("Chat error: ", substr(biov_chat_redact(raw, key), 1, 300))
  }
  biov_chat_redact(msg, key)
}

# The hard-scoped system prompt: who the assistant is, what it may do, and a
# compact catalogue of the 26 visualizations so it can explain and recommend
# them without inventing pages. Advisory only.
biov_chat_system_prompt <- function() {
  paste(
    "You are the Plotomics Live guide, embedded in a gallery web app that shows",
    "26 biological-data visualizations, each rendered two ways: a classic",
    "ggplot2 image and an interactive Shiny React (TSX / WebGL) component built",
    "with the plotomics component library. The React engine stays interactive on",
    "very large data (for example ~584k cells) where a static plot cannot.",
    "",
    "Your job: explain the app, describe any visualization, give a rough sense of",
    "how much each can render, and recommend which visualization fits a user's",
    "data. You are ADVICE ONLY: you cannot change any control, load data, or",
    "drive the UI. If asked to do so, say you can only guide, and name the page",
    "and control the user should use. Be concise and honest; never invent gene",
    "names, numbers, datasets, or pages. If a question is outside this app or",
    "general bioinformatics visualization, say it is out of scope. All datasets",
    "are public and referenced on the About page; seven visualizations use seeded",
    "synthetic data (network, Hi-C, Manhattan/GWAS, scATAC, eQTL, N-dimensional",
    "array, gosling) and you should say so if asked.",
    "",
    "The 26 visualizations, grouped into five analysis areas (route in brackets):",
    "",
    "Single-cell & spatial:",
    "- Single-cell UMAP (/umap): ~584k-cell embedding coloured by cell type/organ.",
    "- Tahoe-100M perturbation (/tahoe): drug x cell-line coverage matrix plus a 380k-cell cell-cycle scatter.",
    "- Visium spatial transcriptomics (/visium): capture spots over an H&E section, by cluster or gene.",
    "- Xenium single-molecule transcripts (/xenium): ~1M mRNA molecules at micrometre coordinates.",
    "- Marker gene dot plot (/dotplot): gene x cluster, dot size = percent expressing, colour = level.",
    "- Stacked violin (/violin): per-cluster expression distributions, one row per gene.",
    "",
    "Gene expression:",
    "- Volcano plot (/volcano): log2 fold-change vs -log10 p for differential expression.",
    "- Expression heatmap (/heatmap): gene x sample matrix, optionally z-scored.",
    "- Clustered heatmap (/clustermap): the same matrix with hierarchical clustering and dendrograms.",
    "- PCA explorer (/pca): sample scores, scree, and loadings of one decomposition.",
    "",
    "Cancer genomics:",
    "- Oncoplot / OncoPrint (/oncoplot): gene x sample alteration grid with burden, frequency and clinical strips.",
    "- Domain lollipop (/lollipop): variants along a protein over its Pfam domains, with PTM sites.",
    "- Mutation treemap (/treemap): gene-to-variant hierarchy sized by recurrence.",
    "- Mutational signatures SBS96 (/signatures): the 96 trinucleotide contexts and de novo signatures.",
    "- Driver co-occurrence UpSet (/upset): exclusive set intersections of altered drivers.",
    "- Kaplan-Meier survival (/survival): survival curves per stratum with a number-at-risk table.",
    "",
    "Genome & epigenome:",
    "- Manhattan + QQ GWAS (/manhattan): genome-wide -log10 p with a significance line, plus a Q-Q plot.",
    "- eQTL / pQTL effect map (/eqtl): variant x gene effect-size (beta) heatmap, diverging around zero.",
    "- Genome browser IGV (/igv): a live igv.js browser plus a variant needle plot.",
    "- Gosling genome view (/gosling): a declarative JSON-spec genome track.",
    "- Hi-C contact matrix (/hic): a genomic contact matrix with TADs and loops.",
    "- Single-cell ATAC coverage (/atac): per-cluster pseudobulk accessibility tracks.",
    "",
    "Structure & networks:",
    "- Protein structure (/protein): interactive 3D AlphaFold structure coloured by pLDDT.",
    "- AlphaFold PAE matrix (/pae): residue x residue predicted aligned error.",
    "- Gene network (/network): a node-link graph with community structure.",
    "- N-dimensional array viewer (/ndarray): a slice of an N-D image cube with a per-pixel spectrum.",
    sep = "\n"
  )
}

# Build a per-session ellmer Chat for a provider + key. Errors (bad provider,
# construction failure) propagate to the caller, which maps them for display.
biov_chat_build <- function(provider, key, model = "",
                            system_prompt = biov_chat_system_prompt(),
                            temperature = 0.2, max_tokens = 1024L) {
  ctor <- .biov_chat_ctor(provider)
  if (is.null(ctor)) stop("Unknown chat provider: ", provider)
  args <- list(
    system_prompt = system_prompt,
    api_key = key,
    params = ellmer::params(temperature = temperature, max_tokens = max_tokens),
    echo = "none"
  )
  if (nzchar(model)) args$model <- model
  do.call(ctor, args)
}
