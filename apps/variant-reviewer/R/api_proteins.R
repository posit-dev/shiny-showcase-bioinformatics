# EBI Proteins API client: UniProt sequence features (domains, regions, sites)
# for an accession, used to place a variant within the protein's architecture.
# Docs: https://www.ebi.ac.uk/proteins/api/doc/

PROTEINS_BASE <- "https://www.ebi.ac.uk/proteins/api"

# Feature types worth showing for "which domain/site is the variant in".
PROTEINS_FEATURE_TYPES <- c(
  "DOMAIN",
  "REGION",
  "MOTIF",
  "REPEAT",
  "ZN_FING",
  "DNA_BIND",
  "CA_BIND",
  "NP_BIND",
  "ACT_SITE",
  "BINDING",
  "SITE"
)

# Human-readable labels for the terse UniProt feature-type codes.
proteins_type_label <- function(type) {
  switch(
    type,
    DOMAIN = "Domain",
    REGION = "Region",
    MOTIF = "Motif",
    REPEAT = "Repeat",
    ZN_FING = "Zinc finger",
    DNA_BIND = "DNA binding",
    CA_BIND = "Calcium binding",
    NP_BIND = "Nucleotide binding",
    ACT_SITE = "Active site",
    BINDING = "Binding site",
    SITE = "Site",
    type
  )
}

# An empty feature frame with the columns the parser guarantees.
proteins_empty_features <- function() {
  data.frame(
    type = character(0),
    label = character(0),
    description = character(0),
    begin = integer(0),
    end = integer(0),
    stringsAsFactors = FALSE
  )
}

# Sequence features for a UniProt accession.
# Returns:
#   list(ok = TRUE, accession,
#        features = data.frame(type, label, description, begin, end))
#   list(ok = FALSE, error = "...")
proteins_features <- function(accession, types = PROTEINS_FEATURE_TYPES) {
  if (is_blank(accession)) {
    return(list(ok = FALSE, error = "No UniProt accession for feature lookup."))
  }
  res <- vr_api_get(
    PROTEINS_BASE,
    path = paste0("features/", accession),
    query = list(types = paste(types, collapse = ",")),
    source = "EBI Proteins"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  proteins_parse_features(res$data, accession)
}

# Pure parser: the features array -> a tidy data frame sorted by start position.
proteins_parse_features <- function(data, accession = NA_character_) {
  feats <- pluck_at(data, "features")
  if (is.null(feats) || length(feats) == 0) {
    return(list(
      ok = TRUE,
      accession = accession,
      features = proteins_empty_features()
    ))
  }
  rows <- lapply(feats, function(f) {
    type <- as.character(pluck_at(f, "type", default = NA))
    data.frame(
      type = type,
      label = proteins_type_label(type),
      description = as.character(pluck_at(f, "description", default = "")),
      begin = suppressWarnings(as.integer(pluck_at(f, "begin", default = NA))),
      end = suppressWarnings(as.integer(pluck_at(f, "end", default = NA))),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  df <- df[order(df$begin), , drop = FALSE]
  rownames(df) <- NULL
  list(ok = TRUE, accession = accession, features = df)
}

# Rows of a feature frame that span a residue position (begin <= pos <= end).
proteins_features_at <- function(df, position) {
  empty <- if (is.data.frame(df)) {
    df[0, , drop = FALSE]
  } else {
    proteins_empty_features()
  }
  if (!is.data.frame(df) || nrow(df) == 0 || is_blank(position)) {
    return(empty)
  }
  pos <- suppressWarnings(as.integer(position))
  if (is.na(pos)) {
    return(empty)
  }
  df[
    !is.na(df$begin) & !is.na(df$end) & df$begin <= pos & df$end >= pos,
    ,
    drop = FALSE
  ]
}

# A compact "Description (range)" label for one feature row.
proteins_feature_text <- function(df, i) {
  desc <- if (nzchar(df$description[i])) df$description[i] else df$label[i]
  range <- if (!is.na(df$begin[i]) && df$begin[i] == df$end[i]) {
    as.character(df$begin[i])
  } else {
    paste0(df$begin[i], "–", df$end[i])
  }
  paste0(desc, " (", range, ")")
}
