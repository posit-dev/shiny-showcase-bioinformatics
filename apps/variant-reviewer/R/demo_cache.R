# Warm cache for the demo example.
#
# The navbar Demo button loads BRAF V600E. Cold, that is ~16 sequential API
# calls and a slow first search, which is a bad look mid-presentation. So we ship
# a recorded snapshot of those responses (made by dev/record_demo_cache.R) and
# seed the in-process cache from it, letting the demo load from memory instead of
# the network. The progress popup still animates; the sources just arrive fast.
#
# The snapshot is a named list of vr_cache entries: each name is the exact cache
# key (a content hash of the request) and each value is the normalized result
# list vr_perform() returns. Because the key is a hash of the request itself, a
# recorded entry is used only when the request is byte-for-byte identical, so a
# snapshot stays valid until the example, an endpoint, or a query parameter
# changes. Re-record with dev/record_demo_cache.R when any of those move.

# Where the snapshot lives. Kept at the app root so it ships in the deploy
# bundle; .rscignore does not exclude root files.
VR_DEMO_CACHE_FILE <- "demo_cache.rds"

# Seed vr_cache from the snapshot when it exists. Idempotent and cheap (a file
# read plus a handful of $set calls), so it is safe to run at startup and again
# each time the demo is launched. Re-running also refreshes entries that the TTL
# or LRU eviction may have dropped on a long-running server, so the demo stays
# warm however long the process has been up. Returns the number of entries
# seeded, invisibly.
vr_seed_demo_cache <- function(file = VR_DEMO_CACHE_FILE) {
  if (!file.exists(file)) {
    return(invisible(0L))
  }
  snapshot <- tryCatch(readRDS(file), error = function(e) NULL)
  if (!is.list(snapshot) || length(snapshot) == 0) {
    return(invisible(0L))
  }
  for (key in names(snapshot)) {
    vr_cache$set(key, snapshot[[key]])
  }
  invisible(length(snapshot))
}
