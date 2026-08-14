# Connectors page: a reference catalog of every data source GeneScout can pull a
# signal from (see R/connectors.R). It is static - built once at startup from the
# live source catalog - so no server wiring is needed; new registered connectors
# appear here automatically.
connectors_page <- gs_page(render_connectors_page())
