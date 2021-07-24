#' Helpers
#' Helper functions to list species and vegetation/soil/footprint classes.

#' @name helper
NULL

#' @export
#' @rdname helper
ai_species <- function() {
    if (!.loaded())
        stop("Use ai_load_coefs() to load coefs")
    out <- NULL
    for (taxon in names(.ai1$COEFS)) {
        tmp <- .ai1$COEFS[[taxon]]$species
        tmp <- tmp[tmp$ModelNorth | tmp$ModelSouth,]
        out <- rbind(out, tmp)
    }
    out
}

#' @export
#' @rdname helper
ai_classes <- function() {
    .lt()
}
