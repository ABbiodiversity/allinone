#' Helpers
#' Helper functions to list species and vegetation/soil/footprint classes.

#' @name helper
NULL

#' @export
#' @rdname helper
ai_species <- function() {
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
