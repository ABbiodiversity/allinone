#' Helpers
#'Helper functionsDownload, load, unload coefficients.

#' @name helper
NULL

#' @export
#' @rdname helper
ai_species <- function() {
    out <- NULL
    for (i in names(.ai1$COEFS)) {
        out <- rbind(out, .ai1$COEFS[[i]]$species)
    }
    out
}
