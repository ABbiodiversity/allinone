#' Coefficients
#'
#' Download, load, unload coefficients.

#' @param dir Directory path.
#' @param ... Arguments parameters passed to [utils::download.file()].
#'
#' @return
#'
#' `ai_dowload_coefs` dowloads a file as a side effect.
#'
#' `ai_load_coefs` loads the coefficients into the current session.
#'
#' `ai_unload_coefs` unloads the coefficients from the current session.
#'
#' @seealso [utils::download.file()].
#'
#' @name coefs
NULL

#' @export
#' @rdname coefs
ai_dowload_coefs <- function(dir=NULL, ...) {
    if (is.null(dir))
        dir <- getOption("allinone")$dir
    fn <- file.path(getOption("allinone")$url, "COEFS.RData")
    fo <- file.path(dir, "COEFS.RData")
    if (file.exists(fo))
        .msg("File already exists", 2)
    .msg("Downloading coefs")
    utils::download.file(
        url=fn,
        destfile=fo, ...)
}

#' @export
#' @rdname coefs
ai_load_coefs <- function(dir=NULL) {
    if (.loaded())
        invisible(NULL)
    if (is.null(dir))
        dir <- getOption("allinone")$dir
    fn <- file.path(dir, "COEFS.RData")
    .msg("Loading coefs")
    out <- try(load(fn, envir=.ai1))
    if (inherits(out, "try-error"))
        stop("Use ai_download_coefs() to download coefs")
    invisible(out)
}

#' @export
#' @rdname coefs
ai_unload_coefs <- function() {
    if (.loaded())
        .msg("Unloading coefs")
    rm(list=ls(envir=.ai1), envir=.ai1)
}
