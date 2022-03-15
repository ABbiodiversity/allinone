.onAttach <- function(libname, pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname),
                    fields=c("Version", "Date"))
    packageStartupMessage(paste(pkgname, ver[1], "\t", ver[2]))
    if (is.null(getOption("allinone"))) {
        options("allinone" = list(
            url="https://abbiodiversity.github.io/allinone-coefs/v2022",
            dir=".",
            verbose=1))
    }
    invisible(NULL)
}

.onUnload <- function(libpath){
    options("allinone" = NULL)
    invisible(NULL)
}
