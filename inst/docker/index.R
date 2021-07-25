#!/usr/bin/env Rscript

cat("[INFO] This is the All-in-one API\n")

suppressPackageStartupMessages({
    library(httpuv)
    library(jsonlite)
    library(allinone)
    library(sf)
    library(raster)
})
cat("[INFO] Packages loaded\n")

PORT <- 8080
dir <- if (interactive())
    "~/repos/allinone-coefs" else "allinone-coefs"

vn <- c("AHM", "FFP", "MAP", "MAT", "MCMT", "MWMT",
    "NSR1CentralMixedwood", "NSR1DryMixedwood", "NSR1Foothills",
    "NSR1Mountain", "NSR1North", "NSR1Parkland", "NSR1Shield",
    "pAspen", "PeaceRiver", "PET", "pWater", "wN")
rl <- list()
for (j in vn) {
    rl[[j]] <- suppressWarnings(raster(
        file.path(dir, "spatial", paste0(j, ".tif"))))
}
rl <- stack(rl)
cat("[INFO] Raster stack loaded\n")

crsll <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crstm <- "+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs"

ai_load_coefs(file.path(dir, "v2020"))

workhorse <- function(x) {
    XY <- data.frame(POINT_X=x$long, POINT_Y=x$lat)
    xy <- sf::st_as_sf(XY,coords = c("POINT_X", "POINT_Y"))
    xy <- st_set_crs(xy, crsll)
    xy <- st_transform(xy, crstm)
    e <- extract(rl, xy)
    z <- ai_predict(x$spp,
        spclim=data.frame(XY, e),
        veghf=x$veghf,
        soilhf=x$soilhf,
        i=x$i)
    if (!is.null(z$north) && !is.null(z$south))
        out <- e[,"wN"] * z$north + (1-e[,"wN"]) * z$south
    if (is.null(z$north) && !is.null(z$south))
        out <- z$south
    if (!is.null(z$north) && is.null(z$south))
        out <- z$north
    if (is.null(z$north) && is.null(z$south))
        out <- numeric(0)
    out
}

handle <- function(req) {
    input <- req[["rook.input"]]
    postdata <- jsonlite::fromJSON(input$read_lines())
    jsonlite::toJSON(workhorse(postdata))
}
cat(sprintf("[INFO] %s Starting server: http://0.0.0.0:%s\n", Sys.time(), PORT))

httpuv::runServer(
    host = "0.0.0.0",
    port = PORT,
    app = list(
        call = function(req) {
            out <- try(handle(req))
            if (inherits(out, "try-error")) {
                list(
                    status = 422L,
                    headers = list('Content-Type' = 'application/json'),
                    body = jsonlite::toJSON(list(
                        code="422 - Unprocessable Entity",
                        message=as.character(out))))
            } else {
                list(
                    status = 200L,
                    headers = list('Content-Type' = 'application/json'),
                    body = out)
            }
        }
    )
)

