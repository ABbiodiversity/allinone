library(allinone)
options("allinone" = list(
    url="https://abbiodiversity.github.io/allinone-coefs/v2020",
    dir=".",
    verbose=0))
#ai_dowload_coefs()
ai_load_coefs()
tab <- ai_species()
load(system.file("extdata/example.RData", package="allinone"))

check_fun <- function(spp, i) {
    z1 <- ai_predict(spp,
      spclim=spclim,
      veghf=p_veghf,
      soilhf=p_soilhf,
      i=i)
    z2 <- ai_predict(spp,
      spclim=spclim,
      veghf=spclim$veghf,
      soilhf=spclim$soilhf,
      i=i)
    invisible()
}

tab$OK <- TRUE
#tab <- tab[tab$Group=="mammals",]
dim(tab)
for (j in 1:nrow(tab)) {
    spp <- tab$SpeciesID[j]
    taxon <- tab$Group[j]
    cat(sprintf("%s\t%s (%s) ... ", j, spp, taxon))
    flush.console()
    x1 <- try(check_fun(spp, 1))
    if (inherits(x1, "try-error"))
        tab$OK[j] <- FALSE
    if (!(taxon %in% c("mammals", "habitats"))) {
        x2 <- try(check_fun(spp, 2))
        if (inherits(x2, "try-error"))
            tab$OK[j] <- FALSE
    }
    cat(ifelse(tab$OK[j], "OK", "ERROR"), "\n")
}
table(tab$Group, tab$OK)


if (FALSE) {
ai_load_coefs()

.ai1 <- list(COEFS=allinone:::.ai1$COEFS)
source("R/internals.R")
source("R/predict.R")
check_fun("Coyote", 1)
check_fun("Ovenbird", 1)

names(allinone:::.ai1)

spp <- "Coyote"
i <- 1
    z1 <- ai_predict(spp,
      spclim=spclim,
      veghf=p_veghf,
      soilhf=p_soilhf,
      i=i)
}
