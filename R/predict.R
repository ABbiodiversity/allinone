#' Predict
#'
#' Predict based on input data.

#' @param spp Stecies ID.
#' @param spclim Space climate data as a data frame.
#' @param veghf,soilhf Vegetation/Soil and footprint info as
#'   (1) a vector of land cover classes or as a
#'   (2) composition matrix with land cover classes as columns.
#' @param i Bootstrap ID (1-100).
#'
#' @return
#'
#' Stuff.
#'
#' @name preds
NULL

#' @export
#' @rdname preds
#' @import Matrix
## ai_predict_class and ai_predict_comp
ai_predict <- function(spp, spclim, veghf=NULL, soilhf=NULL, i=1) {

    ## We need Matrix pkg loaded and not just import
    ## for all the math methods to work
    TMP <- Matrix(sparse=TRUE)

    if (!is.null(veghf)) {
        if (is.null(dim(veghf))) {
            p_veghf <- methods::as(stats::model.matrix(~x-1,
                data.frame(x=as.character(veghf))), "dgCMatrix")
            colnames(p_veghf) <- substr(colnames(p_veghf), 2, nchar(colnames(p_veghf)))
        } else {
            p_veghf <- veghf
        }
    } else {
        p_veghf <- NULL
    }
    if (!is.null(soilhf)) {
        if (is.null(dim(soilhf))) {
            p_soilhf <- methods::as(stats::model.matrix(~x-1,
                data.frame(x=as.character(soilhf))), "dgCMatrix")
            colnames(p_soilhf) <- substr(colnames(p_soilhf), 2, nchar(colnames(p_soilhf)))
        } else {
            p_soilhf <- soilhf
        }
    } else {
        p_soilhf <- NULL
    }

    ## pull out N/S species list based on taxon
    taxon <- .get_taxon(spp)

    if (taxon %in% c("mammals", "habitats") && i > 1)
        .msg(sprintf("Bootstrap .ai1$COEFS not available for species %s (%s)", spp, taxon), 4)
    .msg(sprintf("Making predictions for species %s (%s)", spp, taxon))
    if (i > 100)
        .msg("Bootstrap ID cannot be > 100", 4)
    if (i < 1)
        .msg("Bootstrap ID cannot be < 1", 4)
    i <- as.integer(i)

    if (taxon == "mammals") {
        TAB <- .ai1$COEFS$mammals$species
        rownames(TAB) <- TAB$SpeciesID
        SPPn <- rownames(TAB)[TAB$ModelNorth]
        SPPs <- rownames(TAB)[TAB$ModelSouth]
    } else {
        SPPn <- if (taxon != "birds")
            dimnames(.ai1$COEFS[[taxon]]$north)[[1]] else dimnames(.ai1$COEFS[[taxon]]$north$joint)[[1]]
        SPPs <- if (taxon != "birds")
            dimnames(.ai1$COEFS[[taxon]]$south)[[1]] else dimnames(.ai1$COEFS[[taxon]]$south$joint)[[1]]
    }

    ## determine what models to do for spp
    type <- "C" # combo species (N+S)
    M <- list(N=spp %in% SPPn, S=spp %in% SPPs)
    if (M$N & !M$S)
        type <- "N"
    if (!M$N & M$S)
        type <- "S"
    if (taxon == "birds") {
        cfn <- if (type == "S")
            NULL else .ai1$COEFS[[taxon]]$north$joint[spp,,]
        cfs <- if (type == "N")
            NULL else .ai1$COEFS[[taxon]]$south$joint[spp,,]
        FUN <- function (eta)
            pmin(pmax(exp(eta), .Machine$double.eps), .Machine$double.xmax)
    } else {
        if (taxon == "mammals") {

            INFO <- TAB[spp,]
            II <- .mammal_lookup()
            II <- II[spp,]
            Link <- list(
                N=as.character(II$LinkSpclimNorth),
                S=as.character(II$LinkSpclimSouth))
            if (is.na(Link$N))
                Link$N <- "Log" # option 3 - no climate model, use 0
            if (is.na(Link$S))
                Link$S <- "Log" # option 3 - no climate model, use 0
            # NOTE!!!!
            # mammal habitat .ai1$COEFS are NOT transfoed to the log/logit scale
            ## (other taxa have cfn and cfs as log(x)/pogit(x))
            cfn_tot <- if (type == "S") NULL else .ai1$COEFS[[taxon]]$north$total[spp,]
            cfs_tot <- if (type == "N") NULL else .ai1$COEFS[[taxon]]$south$total[spp,]
            cfn_pa <-  if (type == "S") NULL else .ai1$COEFS[[taxon]]$north$pa[spp,]
            cfs_pa <-  if (type == "N") NULL else .ai1$COEFS[[taxon]]$south$pa[spp,]
            cfn_agp <- if (type == "S") NULL else .ai1$COEFS[[taxon]]$north$agp[spp,]
            cfs_agp <- if (type == "N") NULL else .ai1$COEFS[[taxon]]$south$agp[spp,]
            if (!is.null(cfn_agp))
                cfn_agp[is.na(cfn_agp)] <- 0
            if (!is.null(cfs_agp))
                cfs_agp[is.na(cfs_agp)] <- 0
            cfn_cl <-  if (type == "S") NULL else .ai1$COEFS[[taxon]]$north$clim[spp,]
            cfs_cl <-  if (type == "N") NULL else .ai1$COEFS[[taxon]]$south$clim[spp,]
            cfs_asp <- if (type == "N") NULL else .ai1$COEFS[[taxon]]$south$asp[spp,]
        } else {
            cfn <- if (type == "S")
                NULL else .ai1$COEFS[[taxon]]$north[spp,,]
            cfs <- if (type == "N")
                NULL else .ai1$COEFS[[taxon]]$south[spp,,]
            FUN <- stats::binomial()$linkinv
            if (taxon == "habitats")
                FUN <- stats::binomial(.ai1$COEFS[[taxon]]$species[spp, "LinkHabitat"])$linkinv
        }
    }


    ## process veg, soil, spclim
    Xclim <- .make_clim(spclim, taxon) # same for N and S
    pA <- spclim$pAspen

    out <- list(north=NULL, south=NULL)
    if (type == "C" && is.null(p_veghf) && is.null(p_soilhf))
        return(out)
    if (type == "C" && is.null(p_veghf))
        type <- "S"
    if (type == "C" && is.null(p_soilhf))
        type <- "N"
    if (type == "N" && is.null(p_veghf))
        return(out)
    if (type == "S" && is.null(p_soilhf))
        return(out)

    if (taxon == "mammals") {
        if (type != "N") {


            gc()
            ## south calculations for the i'th run
            ## space-climate .ai1$COEFS
            bscl <- cfs_cl[colnames(Xclim)]
            bscl[is.na(bscl)] <- 0 # this happens for habitat elements
            muscl <- drop(Xclim %*% bscl)

            muspaPA <-  pA * cfs_asp["pAspen.pa"]
            muspaAGP <- pA * cfs_asp["pAspen.agp"]

            ## habitat
            if (Link$S == "Log") {
                # pa
                tmps <- c(cfs_pa, SnowIce=0)
                tmps <- stats::qlogis(tmps)
                tmps[is.na(tmps)] <- -10^4
                bscrp <- tmps[colnames(p_soilhf)] # current land cover
                # agp
                tmps <- c(cfs_agp, SnowIce=0)
                tmps <- log(tmps)
                tmps[is.na(tmps)] <- -10^4
                bscra <- tmps[colnames(p_soilhf)] # current land cover

                mpacr <- matrix(bscrp, nrow=nrow(p_soilhf), ncol=ncol(p_soilhf), byrow=TRUE) + muspaPA
                magpcr <- matrix(bscra, nrow=nrow(p_soilhf), ncol=ncol(p_soilhf), byrow=TRUE) + muspaAGP
                muscr <- matrix(muscl, nrow=nrow(p_soilhf), ncol=ncol(p_soilhf))
                muscr <- exp(log(stats::plogis(mpacr) * exp(magpcr)) + muscr)
                NScr <- as.matrix(p_soilhf * muscr)
            } else {
                # pa
                tmpsp <- c(cfs_pa, SnowIce=0)
                tmpsp <- stats::qlogis(tmpsp)
                tmpsp[is.na(tmpsp)] <- -10^4
                bscrp <- tmpsp[colnames(p_soilhf)] # current land cover
                # agp
                tmpsa <- c(cfs_agp, SnowIce=0)
                tmpsa <- log(tmpsa)
                tmpsa[is.na(tmpsa)] <- -10^4
                bscra <- tmpsa[colnames(p_soilhf)] # current land cover

                mpacr <- matrix(bscrp, nrow=nrow(p_soilhf), ncol=ncol(p_soilhf), byrow=TRUE) + muspaPA
                magpcr <- matrix(bscra, nrow=nrow(p_soilhf), ncol=ncol(p_soilhf), byrow=TRUE) + muspaAGP
                muscr <- stats::plogis(mpacr + muscl) * exp(magpcr)
                NScr <- as.matrix(p_soilhf * muscr)
            }
        } else {
            NScr <- NULL
        }
        if (type != "S") {

            gc()
            ## north calculations for the i'th run
            ## space-climate .ai1$COEFS
            bncl <- cfn_cl[colnames(Xclim)]
            bncl[is.na(bncl)] <- 0 # this happens for habitat elements
            muncl <- drop(Xclim %*% bncl)
            ## habitat
            if (Link$N == "Log") {
                # this is total, log transformed
                tmpn <- c(cfn_tot, SnowIce=0)
                tmpn <- log(tmpn)
                tmpn[is.na(tmpn)] <- -10^4
                bncr <- tmpn[colnames(p_veghf)] # current land cover

                muncr <- matrix(muncl, nrow=nrow(p_veghf), ncol=ncol(p_veghf))
                muncr <- t(t(muncr) + bncr)
                NNcr <- as.matrix(p_veghf * exp(muncr))
            } else {
                # this is PA, stats::qlogis transformed
                tmpn <- c(cfn_pa, SnowIce=0)
                tmpn <- stats::qlogis(tmpn)
                tmpn[is.na(tmpn)] <- -10^4
                bncr <- tmpn[colnames(p_veghf)] # current land cover
                # this is the agp, untransformed
                tmpna <- c(cfn_agp, SnowIce=0)
                tmpna[is.na(tmpna)] <- 0
                tmpna[is.infinite(tmpna)] <- max(tmpna[!is.infinite(tmpna)])
                bncra <- tmpna[colnames(p_veghf)] # current land cover

                muncr <- matrix(muncl, nrow=nrow(p_veghf), ncol=ncol(p_veghf))
                muncr <- t(stats::plogis(t(muncr) + bncr) * bncra)
                NNcr <- as.matrix(p_veghf * muncr)
            }
        } else {
            NNcr <- NULL
        }
    ## non-mammal taxa
    } else {


        if (type != "N") {
            gc()
            ## south calculations for the i'th run
            #compare_sets(rownames(cfs), chSoil$cr2)
            bscr <- cfs[colnames(p_soilhf), i] # current land cover
            ## space-climate .ai1$COEFS
            bscl <- cfs[colnames(Xclim), i]
            bscl[is.na(bscl)] <- 0 # this happens for habitat elements
            bspa <- cfs["pAspen", i]
            ## additive components for south
            muscl <- drop(Xclim %*% bscl)
            muspa <- pA * bspa

            muscr <- matrix(muscl + muspa, nrow=nrow(p_soilhf), ncol=ncol(p_soilhf))
            muscr <- t(t(muscr) + bscr)
            NScr <- as.matrix(p_soilhf * FUN(muscr))
        } else {
            NScr <- NULL
        }
        if (type != "S") {
            gc()
            ## north calculations for the i'th run
            #compare_sets(rownames(cfn), chVeg$cr2)
            tmpn <- c(cfn[,i], Bare=-10^4, SnowIce= -10^4)
            bncr <- tmpn[colnames(p_veghf)] # current land cover
            ## space-climate .ai1$COEFS
            bncl <- cfn[colnames(Xclim), i]
            bncl[is.na(bncl)] <- 0 # this happens for habitat elements
            ## additive components for north
            muncl <- drop(Xclim %*% bncl)

            muncr <- matrix(muncl, nrow=nrow(p_veghf), ncol=ncol(p_veghf))
            muncr <- t(t(muncr) + bncr)
            NNcr <- as.matrix(p_veghf * FUN(muncr))
        } else {
            NNcr <- NULL
        }
    }

    list(north=NNcr, south=NScr)

}



