## internal functions -----------------

## hidden environment to store coefs in
.ai1 <- new.env(parent=emptyenv())
## store object for subset of the grid and species
#.ai1=allinone:::.ai1

## path to COEFS
.path <- function() {
    getOption("allinone")$url
}

## check if coefs are loaded
.loaded <- function() {
    length(names(.ai1)) > 0
}

## check if user wants messages
.verbose <- function() {
    x <- getOption("allinone")$verbose
    !is.null(x) && x > 0
}

## print messages if verbose
.msg <- function(x, level=1) {
    if (.verbose()) {
        if (level >= getOption("allinone")$verbose) {
            pf <- switch(level,
                "1"="[INFO] ",
                "2"="[NOTE] ",
                "3"="[WARN] ",
                "4"="")
            msg <- paste0(pf, x, "\n")
            if (level >= 4) {
                stop(msg, call.=FALSE)
            } else {
                cat(msg)
            }
            utils::flush.console()
        }
    }
    invisible()
}

## transform climate variables
.make_clim <- function(x, taxon) {
    if (taxon == "birds") {
        z <- with(x, cbind(
            pWater_KM=pWater,
            pWater2_KM=pWater^2,
            xPET=(PET - 0) / 800,
            xMAT=(MAT - 0) / 6,
            xAHM=(AHM - 0) / 50,
            xFFP=(FFP - 0) / 130,
            xMAP=(MAP - 0) / 2300,
            xMWMT=(MWMT - 0) / 20,
            xMCMT=(MCMT - 0) / 25,
            xY=(POINT_Y - 54.1) / 2.43,
            xX=(POINT_X - (-113.3)) / 2.12))
        z <- cbind(z,
            xY2=z[,"xY"]^2,
            xX2=z[,"xX"]^2,
            `xFFP:xMAP`=z[,"xFFP"]*z[,"xMAP"],
            `xMAP:xPET`=z[,"xMAP"]*z[,"xPET"],
            `xAHM:xMAT`=z[,"xAHM"]*z[,"xMAT"],
            `xX:xY`=z[,"xX"]*z[,"xY"])
    } else {
        LAT <- pmin(x$POINT_Y, 56.5)
        z <- with(x, cbind(
            Intercept=1,
            Lat=LAT,
            Long=POINT_X,
            AHM=AHM,
            PET=PET,
            FFP=FFP,
            MAP=MAP,
            MAT=MAT,
            MCMT=MCMT,
            MWMT=MWMT,
            Lat2=LAT^2,
            Long2=POINT_X^2,
            LatLong=POINT_X*LAT,
            MAPPET=MAP*x$PET,
            MATAHM=MAT*x$AHM,
            MAPFFP=MAP*x$FFP,
            MAT2=MAT^2,
            MWMT2=MWMT^2))
        if (taxon == "mammals") {
            z <- cbind(z,
                Lat3=z[,"Lat"]^3,
                Lat2Long2=z[,"Lat"]^2 * z[,"Long"]^2,
                LongMAT=z[,"Long"] * z[,"MAT"])
            z[,"MAT2"] <- z[,"MAT"]*(z[,"MAT"]+10)
            z <- cbind(z, x[,c("PeaceRiver", "NSR1CentralMixedwood",
                    "NSR1DryMixedwood", "NSR1Foothills",
                    "NSR1Mountain", "NSR1North", "NSR1Parkland", "NSR1Shield")])
        }
    }
    rownames(z) <- rownames(x)
    z
}

.get_taxon <- function(spp) {
    for (i in names(.ai1$COEFS)) {
        SPP <- rownames(.ai1$COEFS[[i]]$species)
        if (tolower(spp) %in% tolower(SPP))
            return(i)
    }
    .msg(sprintf("Coefs not available for species %s", spp), 4)
}


## lookup tables
.lt <- function() {
    list(
        south=structure(list(Label = c("ClaySub", "Other", "Other", "Other",
            "Other", "Other", "RapidDrain", "Loamy", "Other", "Other", "Other",
            "Other", "Other", "Other", "ClaySub", "SandyLoam", "RapidDrain",
            "RapidDrain", "RapidDrain", "RapidDrain", "RapidDrain", "ThinBreak",
            "Blowout", "Other", "Other", "Blowout", "UNK", "Water", "Urban",
            "Urban", "Rural", "Industrial", "Industrial", "Rural", "Mine",
            "Mine", "Wellsites", "EnSoftLin", "EnSoftLin", "EnSeismic", "EnSeismic",
            "HardLin", "HardLin", "TrSoftLin", "TrSoftLin", "TrSoftLin",
            "Crop", "RoughP", "RoughP", "TameP", "Industrial", "Water", "Water",
            "Water", "Water", "UNK"), Sector = c("Native", "Native", "Native",
            "Native", "Native", "Native", "Native", "Native", "Native", "Native",
            "Native", "Native", "Native", "Native", "Native", "Native", "Native",
            "Native", "Native", "Native", "Native", "Native", "Native", "Native",
            "Native", "Native", "Native", "Native", "RuralUrban", "RuralUrban",
            "RuralUrban", "RuralUrban", "Energy", "RuralUrban", "Energy",
            "Misc", "Energy", "Energy", "Energy", "Energy", "Energy", "Transportation",
            "Transportation", "Transportation", "Transportation", "Transportation",
            "Agriculture", "Agriculture", "Agriculture", "Agriculture", "Agriculture",
            "Misc", "Misc", "Misc", "Misc", "Forestry")), row.names = c("Cy",
            "Len", "LenS", "LenSP", "LenT", "LenW", "Li", "Lo", "Ltc", "LtcC",
            "LtcD", "LtcH", "LtcS", "LtcR", "Sb", "Sy", "BdL", "CS", "Gr",
            "Sa", "SwG", "TB", "BlO", "LenA", "Ov", "SL", "UNK", "Water",
            "UrbanIndustrial", "UrbanResidence", "RuralResidentialIndustrial",
            "IndustrialSiteRural", "WindGenerationFacility", "OtherDisturbedVegetation",
            "MineSite", "PeatMine", "WellSite", "Pipeline", "TransmissionLine",
            "SeismicLineNarrow", "SeismicLineWide", "RoadHardSurface", "RailHardSurface",
            "RoadTrailVegetated", "RoadVegetatedVerge", "RailVegetatedVerge",
            "CultivationCrop", "CultivationAbandoned", "CultivationRoughPasture",
            "CultivationTamePasture", "HighDensityLivestockOperation", "BorrowpitsDugoutsSumps",
            "MunicipalWaterSewage", "Reservoirs", "Canals", "CutBlocks"), class = "data.frame"),
        north=structure(list(Label = c("DeciduousR", "Deciduous1", "Deciduous2",
            "Deciduous3", "Deciduous4", "Deciduous5", "Deciduous6", "Deciduous7",
            "Deciduous8", "MixedwoodR", "Mixedwood1", "Mixedwood2", "Mixedwood3",
            "Mixedwood4", "Mixedwood5", "Mixedwood6", "Mixedwood7", "Mixedwood8",
            "PineR", "Pine1", "Pine2", "Pine3", "Pine4", "Pine5", "Pine6",
            "Pine7", "Pine8", "WhiteSpruceR", "WhiteSpruce1", "WhiteSpruce2",
            "WhiteSpruce3", "WhiteSpruce4", "WhiteSpruce5", "WhiteSpruce6",
            "WhiteSpruce7", "WhiteSpruce8", "TreedBogR", "TreedBog1", "TreedBog2",
            "TreedBog3", "TreedBog4", "TreedBog5", "TreedBog6", "TreedBog7",
            "TreedBog8", "TreedFenR", "TreedFen1", "TreedFen2", "TreedFen3",
            "TreedFen4", "TreedFen5", "TreedFen6", "TreedFen7", "TreedFen8",
            "TreedSwamp", "TreedSwamp", "TreedSwamp", "TreedSwamp", "TreedSwamp",
            "TreedSwamp", "TreedSwamp", "TreedSwamp", "TreedSwamp", "GrassHerb",
            "Shrub", "GraminoidFen", "Marsh", "ShrubbyBog", "ShrubbyFen",
            "ShrubbySwamp", "Water", "Urban", "Urban", "Rural", "Industrial",
            "Industrial", "Rural", "Mine", "Mine", "Wellsites", "EnSoftLin",
            "EnSoftLin", "EnSeismic", "EnSeismic", "HardLin", "HardLin",
            "TrSoftLin", "TrSoftLin", "TrSoftLin", "Crop", "RoughP", "RoughP",
            "TameP", "Industrial", "CCDeciduousR", "CCDeciduous1", "CCDeciduous2",
            "CCDeciduous3", "CCDeciduous4", "CCMixedwoodR", "CCMixedwood1",
            "CCMixedwood2", "CCMixedwood3", "CCMixedwood4", "CCPineR", "CCPine1",
            "CCPine2", "CCPine3", "CCPine4", "CCWhiteSpruceR", "CCWhiteSpruce1",
            "CCWhiteSpruce2", "CCWhiteSpruce3", "CCWhiteSpruce4", "Bare",
            "Water", "Water", "Water", "Water", "SnowIce"), Sector = c("Native",
            "Native", "Native", "Native", "Native", "Native", "Native", "Native",
            "Native", "Native", "Native", "Native", "Native", "Native", "Native",
            "Native", "Native", "Native", "Native", "Native", "Native", "Native",
            "Native", "Native", "Native", "Native", "Native", "Native", "Native",
            "Native", "Native", "Native", "Native", "Native", "Native", "Native",
            "Native", "Native", "Native", "Native", "Native", "Native", "Native",
            "Native", "Native", "Native", "Native", "Native", "Native", "Native",
            "Native", "Native", "Native", "Native", "Native", "Native", "Native",
            "Native", "Native", "Native", "Native", "Native", "Native", "Native",
            "Native", "Native", "Native", "Native", "Native", "Native", "Native",
            "RuralUrban", "RuralUrban", "RuralUrban", "RuralUrban", "Energy",
            "RuralUrban", "Energy", "Misc", "Energy", "Energy", "Energy",
            "Energy", "Energy", "Transportation", "Transportation", "Transportation",
            "Transportation", "Transportation", "Agriculture", "Agriculture",
            "Agriculture", "Agriculture", "Agriculture", "Forestry", "Forestry",
            "Forestry", "Forestry", "Forestry", "Forestry", "Forestry", "Forestry",
            "Forestry", "Forestry", "Forestry", "Forestry", "Forestry", "Forestry",
            "Forestry", "Forestry", "Forestry", "Forestry", "Forestry", "Forestry",
            "Native", "Misc", "Misc", "Misc", "Misc", "Native")), row.names = c("DecidR",
            "Decid1", "Decid2", "Decid3", "Decid4", "Decid5", "Decid6", "Decid7",
            "Decid8", "MixedwoodR", "Mixedwood1", "Mixedwood2", "Mixedwood3",
            "Mixedwood4", "Mixedwood5", "Mixedwood6", "Mixedwood7", "Mixedwood8",
            "PineR", "Pine1", "Pine2", "Pine3", "Pine4", "Pine5", "Pine6",
            "Pine7", "Pine8", "SpruceR", "Spruce1", "Spruce2", "Spruce3",
            "Spruce4", "Spruce5", "Spruce6", "Spruce7", "Spruce8", "TreedBogR",
            "TreedBog1", "TreedBog2", "TreedBog3", "TreedBog4", "TreedBog5",
            "TreedBog6", "TreedBog7", "TreedBog8", "TreedFenR", "TreedFen1",
            "TreedFen2", "TreedFen3", "TreedFen4", "TreedFen5", "TreedFen6",
            "TreedFen7", "TreedFen8", "TreedSwampR", "TreedSwamp1", "TreedSwamp2",
            "TreedSwamp3", "TreedSwamp4", "TreedSwamp5", "TreedSwamp6", "TreedSwamp7",
            "TreedSwamp8", "GrassHerb", "Shrub", "GraminoidFen", "Marsh",
            "ShrubbyBog", "ShrubbyFen", "ShrubbySwamp", "Water", "UrbanIndustrial",
            "UrbanResidence", "RuralResidentialIndustrial", "IndustrialSiteRural",
            "WindGenerationFacility", "OtherDisturbedVegetation", "MineSite",
            "PeatMine", "WellSite", "Pipeline", "TransmissionLine", "SeismicLineNarrow",
            "SeismicLineWide", "RoadHardSurface", "RailHardSurface", "RoadTrailVegetated",
            "RoadVegetatedVerge", "RailVegetatedVerge", "CultivationCrop",
            "CultivationAbandoned", "CultivationRoughPasture", "CultivationTamePasture",
            "HighDensityLivestockOperation", "CCDecidR", "CCDecid1", "CCDecid2",
            "CCDecid3", "CCDecid4", "CCMixedwoodR", "CCMixedwood1", "CCMixedwood2",
            "CCMixedwood3", "CCMixedwood4", "CCPineR", "CCPine1", "CCPine2",
            "CCPine3", "CCPine4", "CCSpruceR", "CCSpruce1", "CCSpruce2",
            "CCSpruce3", "CCSpruce4", "Bare", "BorrowpitsDugoutsSumps", "Canals",
            "MunicipalWaterSewage", "Reservoirs", "SnowIce"), class = "data.frame"))
}

.mammal_lookup <- function() {
    structure(list(SpeciesID = structure(c(1L, 2L, 3L, 4L, 5L, 7L,
        8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L,
        21L, 22L, 23L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 35L,
        36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 6L, 24L, 34L), .Label = c("Badger",
        "Beaver", "Bighornsheep", "Bison", "BlackBear", "Bobcat", "CanadaLynx",
        "ColumbianGroundSquirrel", "Cougar", "Coyote", "Deer", "Elk",
        "Fisher", "Foxes", "GoldenMantledGroundSquirrel", "GrayWolf",
        "Grizzlybear", "Groundhog", "HoaryMarmot", "LeastChipmunk", "Marten",
        "Mink", "Moose", "Mountaingoat", "Muledeer", "Muskrat", "NorthernFlyingSquirrel",
        "Porcupine", "Pronghorn", "Raccoon", "Redfox", "RedSquirrel",
        "RichardsonsGroundSquirrel", "RiverOtter", "SnowshoeHare", "StripedSkunk",
        "VolesMiceandAllies", "WeaselsandErmine", "WhitetailedDeer",
        "WhitetailedJackRabbit", "Wolverine", "WolvesCoyotesandAllies",
        "WoodlandCaribou"), class = "factor"), CommonName = structure(c(1L,
        2L, 3L, 4L, 5L, 20L, 8L, 9L, 10L, 34L, 11L, 12L, 14L, 15L, 39L,
        16L, 17L, 18L, 19L, 21L, 22L, 23L, 24L, 25L, 13L, 26L, 27L, 28L,
        29L, 30L, 31L, 32L, 33L, 35L, 36L, 37L, 38L, 40L, 7L, 41L, 6L,
        NA, NA), .Label = c("Badger", "Beaver", "Bighorn Sheep", "Bison",
        "Black Bear", "Bobcat", "Canids", "Columbian Ground Squirrel",
        "Cougar", "Coyote", "Elk", "Fisher", "Flying Squirrel", "Foxes",
        "Golden-Mantled Ground Squirrel", "Grizzly Bear", "Groundhog",
        "Hoary Marmot", "Least Chipmunk", "Lynx", "Marten", "Mink", "Moose",
        "Mule Deer", "Muskrat", "Porcupine", "Pronghorn", "Raccoon",
        "Red Fox", "Red Squirrel", "Richardson's Ground Squirrel", "Snowshoe Hare",
        "Striped Skunk", "Unknown Deer", "Voles and Mice", "Weasels",
        "White-tailed Deer", "White-tailed Jackrabbit", "Wolf", "Wolverine",
        "Woodland Caribou"), class = "factor"), ScientificName = c(NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), TSNID = c(NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA), Nonnative = c(FALSE, FALSE,
        FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
        FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
        FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
        FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
        FALSE, FALSE, FALSE, FALSE, FALSE), ModelNorth = c(FALSE, FALSE,
        FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE,
        FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
        TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
        TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
        FALSE, FALSE), LinkHabitatNorth = structure(c(NA, NA, NA, NA,
        1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, NA, 1L, NA, NA, NA, NA, 1L,
        NA, 1L, 1L, NA, NA, NA, NA, NA, 1L, NA, NA, 1L, 1L, NA, NA, 1L,
        1L, 1L, NA, NA, NA, NA, NA), .Label = "Logit/Log", class = "factor"),
            LinkSpclimNorth = structure(c(NA, NA, NA, NA, 1L, 2L, NA,
            2L, 2L, NA, 2L, 2L, NA, NA, 2L, NA, NA, NA, NA, 2L, NA, 1L,
            1L, NA, NA, NA, NA, NA, 2L, NA, NA, 2L, 2L, NA, NA, 2L, 2L,
            2L, NA, NA, NA, NA, NA), .Label = c("Log", "Logit"), class = "factor"),
            ModelNorthWinter = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
            FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE,
            FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE,
            FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE,
            FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
            FALSE), ModelNorthSummer = c(FALSE, FALSE, FALSE, FALSE,
            TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE,
            FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE,
            TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
            TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE), UseavailNorth = c(TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE,
            TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE,
            TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
            FALSE, FALSE), ModelSouth = c(TRUE, FALSE, FALSE, FALSE,
            TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
            TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
            TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE), LinkHabitatSouth = structure(c(1L,
            NA, NA, NA, 1L, NA, NA, NA, 1L, NA, 1L, NA, NA, NA, NA, NA,
            NA, NA, NA, NA, NA, 1L, 1L, NA, NA, NA, 1L, NA, 1L, NA, 1L,
            1L, 1L, NA, NA, 1L, 1L, NA, NA, NA, NA, NA, NA), .Label = "Logit/Log", class = "factor"),
            LinkSpclimSouth = structure(c(2L, NA, NA, NA, 2L, NA, NA,
            NA, 1L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2L,
            1L, NA, NA, NA, 2L, NA, 2L, NA, 2L, 2L, 2L, NA, NA, NA, 2L,
            NA, NA, NA, NA, NA, NA), .Label = c("Log", "Logit"), class = "factor"),
            ModelSouthWinter = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE,
            FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE,
            FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE), ModelSouthSummer = c(TRUE, FALSE, FALSE, FALSE, TRUE,
            FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
            FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE,
            TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE), UseavailSouth = c(TRUE, FALSE, FALSE, FALSE,
            TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE,
            FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE,
            TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,
            TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE,
            FALSE, FALSE, FALSE), AUCNorth = c(NA, NA, NA, NA, 0.73,
            0.744, NA, 0.857, 0.84, 0.805, 0.921, 0.633, NA, NA, 0.69,
            0.885, NA, NA, NA, 0.64, NA, 0.628, 0.926, NA, NA, 0.875,
            NA, NA, 0.737, 0.677, NA, 0.699, 0.873, NA, 0.605, 0.841,
            0.964, 0.92, NA, 0.848, NA, NA, NA), XValNorth = c(NA, NA,
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), AUCSouth = c(0.666,
            NA, NA, NA, 0.917, NA, NA, NA, 0.646, 0.626, 0.707, 0.739,
            NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.817, 0.686, NA, NA,
            0.579, 0.928, NA, 0.701, 0.9, 0.856, 0.83, 0.687, NA, NA,
            0.714, 0.791, NA, NA, NA, NA, NA, NA), XValSouth = c(NA,
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), Notes = structure(c(1L,
            7L, 1L, 1L, 1L, 1L, 7L, 1L, 1L, 5L, 1L, 9L, 6L, 7L, 1L, 10L,
            7L, 7L, 7L, 1L, 7L, 1L, 1L, 7L, 4L, 4L, 1L, 1L, 1L, 4L, 7L,
            1L, 7L, 3L, 7L, 1L, 1L, 1L, 1L, 8L, 2L, 1L, 7L), .Label = c("",
            "3 records but one is dubious ID", "Omit because poorly sampled by cameras",
            "Omit because poorly sampled by cameras - only when on ground",
            "Omit unknown deer", "Omit unknown foxes", "Poorly sampled by cameras",
            "Results withheld due to incomplete sampling of all ranges and better information sources elsewhere",
            "South model rejected", "Withheld until bigger sample size in range; better sources of information available"
            ), class = "factor")), class = "data.frame", row.names = c("Badger",
        "Beaver", "Bighornsheep", "Bison", "BlackBear", "CanadaLynx",
        "ColumbianGroundSquirrel", "Cougar", "Coyote", "Deer", "Elk",
        "Fisher", "Foxes", "GoldenMantledGroundSquirrel", "GrayWolf",
        "Grizzlybear", "Groundhog", "HoaryMarmot", "LeastChipmunk", "Marten",
        "Mink", "Moose", "Muledeer", "Muskrat", "NorthernFlyingSquirrel",
        "Porcupine", "Pronghorn", "Raccoon", "Redfox", "RedSquirrel",
        "RichardsonsGroundSquirrel", "SnowshoeHare", "StripedSkunk",
        "VolesMiceandAllies", "WeaselsandErmine", "WhitetailedDeer",
        "WhitetailedJackRabbit", "Wolverine", "WolvesCoyotesandAllies",
        "WoodlandCaribou", "Bobcat", "Mountaingoat", "RiverOtter"))
}
