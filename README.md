allinone
========

> All-in-on model based custom predictions

Install

``` r
remotes::install_github("ABbiodiversity/allinone")
```

Load the package, then download and load the coefs, etc.

``` r
library(allinone)

#ai_dowload_coefs()

names(allinone:::.ai1)
## character(0)
ai_load_coefs()
## [INFO] Loading coefs
names(allinone:::.ai1)
## [1] "COEFS"
tab <- ai_species()
str(tab)
## 'data.frame':    1050 obs. of  21 variables:
##  $ SpeciesID     : chr  "Bryoria.fuscescens.glabra.lanestris.vrangiana" "Candelaria.pacifica" "Cetraria.aculeata.muricata" "Cetraria.arenaria" ...
##  $ ScientificName: chr  "Bryoria fuscescens/glabra/lanestris/vrangiana" "Candelaria pacifica" "Cetraria aculeata/muricata" "Cetraria arenaria" ...
##  $ TSNID         : chr  "99002220" "99002875" "98283083" "99002154" ...
##  $ CommonName    : chr  NA NA NA NA ...
##  $ ModelNorth    : logi  TRUE FALSE FALSE FALSE FALSE TRUE ...
##  $ ModelSouth    : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
##  $ UseavailNorth : logi  FALSE TRUE TRUE FALSE TRUE FALSE ...
##  $ UseavailSouth : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
##  $ Occurrences   : int  2719 213 151 58 75 1921 1968 2231 2055 402 ...
##  $ nSites        : int  1028 148 73 28 58 846 1006 1008 887 197 ...
##  $ SizeNorth     : logi  NA NA NA NA NA NA ...
##  $ SizeSouth     : logi  NA NA NA NA NA NA ...
##  $ Nonnative     : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
##  $ LinkHabitat   : chr  "logit" "logit" "logit" "logit" ...
##  $ LinkSpclim    : chr  "logit" "logit" "logit" "logit" ...
##  $ AUCNorth      : num  0.926 0.808 NA NA 0.785 0.864 0.797 0.844 0.869 NA ...
##  $ AUCSouth      : num  0.912 0.881 0.911 0.936 0.879 0.918 0.834 0.926 0.888 0.871 ...
##  $ R2North       : num  0.494 0.15 NA NA 0.09 0.336 0.214 0.3 0.352 NA ...
##  $ R2South       : num  0.356 0.236 0.327 0.359 0.188 0.306 0.213 0.4 0.221 0.297 ...
##  $ Comments      : chr  NA NA NA NA ...
##  $ Group         : chr  "lichens" "lichens" "lichens" "lichens" ...

## example data to see what is needed and how it is formatted
load(system.file("extdata/example.RData", package="allinone"))

## define species and bootstrap id
spp <- "AlderFlycatcher"
i <- 1
# 1:2880, 1:231

## use composition
z1 <- ai_predict(spp, 
  spclim=spclim, 
  veghf=p_veghf, 
  soilhf=p_soilhf,
  i=i)
## [INFO] Making predictions for species AlderFlycatcher (birds)
str(z1)
## List of 2
##  $ north: num [1:30, 1:96] 0 0 0 0 0 0 0 0 0 0 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:30] "1167_502" "964_613" "1194_487" "973_442" ...
##   .. ..$ : chr [1:96] "Bare" "RoughP" "Crop" "TameP" ...
##  $ south: num [1:30, 1:19] 0.00 0.00 6.64e-07 0.00 1.41e-05 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:30] "1167_502" "964_613" "1194_487" "973_442" ...
##   .. ..$ : chr [1:19] "RapidDrain" "Crop" "RoughP" "TameP" ...

## sector effects
library(mefa4)
lt <- ai_classes()
ltn <- nonDuplicated(lt$north, Label, TRUE)
lts <- nonDuplicated(lt$south, Label, TRUE)

Nn <- groupSums(z1$north, 2, ltn[colnames(z1$north), "Sector"])
Ns <- groupSums(z1$south, 2, lts[colnames(z1$south), "Sector"])
Ns <- cbind(Ns, Forestry=0)
N <- spclim$wN * Nn + (1-spclim$wN) * Ns
colSums(Nn)
##         Native    Agriculture     RuralUrban         Energy Transportation 
##     0.64406824     0.14011059     0.01153628     0.03155056     0.02626116 
##       Forestry 
##     0.17863231
colSums(Ns)
##         Native    Agriculture     RuralUrban         Energy Transportation 
##   2.973927e-03   1.180921e-02   2.369699e-04   2.922007e-10   3.790724e-10 
##       Forestry 
##   0.000000e+00
colSums(N)
##         Native    Agriculture     RuralUrban         Energy Transportation 
##    0.610374698    0.076370845    0.005830417    0.022772096    0.015636447 
##       Forestry 
##    0.178632312


## use land cover classes
z2 <- ai_predict(spp, 
  spclim=spclim, 
  veghf=spclim$veghf, 
  soilhf=spclim$soilhf,
  i=i)
## [INFO] Making predictions for species AlderFlycatcher (birds)
str(z2)
## List of 2
##  $ north: num [1:30, 1:15] 0 0 0 0 0 0 0 0 0 0 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:30] "1" "2" "3" "4" ...
##   .. ..$ : chr [1:15] "CCDeciduous2" "CCDeciduousR" "Crop" "GrassHerb" ...
##  $ south: num [1:30, 1:7] 0 0 0 0 0 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:30] "1" "2" "3" "4" ...
##   .. ..$ : chr [1:7] "Blowout" "Crop" "RapidDrain" "RoughP" ...

## averaging predictions
avg2 <- spclim$wN * rowSums(z2$north) + (1-spclim$wN) * rowSums(z2$south)
str(avg2)
##  Named num [1:30] 2.06e-05 6.72e-05 4.12e-05 1.07e-04 1.73e-04 ...
##  - attr(*, "names")= chr [1:30] "1" "2" "3" "4" ...
```
