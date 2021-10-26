# allinone

> All-in-on model based custom predictions for species in Alberta

## Install

``` r
if (!require("remotes"))
    install.packages("remotes")
remotes::install_github("ABbiodiversity/allinone")
```

You will need data and coefficients from the
[ABbiodiversity/allinone-coefs](https://github.com/ABbiodiversity/allinone-coefs)
repository. Clone or download the contents in zip format and extract
into a folder (`dir` in the example).

``` r
dir <- "~/repos/allinone-coefs"
```

If you don’t need the spatial raster files from the
ABbiodiversity/allinone-coefs, the `ai_download_coefs()` function will
grab the coefficients for you and you’ll be ready to roll:

``` r
library(allinone)

#ai_dowload_coefs()

ai_load_coefs()
## [INFO] Loading coefs
```

## Command line usage

See all the 1050 species that we have coefficients for:

``` r
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
```

Here is the number of species by groups:

``` r
data.frame(table(tab$Group))
##       Var1 Freq
## 1    birds  127
## 2 habitats   45
## 3  lichens  164
## 4  mammals   19
## 5    mites  118
## 6   mosses  120
## 7 nnplants    1
## 8  vplants  456
```

### Predictor data

We use an example data set that shows you how to organize the data:

``` r
## example data to see what is needed and how it is formatted
load(system.file("extdata/example.RData", package="allinone"))

## space climate data frame + veg/soil classes
str(spclim)
## 'data.frame':    30 obs. of  26 variables:
##  $ POINT_X             : num  -113 -111 -113 -113 -114 ...
##  $ POINT_Y             : num  49.6 51.4 49.3 51.3 49.6 ...
##  $ NSRNAME             : Factor w/ 21 levels "Alpine","Athabasca Plain",..: 13 6 13 8 13 13 6 6 6 8 ...
##  $ NRNAME              : Factor w/ 6 levels "Boreal","Canadian Shield",..: 4 4 4 4 4 4 4 4 4 4 ...
##  $ LUF_NAME            : Factor w/ 7 levels "Lower Athabasca",..: 5 4 5 5 5 4 5 4 5 5 ...
##  $ AHM                 : num  37.6 40.6 34.1 33 27.1 36.1 46.4 42.2 38.7 28.6 ...
##  $ PET                 : int  668 630 660 623 634 632 699 672 672 662 ...
##  $ FFP                 : int  119 114 112 111 107 113 126 119 118 112 ...
##  $ MAP                 : int  399 320 434 409 532 371 317 322 385 521 ...
##  $ MAT                 : num  5 3 4.8 3.5 4.4 3.4 4.7 3.6 4.9 4.9 ...
##  $ MCMT                : num  -9.4 -13.9 -8.7 -12 -8.4 -12.6 -12.3 -14.1 -9.7 -8.3 ...
##  $ MWMT                : num  18 17.7 17.3 16.9 16.4 17.4 19.6 18.8 18.1 17.3 ...
##  $ pAspen              : num  0.000202 0.21 0.001266 0.01 0.006599 ...
##  $ pWater              : num  0.001382 0.014561 0.003223 0.000396 0.002973 ...
##  $ pSoil               : num  0.999 0.985 0.997 1 0.997 ...
##  $ wN                  : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ PeaceRiver          : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ NSR1CentralMixedwood: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ NSR1DryMixedwood    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ NSR1Foothills       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ NSR1Mountain        : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ NSR1North           : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ NSR1Parkland        : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ NSR1Shield          : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ veghf               : chr  "Crop" "RoughP" "Crop" "Crop" ...
##  $ soilhf              : chr  "Crop" "RoughP" "Crop" "Crop" ...

## veg+HF composition data matrix
colnames(p_veghf)
##  [1] "Bare"           "RoughP"         "Crop"           "TameP"         
##  [5] "Industrial"     "Mine"           "Rural"          "EnSoftLin"     
##  [9] "HardLin"        "TrSoftLin"      "EnSeismic"      "Urban"         
## [13] "Wellsites"      "Deciduous1"     "CCDeciduous1"   "CCDeciduous2"  
## [17] "CCDeciduous3"   "CCDeciduous4"   "CCDeciduousR"   "Deciduous2"    
## [21] "Deciduous3"     "Deciduous4"     "Deciduous5"     "Deciduous6"    
## [25] "Deciduous7"     "Deciduous8"     "DeciduousR"     "GraminoidFen"  
## [29] "GrassHerb"      "Marsh"          "Mixedwood1"     "CCMixedwood1"  
## [33] "CCMixedwood2"   "CCMixedwood3"   "CCMixedwoodR"   "Mixedwood2"    
## [37] "CCMixedwood4"   "Mixedwood3"     "Mixedwood4"     "Mixedwood5"    
## [41] "Mixedwood6"     "Mixedwood7"     "Mixedwood8"     "MixedwoodR"    
## [45] "Pine1"          "CCPine1"        "CCPine2"        "CCPine3"       
## [49] "CCPine4"        "CCPineR"        "Pine2"          "Pine3"         
## [53] "Pine4"          "Pine5"          "Pine6"          "Pine7"         
## [57] "Pine8"          "PineR"          "Shrub"          "ShrubbyBog"    
## [61] "ShrubbyFen"     "ShrubbySwamp"   "SnowIce"        "WhiteSpruce1"  
## [65] "CCWhiteSpruce1" "CCWhiteSpruce2" "CCWhiteSpruce3" "CCWhiteSpruce4"
## [69] "CCWhiteSpruceR" "WhiteSpruce2"   "WhiteSpruce3"   "WhiteSpruce4"  
## [73] "WhiteSpruce5"   "WhiteSpruce6"   "WhiteSpruce7"   "WhiteSpruce8"  
## [77] "WhiteSpruceR"   "TreedBog1"      "TreedBog2"      "TreedBog3"     
## [81] "TreedBog4"      "TreedBog5"      "TreedBog6"      "TreedBog7"     
## [85] "TreedBog8"      "TreedBogR"      "TreedFen1"      "TreedFen2"     
## [89] "TreedFen3"      "TreedFen4"      "TreedFen5"      "TreedFen6"     
## [93] "TreedFen7"      "TreedFen8"      "TreedFenR"      "TreedSwamp"

## soil+HF composition data matrix
colnames(p_soilhf)
##  [1] "RapidDrain" "Crop"       "RoughP"     "TameP"      "Industrial"
##  [6] "Mine"       "Rural"      "EnSoftLin"  "HardLin"    "TrSoftLin" 
## [11] "EnSeismic"  "Wellsites"  "Blowout"    "Urban"      "ClaySub"   
## [16] "Other"      "Loamy"      "SandyLoam"  "ThinBreak"
```

### Predict for a species

You need to define the species ID (use the `tab` object to find out) and
the bootstrap ID (`i`). The bootstrap ID can be between 1 and 100 (only
1 for mammals and habitat elements).

``` r
## define species and bootstrap id
spp <- "AlderFlycatcher"
i <- 1
```

#### Composition data

You can use composition data, i.e. giving the areas or proportions of
different landcover types (columns) in a spatial unit (rows). The
corresponding relative abundance values will be returned in a matrix
format:

``` r
## use composition
z1 <- ai_predict(spp, 
  spclim=spclim, 
  veghf=p_veghf, 
  soilhf=p_soilhf,
  i=i)
## [INFO] Making predictions for species AlderFlycatcher (birds) i=1
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
```

#### Sector effects

Having such a matrix format is ideal when further aggregation is to e
performad on the output, e.g. when calculating sector effects. In the
example we use only the current landscape here, and show how to use
model weights (`wN`) to average the north and south results in the
overlap zone:

``` r
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
```

#### Classified landcover data

We have classified landcover data when we are making predictions for
single polygons (which are aggregated in the composition data case). We
can provide `veghf` and `soilhf` as a vector of these classes.

Make sure that the class names are consistent with column names in the
example data matrices for the north and south, respectively.

The function now returns a list of vectors:

``` r
## use land cover classes
z2 <- ai_predict(spp, 
  spclim=spclim, 
  veghf=spclim$veghf, 
  soilhf=spclim$soilhf,
  i=i)
## [INFO] Making predictions for species AlderFlycatcher (birds) i=1
str(z2)
## List of 2
##  $ north: Named num [1:30] 0.00151 0.00835 0.00186 0.00443 0.0058 ...
##   ..- attr(*, "names")= chr [1:30] "1" "2" "3" "4" ...
##  $ south: Named num [1:30] 2.06e-05 6.72e-05 4.12e-05 1.07e-04 1.73e-04 ...
##   ..- attr(*, "names")= chr [1:30] "1" "2" "3" "4" ...

## averaging predictions
avg2 <- spclim$wN * z2$north + (1-spclim$wN) * z2$south
str(avg2)
##  Named num [1:30] 2.06e-05 6.72e-05 4.12e-05 1.07e-04 1.73e-04 ...
##  - attr(*, "names")= chr [1:30] "1" "2" "3" "4" ...
```

### Prediction uncertainty

We can use the bootstrap distribution to calculate uncertainty
(i.e. confidence intervals, CI):

``` r
v <- NULL

for (i in 1:20) {
    zz <- ai_predict(spp, 
        spclim=spclim, 
        veghf=spclim$veghf, 
        soilhf=spclim$soilhf,
        i=i)
    v <- cbind(v, spclim$wN * zz$north + (1-spclim$wN) * zz$south)
}

t(apply(v[25:30,], 1, quantile, c(0.5, 0.05, 0.95)))
          50%         5%        95%
## 25 0.31590764 0.26543390 0.35751442
## 26 0.05219450 0.03893699 0.07041478
## 27 0.09576693 0.07895616 0.11126043
## 28 0.04139684 0.03238818 0.05872012
## 29 0.12563301 0.09836205 0.15970154
## 30 0.06754963 0.05384576 0.08798115
```

This gives the median and the 90% CI. This is currently not available
for mammals and habitat elements.

### Predict for multiple species

Once the predictors are organized, loop over the species IDs from `tab`
and store the results in an organized fashion.

## Spatial data manipulation

The variables in the `spclim` object can be extracted from the raster
layers stored in the
[ABbiodiversity/allinone-coefs](https://github.com/ABbiodiversity/allinone-coefs)
repository. Clone or download the contents in zip format and extract
into a folder (`dir` variable used here).

``` r
library(sf)
library(raster)

## you got some coordinates (degree long/lat)
XY <- spclim[,c("POINT_X", "POINT_Y")]
head(XY)

## make a sf data frame
xy <- sf::st_as_sf(XY, coords = c("POINT_X", "POINT_Y"))
## set CRS
xy <- st_set_crs(xy, "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

## variable names
vn <- c("AHM", "FFP", "MAP", "MAT", "MCMT", "MWMT", 
  "NSR1CentralMixedwood", "NSR1DryMixedwood", "NSR1Foothills", 
  "NSR1Mountain", "NSR1North", "NSR1Parkland", "NSR1Shield", 
  "pAspen", "PeaceRiver", "PET", "pWater", "wN")
## link to the rasters & create a stack
rl <- list()
for (j in vn)
  rl[[j]] <- suppressWarnings(raster(
    file.path(dir, "spatial", paste0(j, ".tif"))))
rl <- stack(rl)

## transform xy to Alberta TM
xy <- st_transform(xy, proj4string(rl))

plot(rl[["pAspen"]], axes=FALSE, box=FALSE)
plot(xy$geometry, add=TRUE, pch=3)

## extract info
e <- extract(rl, xy)

## absolute difference should be tiny
max(colSums(abs(spclim[,vn] - e[,vn])))

## we need long/lat as in XY too
spclim2 <- data.frame(XY, e)

## let's see what we get
str(ai_predict(spp, 
  spclim=spclim2, 
  veghf=spclim$veghf, 
  soilhf=spclim$soilhf,
  i=i))
```

## Batch processing

For batch processing, look into the `inst/docker` folder. The API can be
deployed using Docker or Docker Compose.

### Request

Pass is a JSON object (Content type: application/json) as the request
body with longitude, latitude, veg/soil/HF classes, the species ID and
the bootstrap ID (all these arrays in side an object).

``` json
{
  "long":[-112.6178,-110.9309,-112.8359,-113.3896,-113.8632,-112.299,
    -110.5805,-111.7806,-112.3541,-113.4144,-110.2329,-114.6634,-111.5857,
    -114.2688,-114.4383,-110.4444,-113.6355,-113.5874,-113.7964,-113.9445,
    -119.9851,-112.7504,-113.5144,-112.7433,-118.0259,-116.2997,-114.9535,
    -111.387,-111.8138,-111.1588],
  "lat":[49.5851,51.3653,49.3464,51.344,
    49.5951,51.3239,50.2184,50.9328,49.5974,49.3468,53.4814,53.8818,53.1679,
    51.1549,51.3357,52.3465,51.7699,53.748,52.7971,52.1959,59.2693,56.3071,
    58.269,56.1541,56.3535,59.8075,53.046,58.2175,58.7773,55.0714],
  "veghf":["Crop","RoughP","Crop","Crop","TameP","GrassHerb","GrassHerb",
    "GrassHerb","Crop","Crop","Crop","Crop","Crop","Rural","Crop","Crop","TameP",
    "Crop","Crop","Crop","TreedFenR","TreedBog4","TreedSwamp","TreedBog5",
    "CCDeciduousR","TreedFen8","CCDeciduous2","PineR","ShrubbyFen","Mixedwood2"],
  "soilhf":["Crop","RoughP","Crop","Crop","TameP","Blowout","ThinBreak",
    "RapidDrain","Crop","Crop","Crop","Crop","Crop","Rural","Crop","Crop",
    "TameP","Crop","Crop","Crop","RapidDrain","RapidDrain","RapidDrain",
    "RapidDrain","RapidDrain","RapidDrain","RoughP","RapidDrain","RapidDrain",
    "RapidDrain"],
  "spp":["AlderFlycatcher"],
  "i":[1]
}
```

### Response

The response is a JSON (application/json) array with the model averaged
relative abundance values:

``` json
[0,0.0001,0,0.0001,0.0002,0,0,0,0,0.0001,0.0044,0.0105,0.0063,
0.0045,0.005,0.0055,0.0059,0.0128,0.0116,0.0082,0.1826,0.0326,
0.072,0.0269,0.3291,0.0538,0.1034,0.0447,0.1266,0.0681]
```

### Docker workflow

A Docker workflow is ideal because the size of the application with all
the required spatial layers and coefficients is relatively small.

#### Building the image

Change directory to where the Dockerfile is, build and push the image:

``` bash
cd inst/docker

export REGISTRY="psolymos"
export TAG="allinone:latest"

## build the image
docker build -t $REGISTRY/$TAG .

docker push $REGISTRY/$TAG
```

#### Deploying

The Docker image can be deployed enywhere. The machine needs to have the
Docker Engine installed. Pull the Docker image to the machine you want
to run the API on:

``` bash
docker pull $REGISTRY/$TAG
```

In production, run the image with port mapping (-p host:container) in
the background (-d):

``` bash
docker run -d --name aiapi -p 8080:8080 $REGISTRY/$TAG
```

Now use curl to make a request and test the API, it can take a single
value, or a vector of values for long, lat, veghf, and soilhf:

``` bash
## curl
curl http://localhost:8080/ -d \
  '{"long":[-111.1588],"lat":[55.0714],"veghf":["Mixedwood2"],"soilhf":["RapidDrain"],"spp":["AlderFlycatcher"],"i":[2]}'
# [0.0605]

curl http://localhost:8080/ -d \
  '{"long":[-112.6178,-111.1588],"lat":[49.5851,55.0714],"veghf":["Crop","Mixedwood2"],"soilhf":["Crop","RapidDrain"],"spp":["AlderFlycatcher"],"i":[1]}'
# [0,0.0681]
```

See the logs:

``` bash
docker logs aiapi
# [INFO] This is the All-in-one API
# [INFO] Packages loaded
# [INFO] Raster stack loaded
# [INFO] Loading coefs
# [INFO] 2021-07-24 06:36:14 Starting server: http://0.0.0.0:8080
# [INFO] Making predictions for species AlderFlycatcher (birds) i=1
```

Kill and remove the container:

``` bash
docker kill aiapi && docker rm aiapi
```

It is advised to add the Docker background process to systemd or use
Docker Compose in production. Docker Compose can better handle container
restarts and can run multiple replicas (simple round robin load
balancing, which requires a proxy server, like Nginx or Caddy).

See the `inst/docker/docker-compose.yml` file for details. Deploy with
`docker-compose up -d` which will publish the app on port 8080.

When the containers are already running, and the configuration or images
have changed after the container’s creation, `docker-compose up` picks
up the changes. Tear down with `docker-compose down`.
