## r2EO
### DOwnload spatial data from LPDAAC, Open Street Maps, and more

Christian Devenish

### Installation


Make sure you have the `devtools` package installed
Open an R script:

```
# Install devtools, if necessary
# install.packages("devtools")

# install
devtools::install_github("https://github.com/Cdevenish/r2EO")

library(r2EO)

```


### Functions organised intro groups

aoi_*
Functions for creating study areas, template rasters
Basic functions for creating standardised study areas, with shapefile/extent/raster
templates with common extents, resolution and projection for whole project. 

eo_*  
Functions for downloading spatial covariates from LPDAAC

osm_*
Functions for downloading Open Street Maps data and creating distance rasters 
from these.


### Usage overview
See detailed help files for each function

#### Spatial covariates
DEMs: ASTER or SRTM

MODIS

LANDSAT  



