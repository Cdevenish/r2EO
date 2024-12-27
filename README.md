## downEO
### Create study areas, extract river networks, obtain spatial covariates

Christian Devenish

### Installation

Go to [https://github.com/settings/tokens](https://github.com/settings/tokens)  
Create an access token (enable read access to packages on options)

Make sure you have the `devtools` package installed
Open an R script:

```
# install.packages("devtools")

token <- "" # copy your token here

repo <- "NatureMetrics/downEO"

# install
devtools::install_github(repo = repo,
                         ref="feature/aoi",
                         auth_token = token)

library(downEO)

```


### Functions organised intro groups

aoi_*
Functions for creating study areas, template rasters
Basic functions for creating standardised study areas, with shapefile/extent/raster
templates with common extents, resolution and projection for whole project. 

eo_*  
Functions for downloading spatial covariates:

osm_*
Functions for downloading Open Street Maps data and creating distance rasters 
from these.

sd_*
Functions for creating study area designs, using suda muda, and placing points
along linear networks. Requires a conda environment for sudamuda. See `sd_python install`

hydro_*  
Functions for extracting river networks
NOTE: in development still, and needs access to stored copies of hydroSHEDs data.


### Usage overview
See also detailed help files for each function

#### Spatial covariates
DEMs: ASTER or SRTM

MODIS

LANDSAT  



#### River networks
Available at different river sizes, for given spatial extent or downstream from provided points/polygons  


Dependencies listed in `Description`
Example datasets in data folder
