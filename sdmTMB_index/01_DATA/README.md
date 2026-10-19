# SPOCC
SPOndyliosoma Cantharus in the Channel

# 01_DATA

This directory contains the spatial and survey datasets used in the `sdmTMB_index` folder

Data files are stored locally and excluded from version control via `.gitignore`

## Data description

### 1. Spatial data (`shapefiles/`)

- east7d: Shapefile representing ICES Division 7.d (Eastern English Channel)
- West7e: Shapefile representing ICES Division 7.e (Western English Channel)

### 2. Groundfish survey data (`surveys/`)

- ECGFS: Eastern Channel Groundfish Survey
- WCGFS: Western Channel Groundfish Survey

These datasets contain scientific bottom-trawl survey observations used for species distribution modelling and abundance index estimation.

### 3. Land boundaries (`ne_10m_land.rds`)

Land polygon data from `rnaturalearth` R package, at 1:10 million scale, stored locally as an RDS file.

## Data availability

The datasets are not included in this Git repository. They must be obtained separately and placed in the appropriate directories to reproduce the analyses.