
# ============================================================================== #
# PREPARATION OF CGFS SURVEY DATA FOR SPECIES DISTRIBUTION MODELLING
#
# Study area: Eastern (ICES 7d) and Western (ICES 7e) English Channel
# Before 2023
#
# Objectives:
# - Import and harmonise Eastern and Western CGFS survey datasets.
# - Extract species-specific catch records and associated haul information.
# - Calculate biomass density and presence-absence for each haul.
# - Convert geographic coordinates into projected UTM coordinates.
# - Select sampling stations located within ICES divisions 7d and 7e.
# ============================================================================== #

  
# ------------------------------------------------------------------------------ #
#### 1. LOAD THE DATA #### 
# ------------------------------------------------------------------------------ #
  
# Load spatial polygons defining the study areas:
# ICES division 7e (Western English Channel) and 7d (Eastern English Channel).
west7e <- terra::vect(here("sdmTMB_index/01_DATA/shapefiles/west7e/west7e.shp"))
east7d <- terra::vect(here("sdmTMB_index/01_DATA/shapefiles/east7d/east7d.shp"))

# Load locally saved Natural Earth land polygons
land_ne <- readRDS(here("sdmTMB_index/01_DATA/ne_10m_land.rds"))



# ------------------ #
# Eastern CGFS
# ------------------ #
# Import catch data from the Eastern Channel Ground Fish Survey (ECGFS)
catch_ECGFS <- readr::read_delim(here("sdmTMB_index/01_DATA/survey/ECGFS/CGFS_1988_2024_ELFIC.V1.4_catch_2025-03-28.csv"),
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

# Remove fully duplicated catch records
catch_ECGFS <- catch_ECGFS %>% dplyr::distinct()


# Import haul-level operation data, including sampling locations, swept area 
operation_ECGFS <- readr::read_delim(here("sdmTMB_index/01_DATA/survey/ECGFS/CGFS_1988_2024_ELFIC.V1.4_operation_2025-03-28.csv"),
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

# Remove duplicated operation records
operation_ECGFS <- operation_ECGFS %>% dplyr::distinct()

# -------------------------------------------------- #
# INVESTIGATION OF POTENTIAL GEOREFERENCING ERRORS
# CGFS 2009 – Hauls 41 and 42
# -------------------------------------------------- #

# # Extract haul metadata
# hauls_check <- operation_ECGFS %>%
#   dplyr::filter(haulID %in% c("2009_1_41_41", "2009_1_42_42"))
# 
# # Check whether coordinates are identical
# hauls_check %>%
#   dplyr::summarise(same_start_lat = n_distinct(startLatDD) == 1,
#                    same_start_lon = n_distinct(startLongDD) == 1,
#                    same_end_lat   = n_distinct(endLatDD) == 1,
#                    same_end_lon   = n_distinct(endLongDD) == 1)
# 
# 
# # Calculate geodesic distances from start/end coordinates and compare them 
# # with reported trawl distances
# hauls_check %>%
#   dplyr::mutate(
#     distance_calculated_m = geosphere::distGeo(
#       p1 = cbind(startLongDD, startLatDD),
#       p2 = cbind(endLongDD, endLatDD)),
#     distance_difference_m = distanceMeters - distance_calculated_m) %>%
#   dplyr::select(haulID, locality, distanceMeters, 
#                 distance_calculated_m, distance_difference_m) %>%
#   print(n = Inf, width = Inf)
# 
# 
# # Compare black seabream catches between the two hauls
# catch_ECGFS %>%
#   dplyr::filter(haulID %in% c("2009_1_41_41", "2009_1_42_42"),
#                 scientificName == "Spondyliosoma cantharus") %>%
#   dplyr::select(haulID, scientificName, totalNumber, totalWeightKg)

# Exclude haul 2009_1_41_41 due to a suspected georeferencing error.
# Hauls 41 and 42 have identical start and end coordinates but different
# reported localities (4N2 vs 3M1) and trawl distances
operation_ECGFS <- operation_ECGFS %>%  
  dplyr::filter(haulID != "2009_1_41_41")


# ------------------ #
# Western CGFS
# ------------------ #
# Import catch data from the Western Channel Ground Fish Survey (WCGFS)
catch_WCGFS <-  readr::read_delim(here("sdmTMB_index/01_DATA/survey/WCGFS/WCGFS_2018_2024_ELFIC.V1.5_catch_2025-03-31.csv"), 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

# Remove fully duplicated catch records
catch_WCGFS <- catch_WCGFS %>%  dplyr::distinct()

# Import haul-level operation data, including sampling locations, swept area 
operation_WCGFS <- readr::read_delim(here("sdmTMB_index/01_DATA/survey/WCGFS/WCGFS_2018_2024_ELFIC.V1.5_operation_2025-03-31.csv"), 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

# Remove duplicated operation records
operation_WCGFS <- operation_WCGFS %>% dplyr::distinct()


# ------------------------------------------------------------------------------ #
#### 2. PROCESS AND HARMONISE CGFS SURVEY DATA ####
# ------------------------------------------------------------------------------ #

# ------------------ #
# Eastern CGFS
# ------------------ #

# Extract catch records for the target species
# Retain haul identifiers, total abundance and total catch weight
sp_catch_ECGFS <- catch_ECGFS %>%
  dplyr::filter(scientificName == sp_scientific) %>%
  dplyr::select(serie, year, stationCode, operationID, haulID, totalNumber, totalWeightKg)

# Join species-specific catches to the complete operation dataset
sp_data_ECGFS <- operation_ECGFS %>%
  dplyr::left_join(sp_catch_ECGFS, by = c("serie", "year", "stationCode", 
                                          "operationID", "haulID"))%>%
  dplyr::filter(year <= 2023)


# ------------------ #
# Western CGFS
# ------------------ #

# Extract catch records for the target species
# Retain haul identifiers, total abundance and total catch weight
sp_catch_WCGFS <- catch_WCGFS %>%
  dplyr::filter(scientificName == sp_scientific) %>%
  dplyr::select(serie, year, stationCode, operationID, haulID, totalNumber, totalWeightKg)

# Join species-specific catches to the complete operation dataset
sp_data_WCGFS <- operation_WCGFS %>%
  dplyr::left_join(sp_catch_WCGFS, by = c("serie", "year", "stationCode", 
                                          "operationID", "haulID"))%>%
  dplyr::filter(year <= 2023)


# --------------------------------- #
# Combine Eastern and Western CGFS
# --------------------------------- #
# Merge both survey datasets into a single data frame
# This provides a harmonised dataset for subsequent processing
sp_data_CGFS <-  dplyr::bind_rows(sp_data_ECGFS, sp_data_WCGFS)

# Prepare haul-level response variables for species distribution modelling:
# - Replace missing species catches with zero (assumed absence).
# - Calculate the midpoint of each trawl using start and end coordinates.
# - Standardise catch weight by swept area to obtain biomass density.
# - Convert catch weight into a binary presence-absence variable.
sp_data_CGFS_density <- sp_data_CGFS %>%
  dplyr::mutate(totalNumber = ifelse(is.na(totalNumber), 0, totalNumber),
                totalWeightKg = ifelse(is.na(totalWeightKg), 0, totalWeightKg)) %>%
  dplyr::mutate(lat = (endLatDD + startLatDD)/2,
                lon = (endLongDD + startLongDD)/2) %>%
  dplyr::mutate(densityKgKm2 = totalWeightKg/sweptAreaKm2) %>%
  dplyr::mutate(presence_absence = ifelse(totalWeightKg == 0, 0, 1))


# ------------------------------------------------------------------------------ #
#### 3. TRANSFORM GEOGRAPHIC COORDINATES INTO UTM ####
# ------------------------------------------------------------------------------ #

# Identify an appropriate UTM coordinate reference system (CRS) based on the
# geographic extent of the sampling stations
utm_crs_used <- sdmTMB::get_crs(sp_data_CGFS_density, c("lon", "lat"))

# Transform geographic coordinates (longitude/latitude, WGS84)
# into projected UTM coordinates.
#
# Projected coordinates expressed in kilometres are required
# for spatial modelling approaches that rely on Euclidean distances,
# such as spatial random fields.
data_CGFS_crs <- sdmTMB::add_utm_columns(
  sp_data_CGFS_density,
  ll_names = c("lon", "lat"),  # input longitude and latitude
  ll_crs = 4326,               # input CRS: WGS84 (EPSG:4326)
  utm_names = c("X", "Y"),     # output projected coordinates
  utm_crs = utm_crs_used,      # selected UTM projection
  units = "km"                 # output coordinates in kilometres
)

# ------------------ #
# Eastern CGFS
# ------------------ #
# Select hauls conducted with the GOV 36/47 fishing gear
# Retain response variables and spatial coordinates required for modelling, 
# and convert year into a categorical variable
data_CGFS_east_all <- data_CGFS_crs %>% 
  dplyr::filter(gear == "GOV 36/47") %>%
  dplyr::select(year, X, Y, lat, lon, presence_absence, densityKgKm2, totalWeightKg, sweptAreaKm2) %>%
  dplyr::mutate(year = factor(year)) 

# ------------------ #
# Western CGFS
# ------------------ #
# Select hauls conducted with the GOV 36/49 fishing gear
# Retain response variables and spatial coordinates required for modelling, 
# and convert year into a categorical variable
data_CGFS_west_all <- data_CGFS_crs %>% 
  dplyr::filter(gear == "GOV 36/49") %>%
  dplyr::select(year, X, Y, lat, lon, presence_absence, densityKgKm2, totalWeightKg, sweptAreaKm2) %>%
  dplyr::mutate(year = factor(year)) 

  
# ------------------------------------------------------------------------------ #
#### 4. RESTRICT SAMPLING STATIONS TO ICES DIVISIONS 7d AND 7e ####
# ------------------------------------------------------------------------------ #
# Apply a spatial filter to retain only hauls located within the corresponding ICES division 
# Haul locations are represented by their midpoint coordinates

# --------------------------------- #
# Eastern English Channel (ICES 7d)
# --------------------------------- #
# Add a unique row identifier to preserve the link with the original dataset
data_CGFS_east_all <- data_CGFS_east_all %>% 
  dplyr::mutate(row_id = dplyr::row_number())

# Convert haul midpoint coordinates into a terra spatial vector using the WGS84 
# geographic coordinate reference system.
pts_east <- terra::vect(data_CGFS_east_all, 
                        geom = c("lon", "lat"), 
                        crs = "EPSG:4326")

# Reproject stations to match the CRS of the ICES 7d division polygon
pts_proj_east <- terra::project(pts_east, terra::crs(east7d))

# Identify stations located within or on the boundary of ICES division 7d
inside_east7d <- terra::relate(pts_proj_east, east7d, 
                               relation = "intersects")[, 1]

# Subset the original dataset to preserve lon, lat, X, Y and all other variables
data_CGFS_east <- data_CGFS_east_all %>%
  dplyr::filter(inside_east7d) %>%
  dplyr::select(-row_id)

# --------------------------------- #
# Western English Channel (ICES 7e)
# --------------------------------- #
# Add a unique row identifier to preserve the link with the original dataset
data_CGFS_west_all <- data_CGFS_west_all %>% 
  dplyr::mutate(row_id = dplyr::row_number())

# Convert survey stations to a spatial object using geographic coordinates
pts_west <- terra::vect(data_CGFS_west_all, 
                        geom = c("lon", "lat"), 
                        crs = "EPSG:4326")

# Reproject stations to match the CRS of the ICES 7e division polygon
pts_proj_west <- terra::project(pts_west, terra::crs(west7e))

# Identify stations located within or on the boundary of ICES division 7e
inside_west7e <- terra::relate(pts_proj_west, west7e, 
                               relation = "intersects")[, 1]

# Subset the original dataset to preserve lon, lat, X, Y and all other variables
data_CGFS_west <- data_CGFS_west_all %>%
  dplyr::filter(inside_west7e) %>%
  dplyr::select(-row_id)
