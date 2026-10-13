
# ============================================================================== #
# DESIGN REGULAR SPATIAL PREDICTION GRIDS
#
# Objectives:
# - Generate regular UTM grids for the Eastern and Western English Channel.
# - Clip grid cells to the regional boundaries and calculate their actual areas.
# - Extract clipped-cell centroids for spatial predictions and index estimation.
# - Preserve regular-grid centroids for raster-based mapping.
# - Replicate prediction locations across survey years.
# - Visualise the clipped prediction grid.
# ============================================================================== #

# ------------------------------------------------------------------------------ #
#### 1. DEFINE SPATIAL GRID CONSTRUCTION FUNCTION ####
# ------------------------------------------------------------------------------ #

# Build a regular prediction grid with clipped cell areas and two sets of
# coordinates: clipped-cell centroids for predictions and nominal grid
# centroids for raster mapping

design_grids <- function(region_name) {

  # Select regional survey data and spatial boundaries
  if (region_name == "west"){
    data_CGFS = data_CGFS_west
    EC_shp <- terra::vect(here::here("01_DATA/shapefiles/west7e/west7e.shp"))
    }
  else if (region_name == "east"){
    data_CGFS = data_CGFS_east
    EC_shp <- terra::vect(here::here("01_DATA/shapefiles/east7d/east7d.shp"))
    } 

  # ---------------------------------------------------------------------------- #
  #### 1.1. CONSTRUCT AND CLIP THE REGULAR UTM GRID ####
  # ---------------------------------------------------------------------------- #
  
  # Reproject, validate, merge and simplify the regional boundary
  EC_boundary_m <- EC_shp %>%
    sf::st_as_sf() %>%
    sf::st_transform(utm_crs_used) %>%
    sf::st_make_valid() %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
    rmapshaper::ms_simplify(keep = 0.01, keep_shapes = TRUE)
  
  # Convert the simplified sf boundary back to a terra SpatVector
  EC_boundary_vect <- terra::vect(EC_boundary_m)
  
  # Convert the requested grid resolution from kilometres to metres
  res_m <- res_km * 1000
  
  # Extract the spatial extent of the simplified regional boundary
  EC_ext <- terra::ext(EC_boundary_vect)
  
  # Create a regular raster grid in the model UTM CRS
  grid_rast <- terra::rast(ext = EC_ext,
                           resolution = c(res_m, res_m),
                           crs = terra::crs(EC_boundary_vect))
  
  # Assign a unique identifier to each raster cell
  grid_rast[] <- seq_len(terra::ncell(grid_rast))
  
  # Convert raster cells to individual polygons
  grid_cells <- terra::as.polygons(grid_rast, values = TRUE, aggregate = FALSE)
  
  # Rename the raster value column as the cell identifier
  names(grid_cells)[1] <- "cell_id"
  
  # Clip grid cells to the simplified regional boundary
  grid_cells_EC <- terra::intersect(grid_cells, EC_boundary_vect)
  
  # Calculate the surface area of each clipped cell in square kilometres
  grid_cells_EC$cell_area_km2 <- terra::expanse(grid_cells_EC, unit = "km")
  
  
  # ---------------------------------------------------------------------------- #
  #### 1.2. EXTRACT CLIPPED-CELL CENTROIDS FOR PREDICTIONS ####
  # ---------------------------------------------------------------------------- #
  
  # Calculate centroids of the clipped grid cells
  grid_centroids <- terra::centroids(grid_cells_EC)
  
  # Extract centroid coordinates in UTM metres
  coords_utm <- terra::crds(grid_centroids) %>%
    as.data.frame() %>%
    dplyr::rename(X_m = x, Y_m = y)
  
  # Build the prediction dataset and convert UTM coordinates to kilometres
  grid_pred <- terra::as.data.frame(grid_centroids) %>%
    dplyr::bind_cols(coords_utm) %>%
    dplyr::mutate(X = X_m / 1000,
                  Y = Y_m / 1000)
  
  # Reproject grid centroids to geographic coordinates
  grid_centroids_ll <- terra::project(grid_centroids, "EPSG:4326")
  
  # Extract centroid longitude and latitude
  coords_ll <- terra::crds(grid_centroids_ll) %>%
    as.data.frame() %>%
    dplyr::rename(lon = x, lat = y)
  
  # Add geographic coordinates and retain the variables required for prediction
  grid_pred <- grid_pred %>%
    dplyr::bind_cols(coords_ll) 
  
  
  # ---------------------------------------------------------------------------- #
  #### 1.3. EXTRACT REGULAR-GRID CENTROIDS FOR MAPPING ####
  # ---------------------------------------------------------------------------- #
  
  # Calculate the centroid of each cell from the FULL, UNCLIPPED grid
  grid_centroids_map <- terra::centroids(grid_cells)
  
  # Extract nominal centroid coordinates in UTM metres
  coords_utm_map <- terra::crds(grid_centroids_map) %>%
    as.data.frame() %>%
    dplyr::rename(X_map_m = x, Y_map_m = y)
  
  # Build the nominal-position dataset and convert UTM coordinates to kilometres
  grid_map <- terra::as.data.frame(grid_centroids_map) %>%
    dplyr::bind_cols(coords_utm_map) %>%
    dplyr::mutate(X_map = X_map_m / 1000,
                  Y_map = Y_map_m / 1000)
  
  # Reproject nominal grid centroids to geographic coordinates
  grid_centroids_map_ll <- terra::project(grid_centroids_map, "EPSG:4326")
  
  # Extract nominal centroid longitude and latitude
  coords_ll_map <- terra::crds(grid_centroids_map_ll) %>%
    as.data.frame() %>%
    dplyr::rename(lon_map = x, lat_map = y)
  
  # Add geographic coordinates and keep only cell_id + nominal coordinates,
  # ready to be joined back onto grid_pred
  grid_map <- grid_map %>%
    dplyr::bind_cols(coords_ll_map) %>%
    dplyr::select(cell_id, X_map, Y_map, X_map_m, Y_map_m, lon_map, lat_map)
  
  
  # ---------------------------------------------------------------------------- #
  #### 1.4. COMBINE PREDICTION AND MAPPING COORDINATES ####
  # ---------------------------------------------------------------------------- #
  
  # Match clipped-cell centroids with regular-grid centroids using cell_id.
  # X, Y, lon, lat: clipped-cell centroids used for predictions.
  # X_map, Y_map, lon_map, lat_map: regular-grid centroids used for mapping.
  # cell_area_km2: actual clipped-cell area used for index estimation.
  grid_pred <- grid_pred %>%
    dplyr::left_join(grid_map, by = "cell_id") %>%
    dplyr::select(cell_id, X, Y, lon, lat, X_map, Y_map, X_map_m, Y_map_m, 
                  lon_map, lat_map, cell_area_km2)
  
  # Replicate the spatial grid for every survey year available in the region
  grid_pred <- replicate_df(grid_pred, "year", factor(unique(data_CGFS$year)))
  
  
  
  # ---------------------------------------------------------------------------- #
  #### 1.5. VISUALISE THE CLIPPED PREDICTION GRID ####
  # ---------------------------------------------------------------------------- #
  
  # Reproject clipped grid cells to longitude and latitude for plotting
  grid_cells_sf <- terra::project(grid_cells_EC, "EPSG:4326") %>%
    sf::st_as_sf()
  
  # Extract the grid bounding box and add small plotting margins
  grid_bbox <- sf::st_bbox(grid_cells_sf)
  y_margin <- 0.005 * (grid_bbox["ymax"] - grid_bbox["ymin"])
  x_margin <- 0.009 * (grid_bbox["xmax"] - grid_bbox["xmin"])
  
  # Plot the clipped prediction grid with land represented by the grey background
  plot_grid <- ggplot() +
    ggplot2::geom_sf(data = grid_cells_sf, 
                     color = "black", fill = "white", linewidth = 0.2) +
    ggplot2::coord_sf(xlim = c(grid_bbox["xmin"] - x_margin, 
                               grid_bbox["xmax"] + x_margin),
                      ylim = c(grid_bbox["ymin"] - y_margin, 
                               grid_bbox["ymax"] + y_margin),
                      expand = FALSE) +
    ggplot2::theme(panel.grid = element_blank(),
                   panel.background = element_rect(fill = "grey90", colour = NA),
                   panel.border = element_rect(colour = "white", fill = NA, linewidth = 2),
                   strip.background = element_rect(fill = "grey40", colour = "white"),
                   strip.text = element_text(colour = "white", face = "bold")) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::ggtitle(bquote("Prediction grid (" * .(res_km) * "-km resolution)"))
  
  
  # ---------------------------------------------------------------------------- #
  #### 1.6. RETURN SPATIAL GRID OUTPUTS ####
  # ---------------------------------------------------------------------------- #
  # Return the regional prediction dataset, clipped grid cells and grid figure
  return(list(grid_pred = grid_pred,
              grid_cells = grid_cells_sf,
              plot_grid = plot_grid))
}



# ------------------------------------------------------------------------------ #
#### 2. CONSTRUCT THE GRID BY REGION ####
# ------------------------------------------------------------------------------ #

# Initialise the list of regional grid
grid_by_region <- list()

# Construct the grid for the Western English Channel
if (isTRUE(West_English_Channel)) grid_by_region$west <- design_grids("west")

# Construct the grid for the Eastern English Channel
if (isTRUE(East_English_Channel)) grid_by_region$east <- design_grids("east")

