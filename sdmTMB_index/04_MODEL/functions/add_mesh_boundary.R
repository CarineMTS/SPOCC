
# ============================================================================== #
# Prepare coastline boundaries for spatial mesh construction.
# The function simplifies the coastline geometry to reduce the number of small 
# triangles generated along complex coastal boundaries, while preserving the 
# overall shape of the coastline.
# The resulting boundary segments are used to construct a spatial mesh that
# accounts for the land-sea boundary.
# ============================================================================== #


add_boundary_to_mesh <- function(EC_shp, simplify_keep) {
  
  # Convert the input shapefile into an sf object
  EC_sf <- sf::st_as_sf(EC_shp)
  
  # Project it to the UTM CRS used for the survey data.
  EC_sf_utm <- sf::st_transform(EC_sf, utm_crs_used)
  
  # Repair invalid geometries and merge polygons into a single geometry 
  # defining the study area
  EC_boundary_m <- EC_sf_utm %>%
    sf::st_make_valid() %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")
  
  # Simplify the boundary to reduce geometric complexity while preserving the
  # main polygon shapes
  # simplify_keep controls the proportion of vertices retained.
  EC_boundary_simple_m <- rmapshaper::ms_simplify(EC_boundary_m,
                                                  keep = simplify_keep,
                                                  keep_shapes = TRUE)
  
  # Convert projected coordinates from metres to kilometres to ensure 
  # consistency with the X/Y coordinates used in the model
  EC_boundary_km <- EC_boundary_simple_m
  sf::st_geometry(EC_boundary_km) <- sf::st_geometry(EC_boundary_km) / 1000
  
  
  # Convert the simplified boundary into a SpatialPolygons object
  EC_sp <- methods::as(methods::as(EC_boundary_km, "Spatial"), "SpatialPolygons")
  
  # Convert the SpatialPolygons object into a segment representation compatible with fmesher
  bnd_segm <- fmesher::fm_as_segm(EC_sp)
  
  # Return the boundary segments for spatial mesh construction.
  return(bnd_segm)
}