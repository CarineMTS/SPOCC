
# ============================================================================== #
# CONSTRUCT SPATIAL MESHES FOR THE SPDE APPROACH
#
# Objectives:
# - Build spatial meshes for the Eastern and Western English Channel.
# - Use simplified coastlines as mesh boundaries to reduce small coastal triangles.
# - Test different cutoff and maximum edge configurations.
# - Visualise meshes, sampling stations and land areas.
# - Store mesh configurations for subsequent spatial modelling.
# ============================================================================== #


# ------------------------------------------------------------------------------ #
#### 1. DEFINE SPATIAL MESH CONSTRUCTION FUNCTION ####
# ------------------------------------------------------------------------------ #

# Build and visualise spatial meshes for a given region and mesh configuration.
create_meshes_configurations <- function(region_name,
                                         cutoff_values,
                                         max_edge_in, 
                                         max_edge_out) {
  
  
  
  # Select survey data, study area boundaries and coastline simplification
  # parameters for the specified region
  if (region_name == "west") {
    data_CGFS <- data_CGFS_west
    EC_shp <- vect(here("01_DATA", "shapefiles", "west7e", "west7e.shp"))
    simplify_keep = 0.0025
    
  } else if (region_name == "east") {
    data_CGFS <- data_CGFS_east
    EC_shp <- vect(here("01_DATA", "shapefiles", "east7d", "east7d.shp"))
    simplify_keep = 0.005
  }
  
  # ------------------------------------------------------------------------------ #
  #### 1.1. PREPARE THE CONSTUCTION OF THE MESH ####
  # ------------------------------------------------------------------------------ #
  # Initialise a list to store each mesh configuration
  configs <- vector("list", length(cutoff_values))
  names(configs) <- paste0("cutoff_", cutoff_values)
  
  # Loop over mesh configurations
  for (i in seq_along(cutoff_values)) {
    
    cutoff  <- cutoff_values[i]
    max_edge <- c(max_edge_in[i], max_edge_out[i])

    message(glue::glue(
      "{sp_scientific} | {region_name} | Testing cutoff = {cutoff} ; max.edge = ({max_edge[1]}, {max_edge[2]})"
    ))
    
    # Construct the mesh using simplified coastline boundaries
    mesh <- sdmTMB::make_mesh(
      data_CGFS, c("X", "Y"),
      fmesher_func = fmesher::fm_mesh_2d_inla,
      max.edge = max_edge,
      cutoff = cutoff,
      boundary  = add_boundary_to_mesh(EC_shp = EC_shp, 
                                       simplify_keep = simplify_keep)
      )



    # ------------------------------------------------------------------------------ #
    #### 1.2. PREPARE SPATIAL DATA FOR MESH VISUALISATION ####
    # ------------------------------------------------------------------------------ #
    # Convert mesh coordinates from kilometres to metres for mapping
    mesh_m <- mesh$mesh
    mesh_m$loc <- mesh_m$loc * 1000
    
    # Calculate the full mesh extent to crop land polygons
    mesh_m_bbox <- st_bbox(c(xmin = min(mesh_m$loc[, 1]),
                             xmax = max(mesh_m$loc[, 1]),
                             ymin = min(mesh_m$loc[, 2]),
                             ymax = max(mesh_m$loc[, 2])), crs = utm_crs_used)
    
    # Reproject and crop land polygons to the mesh extent
    land_mesh <- land_ne %>%
      st_transform(utm_crs_used) %>%
      st_crop(mesh_m_bbox)
    
    
    # ------------------------------------------------------------------------------ #
    #### 1.3. VISUALISE SPATIAL MESH ####
    # ------------------------------------------------------------------------------ #
    # Plot the mesh, land areas, and triangle centers
    plot_mesh <-  ggplot2::ggplot() +
      ggplot2::geom_sf(data = land_mesh, fill = "grey80", color = NA) +
      inlabru::gg(mesh_m, edge.color = "grey50", edge.linewidth = 0.2) +
      ggspatial::geom_spatial_point(data = data_CGFS, aes(lon, lat), 
                                    color = "black", size = 1.5, alpha = 0.5)+
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 4))+
      ggplot2::coord_sf(crs = st_crs(land_mesh)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = element_blank(),
                     axis.ticks = element_line(colour = "black"))+
      ggplot2::labs(x = NULL, y = NULL,
                    title = paste0("Mesh with cutoff = ", round(cutoff,1), "km"))


    # ------------------------------------------------------------------------------ #
    #### 1.4. STORE MESH CONFIGURATION ####
    # ------------------------------------------------------------------------------ #
    
    # Store mesh parameters, mesh object and associated plot
    configs[[i]] <- list(
      cutoff = cutoff,
      max_edge = max_edge,
      mesh = mesh,
      plot_mesh = plot_mesh
    )
  }
  
    return(configs)

}



# ------------------------------------------------------------------------------ #
#### 2. CONSTRUCT SPATIAL MESHES BY REGION ####
# ------------------------------------------------------------------------------ #

# Initialise the list of regional mesh configurations
meshes_by_region <- list()

# Construct candidate meshes for the Western English Channel
if (isTRUE(West_English_Channel)) {
  meshes_by_region$west <- do.call(create_meshes_configurations,
                                   c(list(region_name = "west"), west_params))
}

# Construct candidate meshes for the Eastern English Channel
if (isTRUE(East_English_Channel)) {
  meshes_by_region$east <- do.call(create_meshes_configurations,
                                   c(list(region_name = "east"), east_params))
}


# ------------------------------------------------------------------------------ #
#### 3. SAVE REGIONAL SPATIAL MESH CONFIGURATIONS ####
# ------------------------------------------------------------------------------ #

# Save mesh configurations, triangulations and associated plots
saveRDS(meshes_by_region,
        file.path(new_repertory_model, 
                  paste0(sp_name_safe, "_meshes_by_region.rds")))



