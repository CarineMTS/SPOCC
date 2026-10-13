# ============================================================================== #
# MAP SIMULATION-BASED COEFFICIENTS OF VARIATION
#
# Objectives:
# - Define common CV colour-scale limits across regions and models.
# - Prepare regional land polygons and map extents.
# - Generate annual CV maps for all converged models.
# - Store maps by region, mesh, response and family.
# ============================================================================== #

# ------------------------------------------------------------------------------ #
#### 1. DEFINE COMMON CV COLOUR-SCALE LIMITS ####
# ------------------------------------------------------------------------------ #

cv_range_global <- range(unlist(lapply(sim_cv_all, function(region_list) {
  lapply(region_list, function(mesh_list) {
    lapply(mesh_list, function(response_list) {
      lapply(response_list, function(df) df$cv)
    })
  })
})), na.rm = TRUE)

# ------------------------------------------------------------------------------ #
#### 2. DEFINE REGIONAL CV MAPPING FUNCTION ####
# ------------------------------------------------------------------------------ #

plot_simulated_cv <- function(region_name) {
  cv_predictions_region <- sim_cv_all[[region_name]]
  grid_region <- grid_by_region[[region_name]]$grid_pred
  
  # Define map extents with a one-cell buffer
  res_deg_lat <- res_km / 111.32
  res_deg_lon <- res_km / (111.32 * cos(50 * pi / 180))
  xlim <- range(grid_region$lon) + c(-res_deg_lon, res_deg_lon)
  ylim <- range(grid_region$lat) + c(-res_deg_lat, res_deg_lat)
  
  # Crop land polygons once per region
  land_region <- land_ne |>
    sf::st_transform(4326) |>
    sf::st_crop(xmin = xlim[1], ymin = ylim[1], xmax = xlim[2], ymax = ylim[2])
  
  cv_maps_region <- list()
  
  for (mesh_name in names(cv_predictions_region)) {
    for (response_name in names(cv_predictions_region[[mesh_name]])) {
      for (family_name in names(cv_predictions_region[[mesh_name]][[response_name]])) {
        df <- cv_predictions_region[[mesh_name]][[response_name]][[family_name]]
        
        cv_maps_region[[mesh_name]][[response_name]][[family_name]] <-
          ggplot2::ggplot(df, ggplot2::aes(lon, lat, fill = cv)) +
          ggplot2::geom_raster() +
          ggplot2::geom_sf(data = land_region, fill = "grey80", colour = "grey80",
                           inherit.aes = FALSE) +
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
          ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
          ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
          ggplot2::facet_wrap(~year, nrow = 2) +
          ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = "white", colour = NA),
            panel.border = ggplot2::element_rect(colour = "white", fill = NA, linewidth = 2),
            strip.background = ggplot2::element_rect(fill = "grey40", colour = "white"),
            strip.text = ggplot2::element_text(colour = "white", face = "bold")
          ) +
          ggplot2::labs(
            x = NULL, y = NULL,
            subtitle = paste(region_name, mesh_name, response_name, family_name, "CV", sep = " - "),
            fill = "CV"
          ) +
          ggplot2::scale_fill_viridis_c(trans = "log10", 
                                        limits = cv_range_global, 
                                        oob = scales::squish,
                                        labels = scales::label_number(drop0trailing = TRUE))
      }
    }
  }
  
  return(cv_maps_region)
}

# ------------------------------------------------------------------------------ #
#### 3. GENERATE CV MAPS BY REGION ####
# ------------------------------------------------------------------------------ #

plots_cv_all <- list()

if (isTRUE(West_English_Channel)) {
  plots_cv_all$west <- plot_simulated_cv("west")
}
if (isTRUE(East_English_Channel)) {
  plots_cv_all$east <- plot_simulated_cv("east")
}

# ------------------------------------------------------------------------------ #
#### 4. SAVE CV MAPS ####
# ------------------------------------------------------------------------------ #

saveRDS(plots_cv_all, 
        file.path(new_repertory_model,
                  paste0(sp_name_safe, "_coefficient_variation_maps.rds")))