
# ============================================================================== #
# GENERATE SPATIOTEMPORAL PREDICTION MAPS FROM CONVERGED SDMTMB MODELS
#
# Objectives:
# - Extract and crop land polygons to each regional prediction grid.
# - Calculate common colour-scale limits by region and response variable.
# - Define a mapping function for annual spatial predictions.
# - Map predictions using regular-grid coordinates and land boundaries.
# - Store prediction maps by region, mesh, response and model family.
# ============================================================================== #


# ------------------------------------------------------------------------------ #
#### 1. DEFINE REGIONAL LAND EXTRACTION FUNCTION ####
# ------------------------------------------------------------------------------ #

# Crop land polygons to the prediction grid extent with an additional
# margin corresponding to one grid cell.
get_land_region <- function(grid_pred){
  
  # Convert grid resolution from kilometres to metres
  res_m <- res_km * 1000
  
  # Calculate plotting limits from regular-grid coordinates
  xmin_plot <- range(grid_pred$X_map_m,na.rm = TRUE)[1] - res_m
  xmax_plot <- range(grid_pred$X_map_m,na.rm = TRUE)[2] + res_m
  ymin_plot <- range(grid_pred$Y_map_m,na.rm = TRUE)[1] - res_m
  ymax_plot <- range(grid_pred$Y_map_m,na.rm = TRUE)[2] + res_m
  
  # Reproject and crop land polygons to the regional plotting extent
  land <- land_ne %>%
    sf::st_transform(utm_crs_used) %>%
    sf::st_crop(xmin=xmin_plot,ymin=ymin_plot,xmax=xmax_plot,ymax=ymax_plot)
  
  # Return cropped land polygons and plotting limits
  list(land = land,
       xlim = c(xmin_plot, xmax_plot), 
       ylim = c(ymin_plot, ymax_plot))
}

# ------------------------------------------------------------------------------ #
#### 2. EXTRACT LAND POLYGONS BY REGION ####
# ------------------------------------------------------------------------------ #

# Initialise regional land objects
land_by_region <- list()

# Extract land polygons for available prediction grids
if (!is.null(grid_by_region$east$grid_pred)) {
  land_by_region$east <- get_land_region(
    grid_by_region$east$grid_pred
  )
}

if (!is.null(grid_by_region$west$grid_pred)) {
  land_by_region$west <- get_land_region(
    grid_by_region$west$grid_pred
  )
}


# ------------------------------------------------------------------------------ #
#### 3. CALCULATE COMMON COLOUR-SCALE LIMITS ####
# ------------------------------------------------------------------------------ #

# Calculate prediction ranges across all meshes and model families,
# separately for each region and response variable.
fill_limits_by_region <- lapply(model_predictions_by_region, function(region_data) {
  
  response_names <- unique(unlist(
    lapply(region_data, function(mesh_data) names(mesh_data))
  ))
  
  # Identify response variables available across mesh configurations
  limits_by_response <- lapply(response_names, function(response_name) {
    
    # Calculate prediction limits for each response
    all_est_values <- unlist(lapply(region_data, function(mesh_data) {
      lapply(mesh_data[[response_name]], function(model_predictions) {
        as.data.frame(model_predictions$predictions)$est
      })
    }),
    recursive = TRUE
    )
    # Return the range of predicted values
    range(all_est_values, na.rm = TRUE)
  })
  # Name limits according to response variables
  names(limits_by_response) <- response_names
  
  limits_by_response
})




# ------------------------------------------------------------------------------ #
#### 4. DEFINE SPATIOTEMPORAL PREDICTION MAPPING FUNCTION ####
# ------------------------------------------------------------------------------ #

# Plot annual predictions on the regular UTM grid with land polygons
plot_pred_map <- function(data,land_obj, 
                          response,
                          subtitle,
                          fill_limits){
  
  legend_title <- if (response == "totalWeightKg") {
    "Biomass (kg)"
  } else {
    "Density of\nbiomass\n(kg/km²)"
  }
  
  ggplot2::ggplot(data,aes(X_map_m, Y_map_m, fill = est)) +
    ggplot2::geom_raster() +
    ggplot2::geom_sf(data = land_obj$land, 
                     fill = "grey80", color = "grey80", inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(breaks=pretty_breaks(n=3)) +
    ggplot2::scale_x_continuous(breaks=pretty_breaks(n=4)) +
    ggplot2::coord_sf(xlim = land_obj$xlim,
                      ylim = land_obj$ylim, 
                      expand = FALSE) +
    ggplot2::facet_wrap(~year, nrow=6) +
    ggplot2::scale_fill_distiller(trans="log10", palette="RdYlBu",
                                  labels = label_number(drop0trailing = TRUE)) +
    ggplot2::theme(panel.grid = element_blank(),
                   panel.background = element_rect(fill="white", colour=NA),
                   panel.border = element_rect(color="white", fill=NA, linewidth=2),
                   strip.background = element_rect(fill="grey40", color="white"),
                   strip.text = element_text(color="white", face="bold")) +
    ggplot2::labs(x = NULL, y = NULL, subtitle = subtitle, fill = legend_title)
}

# ------------------------------------------------------------------------------ #
#### 5. GENERATE PREDICTION MAPS FOR ALL CONVERGED MODELS ####
# ------------------------------------------------------------------------------ #

# Initialise the nested list of prediction maps
prediction_maps <- list()

# Loop over regions
for(region in names(model_predictions_by_region)){
  
  prediction_maps[[region]] <- list()
  land_obj <- land_by_region[[region]]
  
  # Loop over mesh configurations
  for (mesh_name in names(model_predictions_by_region[[region]])) {
    prediction_maps[[region]][[mesh_name]] <- list()
    
    # Loop over response variables  
    for(response in names(model_predictions_by_region[[region]][[mesh_name]])){
      
      prediction_maps[[region]][[mesh_name]][[response]] <- list()
      
      # Retrieve common colour-scale limits for the current response
      fill_limits <- fill_limits_by_region[[region]][[response]]
      
      # Loop over fitted model families
      for(model_name in names(model_predictions_by_region[[region]][[mesh_name]][[response]])){
        
        # Extract model predictions
        predictions <- as.data.frame(
          model_predictions_by_region[[region]][[mesh_name]][[response]][[model_name]]$predictions)
        
        # Retrieve the prediction grid corresponding to the current region
        map_coords <- grid_by_region[[region]]$grid_pred %>%
          sf::st_drop_geometry() %>%
          dplyr::select(cell_id, year, X_map_m, Y_map_m) %>%
          dplyr::distinct()
        
        # Attach regular-grid coordinates to model predictions
        predictions <- predictions %>%
          dplyr::left_join(map_coords, by = c("cell_id", "year"))
        
        # Generate and store the annual prediction map
        prediction_maps[[region]][[mesh_name]][[response]][[model_name]] <-
          plot_pred_map(data = predictions,
                        land_obj = land_obj,
                        response = response,
                        subtitle = paste(region, mesh_name, response, model_name, sep = " - "),
                        fill_limits = fill_limits)
      }
    }
  }
}


# ------------------------------------------------------------------------------ #
#### 6. ACCESS INDIVIDUAL PREDICTION MAPS ####
# ------------------------------------------------------------------------------ #

# Example: Eastern English Channel, biomass density, delta-lognormal model
# prediction_maps$east$cutoff_15$densityKgKm2$deltalognormal

# Example: Western English Channel, total biomass, Tweedie model
# prediction_maps$west$cutoff_15$totalWeightKg$tweedie