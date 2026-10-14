
# ============================================================================== #
# VISUALISATION OF SPATIOTEMPORAL BIOMASS PREDICTIONS
#
# Objectives:
# - Prepare land polygons and regular prediction grids for mapping.
# - Preserve the native UTM projection to avoid raster distortion.
# - Generate annual biomass prediction maps for each region, mesh
#   configuration and converged model.
# - Store prediction maps for model comparison and visual assessment.
# ============================================================================== #


# ------------------------------------------------------------------------------ #
#### 1. PREPARE LAND POLYGONS FOR EACH REGION ####
# ------------------------------------------------------------------------------ #

# Crop land polygons to each prediction grid extent.
# Keep coordinates in UTM metres to preserve the regular raster grid.
get_land_region <- function(grid_pred){
  
  # Define plot extent with a one-cell buffer
  res_m <- res_km * 1000
  xmin_plot <- range(grid_pred$X_map_m,na.rm=TRUE)[1]-res_m
  xmax_plot <- range(grid_pred$X_map_m,na.rm=TRUE)[2]+res_m
  ymin_plot <- range(grid_pred$Y_map_m,na.rm=TRUE)[1]-res_m
  ymax_plot <- range(grid_pred$Y_map_m,na.rm=TRUE)[2]+res_m
  
  # Reproject and crop land polygons
  land <- land_ne %>%
    sf::st_transform(utm_crs_used) %>%
    sf::st_crop(xmin=xmin_plot,ymin=ymin_plot,xmax=xmax_plot,ymax=ymax_plot)
  
  list(land=land,xlim=c(xmin_plot,xmax_plot),ylim=c(ymin_plot,ymax_plot))
}

# Prepare land polygons for Eastern and Western regions
land_by_region <- list()
if (!is.null(grid_by_region$east$grid_pred))
  land_by_region$east <- get_land_region(grid_by_region$east$grid_pred)

if (!is.null(grid_by_region$west$grid_pred))
  land_by_region$west <- get_land_region(grid_by_region$west$grid_pred)


# ------------------------------------------------------------------------------ #
#### 2. STORE REGULAR GRID COORDINATES ####
# ------------------------------------------------------------------------------ #

# Store nominal UTM coordinates for each prediction cell and year.
# These coordinates ensure regular spacing required by geom_raster()
# and can be reattached if lost during model predictions.
map_coords_by_region <- lapply(grid_by_region, function(g) {
  g$grid_pred %>%
    dplyr::select(cell_id, year, X_map_m, Y_map_m) %>%
    dplyr::distinct()
})


# ------------------------------------------------------------------------------ #
#### 3. DEFINE PREDICTION MAPPING FUNCTION ####
# ------------------------------------------------------------------------------ #

# Plot annual biomass predictions on a regular UTM grid,
# with land polygons overlaid and a logarithmic colour scale.
plot_pred_map <- function(data, land_obj, response,subtitle){
  # Set legend title according to the response variable
  legend_title <- ifelse(response=="totalWeightKg","Biomass (kg)","Density of \nbiomass \n(kg/km²)")
  
  ggplot2::ggplot(data,aes(X_map_m,Y_map_m,fill=est)) +
    ggplot2::geom_raster() +
    ggplot2::geom_sf(data=land_obj$land,fill="grey80",color="grey80",inherit.aes=FALSE) +
    # Keep the native UTM CRS to avoid raster grid distortion
    ggplot2::coord_sf(crs = utm_crs_used, 
                      xlim = land_obj$xlim,
                      ylim = land_obj$ylim,
                      expand = FALSE) +
    # Display UTM coordinates in kilometres
    ggplot2::scale_y_continuous(breaks = pretty_breaks(n=3),
                                labels = scales::label_number(scale=1e-3)) +
    ggplot2::scale_x_continuous(breaks = pretty_breaks(n=4),
                                labels = scales::label_number(scale=1e-3)) +
    ggplot2::facet_wrap(~year, nrow=6) +
    # Apply a logarithmic colour scale to predicted values
    ggplot2::scale_fill_distiller(trans = "log10", 
                                  palette = "RdYlBu",
                                  oob = scales::squish,
                                  labels = label_number(drop0trailing=TRUE)) +
    ggplot2::theme(panel.grid = element_blank(),
                   panel.background = element_rect(fill="white", colour=NA), 
                   panel.border = element_rect(color="white", fill=NA, linewidth=2),
                   strip.background=element_rect(fill = "grey40", color = "white"),
                   strip.text=element_text(color = "white", face = "bold")) +
    ggplot2::labs(x = NULL, y = NULL, subtitle = subtitle, fill = legend_title)
}


# ------------------------------------------------------------------------------ #
#### 4. GENERATE PREDICTION MAPS ####
# ------------------------------------------------------------------------------ #

# Generate annual biomass prediction maps for each region,
# mesh resolution and converged model family.
prediction_maps <- list()

for(region in names(model_predictions_by_region)){
  prediction_maps[[region]] <- list()
  land_obj <- land_by_region[[region]]
  
  # Retrieve regular grid coordinates for the current region
  map_coords <- map_coords_by_region[[region]]
  
  for (mesh_name in names(model_predictions_by_region[[region]])) {
    prediction_maps[[region]][[mesh_name]] <- list()
    
    for(model_name in names(model_predictions_by_region[[region]][[mesh_name]][["totalWeightKg"]])){
      # Extract model predictions
      predictions <- as.data.frame(
        model_predictions_by_region[[region]][[mesh_name]][["totalWeightKg"]][[model_name]]$predictions)
      
      # Restore regular UTM grid coordinates using cell_id and year
      # Remove existing columns first to avoid duplicate column names
      predictions <- predictions %>%
        dplyr::select(-dplyr::any_of(c("X_map_m", "Y_map_m"))) %>%
        dplyr::left_join(map_coords, by = c("cell_id", "year"))
      
      # Generate and store the prediction map
      prediction_maps[[region]][[mesh_name]][[model_name]] <-
        plot_pred_map(data = predictions,
                      land_obj = land_obj,
                      response = "totalWeightKg",
                      subtitle = paste(region, mesh_name, "totalWeightKg", model_name, sep = " - "))
    }
  }
}


# ------------------------------------------------------------------------------ #
#### 5. SAVE PREDICTION MAPS ####
# ------------------------------------------------------------------------------ #

saveRDS(prediction_maps, 
        file.path(new_repertory_model, 
                  paste0(sp_name_safe, "_", "prediction_maps.rds")))

