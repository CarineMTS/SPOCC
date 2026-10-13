
# ============================================================================== #
# PREDICT CONVERGED SDMTMB MODELS AND COMPUTE ABUNDANCE INDICES
#
# Objectives:
# - Generate spatial and spatiotemporal predictions for all converged models.
# - Predict each model on the corresponding regional prediction grid.
# - Apply a swept-area offset for total catch weight models.
# - Compute bias-corrected abundance indices from model predictions.
# ============================================================================== #

# ------------------------------------------------------------------------------ #
#### 1. DEFINE MODEL PREDICTION FUNCTION ####
# ------------------------------------------------------------------------------ #

# Predict all converged models for a selected region.
# Return predictions and bias-corrected indices by mesh, response and family.
predict_all_converged_models <- function(region_name) {
  
  # Retrieve models and prediction grid for the selected region
  models_by_mesh <- converged_models[[region_name]]
  grid_region_base <- grid_by_region[[region_name]]$grid_pred
  
  # Check that models and prediction grid are available
  if (is.null(models_by_mesh) || is.null(grid_region_base)) {
    stop("Missing converged models or prediction grid for region: ", region_name)
  }
  
  # Initialise regional model prediction results
  model_predictions_region <- list()
  
  # Loop over mesh configurations
  for (mesh_name in names(models_by_mesh)) {
    
    models_by_response <- models_by_mesh[[mesh_name]]
    model_predictions_region[[mesh_name]] <- list()
    
    # Loop over response variables
    for (response_name in names(models_by_response)) {
      
      models_by_family <- models_by_response[[response_name]]
      model_predictions_region[[mesh_name]][[response_name]] <- list()
      
      # Prepare the regional prediction grid
      grid_region <- grid_region_base
      
      # Apply a swept-area offset for total catch weight models
      if (response_name == "totalWeightKg") {
        grid_region$logSweptAreaKm2 <- log(grid_region$cell_area_km2)
        offset_pred <- grid_region$logSweptAreaKm2
      } else {
        offset_pred <- NULL
      }
      
      
      # Loop over model distribution families
      for (family_name in names(models_by_family)) {
        
        # Retrieve the converged model
        fitted_model <- models_by_family[[family_name]]
        
        # Generate predictions on the response scale
        predictions <- predict(fitted_model, 
                               newdata = grid_region, 
                               type = "response", 
                               model = NA,
                               offset = offset_pred)
        
        # Retrieve the TMB prediction object for index calculation
        predictions_index <- predict(fitted_model,
                                     newdata = grid_region,
                                     type = "response",
                                     model = NA,
                                     offset = offset_pred,
                                     return_tmb_object = TRUE)
        
        # Compute bias-corrected abundance indices
        index <- get_index(predictions_index, 
                           bias_correct = TRUE)
        
        
        # Store predictions and indices using the original model structure
        model_predictions_region[[mesh_name]][[response_name]][[family_name]] <- list(
          predictions = predictions,
          index = index
        )
      }
    }
  }
  # Return prediction results for the selected region
  return(model_predictions_region)
}


# ------------------------------------------------------------------------------ #
#### 2. GENERATE MODEL PREDICTIONS BY REGION ####
# ------------------------------------------------------------------------------ #

# Initialise model predictions for all selected regions
model_predictions_by_region <- list()

# Generate predictions for the Western English Channel
if (isTRUE(West_English_Channel)) {
  model_predictions_by_region$west <- predict_all_converged_models(
    region_name = "west"
  )
}

# Generate predictions for the Eastern English Channel
if (isTRUE(East_English_Channel)) {
  model_predictions_by_region$east <- predict_all_converged_models(
    region_name = "east"
  )
}


# ------------------------------------------------------------------------------ #
#### 3. SAVE MODEL PREDICTIONS AND ABUNDANCE INDICES ####
# ------------------------------------------------------------------------------ #

# Save spatial predictions and bias-corrected abundance indices by region
saveRDS(model_predictions_by_region,
        file.path(new_repertory_model,
                  paste0(sp_name_safe, "_model_predictions_by_region.rds")))