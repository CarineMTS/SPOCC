# ============================================================================== #
# COMPUTE SIMULATION-BASED COEFFICIENTS OF VARIATION
#
# Objectives:
# - Simulate predictions from all converged sdmTMB models.
# - Calculate CV for each prediction cell and year.
# - Account for swept area when predicting total biomass.
# - Store CV predictions by region, mesh, response and family.
# ============================================================================== #

# ------------------------------------------------------------------------------ #
#### 1. DEFINE SIMULATION-BASED CV FUNCTION ####
# ------------------------------------------------------------------------------ #

compute_simulated_cv <- function(region_name) {
  models_by_mesh <- converged_models[[region_name]]
  grid_region <- grid_by_region[[region_name]]$grid_pred
  cv_predictions_region <- list()
  
  for (mesh_name in names(models_by_mesh)) {
    for (response_name in names(models_by_mesh[[mesh_name]])) {
      for (family_name in names(models_by_mesh[[mesh_name]][[response_name]])) {
        message("Simulating CV: ", region_name, " | ", mesh_name, " | ", response_name, " | ", family_name)
        
        fitted_model <- models_by_mesh[[mesh_name]][[response_name]][[family_name]]
        grid_pred <- grid_region
        
        # Apply the offset for total catch weight models
        if (response_name == "totalWeightKg") {
          grid_pred$logSweptAreaKm2 <- log(res_km * res_km)
          offset_pred <- grid_pred$logSweptAreaKm2
        } else {
          offset_pred <- NULL
        }
        
        # Simulate predictions on the response scale
        set.seed(123)
        simulations <- stats::predict(fitted_model, newdata = grid_pred, type = "response",
                                      model = NA, offset = offset_pred, nsim = 1000)
        
        # Calculate CV across simulations
        grid_pred$cv <- apply(simulations, 1, function(x) stats::sd(x) / mean(x))
        
        # Store CV predictions
        cv_predictions_region[[mesh_name]][[response_name]][[family_name]] <- grid_pred
      }
    }
  }
  
  return(cv_predictions_region)
}

# ------------------------------------------------------------------------------ #
#### 2. COMPUTE CV PREDICTIONS BY REGION ####
# ------------------------------------------------------------------------------ #

sim_cv_all <- list()

if (isTRUE(West_English_Channel)) {
  sim_cv_all$west <- compute_simulated_cv("west")
}
if (isTRUE(East_English_Channel)) {
  sim_cv_all$east <- compute_simulated_cv("east")
}

# ------------------------------------------------------------------------------ #
#### 3. SAVE SIMULATED CV PREDICTIONS ####
# ------------------------------------------------------------------------------ #

saveRDS(sim_cv_all, 
        file.path(new_repertory_model,
                  paste0(sp_name_safe, "_coefficient_variation_predictions.rds")))