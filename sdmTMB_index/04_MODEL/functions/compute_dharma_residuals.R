# ============================================================================== #
# COMPUTE SIMULATION-BASED DHARMA RESIDUALS FOR SDMTMB MODELS
#
# Objectives:
# - Generate simulated responses for all converged sdmTMB models.
# - Compute simulation-based DHARMa residuals for model diagnostics.
# - Store DHARMa objects for subsequent residual diagnostic tests and plots.
# ============================================================================== #


# ------------------------------------------------------------------------------ #
#### 1. DEFINE DHARMA RESIDUAL COMPUTATION FUNCTION ####
# ------------------------------------------------------------------------------ #

# Generate DHARMa residual objects from simulated responses and fitted values.
# Returns a named list of DHARMa objects, one per converged model.
dharma_from_sim <- function(converged_models, region_name) {
  
  # Select data according to region 
  if (region_name == "west") {
    data_CGFS <- data_CGFS_west
  } else if (region_name == "east") {
    data_CGFS <- data_CGFS_east
  } 
  
  # Extract converged models and initialise the output list
  models_by_mesh <- converged_models[[region_name]]
  dharma_list <- list()
  
  # Loop over mesh 
  for (mesh_name in names(models_by_mesh)) {
    
    models_by_response <- models_by_mesh[[mesh_name]]
    
    dharma_list[[mesh_name]] <- list()
    
    # Loop over response variables
    for (response_name in names(models_by_response)) {
      
      models_by_family <- models_by_response[[response_name]]
      obs <- data_CGFS[[response_name]]
      
      dharma_list[[mesh_name]][[response_name]] <- list()
      
      # Loop over likelihoods/distribution families  
      for (family_name in names(models_by_family)) {
        
        fitted_model <- models_by_family[[family_name]]
        
        message("DHARMa residuals:", 
                region_name, " - ", mesh_name, " - ", response_name, " - ", family_name)
        
        # Set the random seed for reproducible simulations
        set.seed(123)
        
        # Simulate responses using the MLE-MVN approximation
        sim_matrix <- simulate(fitted_model, nsim = 1000, type = "mle-mvn")
        
        # Extract fitted predictions on the response scale
        fitted_pred <- predict(fitted_model, type = "response")$est
        
        # Create standardised simulation-based DHARMa residuals
        dharma_obj <- DHARMa::createDHARMa(simulatedResponse = as.matrix(sim_matrix),
                                           observedResponse = obs,
                                           fittedPredictedResponse = fitted_pred)
        
        # Store the diagnostic object using the model identifier
        dharma_list[[mesh_name]][[response_name]][[family_name]] <- dharma_obj
      }
    }
  }
  # Return DHARMa objects for all converged models in the selected region
  return(dharma_list)
}


# ------------------------------------------------------------------------------ #
#### 2. COMPUTE DHARMA RESIDUALS BY REGION ####
# ------------------------------------------------------------------------------ #

# Initialise regional DHARMa diagnostic results
dharma_by_region <- list()

# Compute DHARMa residuals for the Western English Channel
if (isTRUE(West_English_Channel)) {
  dharma_by_region$west <- dharma_from_sim(converged_models, "west")
}
# Compute DHARMa residuals for the Eastern English Channel
if (isTRUE(East_English_Channel)) {
  dharma_by_region$east <- dharma_from_sim(converged_models, "east")
}



# ------------------------------------------------------------------------------ #
#### 3. SAVE DHARMA DIAGNOSTIC RESULTS ####
# ------------------------------------------------------------------------------ #

# Save simulation-based DHARMa residual objects by region
saveRDS(dharma_by_region, 
        file.path(new_repertory_model, 
                  paste0(sp_name_safe, "_", "dharma_residuals.rds")))




