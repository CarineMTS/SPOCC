# ============================================================================== #
# EXTRACT CONVERGED SDMTMB MODELS FROM DIAGNOSTIC RESULTS
#
# Objectives:
# - Retrieve models that passed the convergence diagnostics.
# - Exclude missing or empty model lists.
# - Prepare converged models for subsequent model selection and predictions.
# - Prepare converged models for subsequent model selection and predictions.
# - Save converged models for each selected region.
# ============================================================================== #

# ------------------------------------------------------------------------------ #
#### 1. DEFINE CONVERGED MODEL EXTRACTION FUNCTION ####
# ------------------------------------------------------------------------------ #

# Extract models passing all convergence diagnostics from the regional
# sanity-check results, excluding empty or missing model configurations.
get_converged_models <- function(all_fitted_models) {
  
  # Initialize an empty list with one element per region
  converged_models <- setNames(vector("list", length(all_fitted_models)),
                               names(all_fitted_models))
  
  # Loop over regions
  for (region in names(all_fitted_models)) {
    region_list <- all_fitted_models[[region]]
    if (is.null(region_list)) next    # Skip empty or NULL regions
    
    # Loop over mesh configurations
    for (mesh_name in names(region_list)) {
      mesh_list <- region_list[[mesh_name]]
      if (is.null(mesh_list)) next
      
    # Loop over responses 
    for (response in names(mesh_list)) {
      
      # Retrieve models passing all sdmTMB sanity checks
      models_valid <- mesh_list[[response]]$models_valid
      
      # Store only non-empty valid model lists
      if (!is.null(models_valid)) {
        converged_models[[region]][[mesh_name]][[response]] <- models_valid
      }
    }
    }
  }
  # Remove regions that ended up empty
  converged_models <- converged_models[vapply(converged_models, length, integer(1)) > 0]
  
  # Return valid models organised by region, mesh, response and family
  return(converged_models)
}


# ------------------------------------------------------------------------------ #
#### 2. EXTRACT CONVERGED MODELS FROM REGIONAL DIAGNOSTICS ####
# ------------------------------------------------------------------------------ #
# Retrieve all models passing convergence and numerical stability diagnostics
converged_models <- get_converged_models(sanity_by_region)


# ------------------------------------------------------------------------------ #
#### 8. SAVE CONVERGED SDMTMB MODELS ####
# ------------------------------------------------------------------------------ #
# Save models passing the convergence diagnostics
saveRDS(converged_models,
        file.path(new_repertory_model, 
                  paste0(sp_name_safe, "_converged_models.rds")))