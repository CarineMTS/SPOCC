# ============================================================================== #
# FILTER FITTED SDMTMB MODELS USING CONVERGENCE DIAGNOSTICS
#
# Objectives:
# - Apply sdmTMB sanity checks to all successfully fitted candidate models.
# - Identify models passing the convergence and numerical stability diagnostics.
# - Separate valid models from fitting and diagnostic failures.
# - Retain valid models for subsequent model selection and prediction.
# - Save diagnostic results for each selected region.
# ============================================================================== #


# ------------------------------------------------------------------------------ #
#### 1. DEFINE MODEL CONVERGENCE FILTERING FUNCTION ####
# ------------------------------------------------------------------------------ #

# Apply convergence diagnostics to all successfully fitted candidate models
# and classify them according to their fitting and diagnostic status.
sanity_filter_models <- function(fit_outputs) {
  
  # Initialise the list of diagnostic results
  sanity_results <- list()
  
  # Loop over mesh configurations
  for (mesh_name in names(fit_outputs)) {
    
    sanity_results[[mesh_name]] <- list()
    
    # Loop over response variables
    for (response in names(fit_outputs[[mesh_name]])) {
      
      # Retrieve successfully fitted models and fitting errors
      models_fitted <- fit_outputs[[mesh_name]][[response]]$models_fitted
      models_fit_failed <- fit_outputs[[mesh_name]][[response]]$models_fit_failed
      
      # Initialise lists for valid models and diagnostic failures
      models_valid <- list()
      models_sanity_failed <- list()
      
      # Loop over candidate families for this response
      for (family_name in names(models_fitted)) {
        
        # Print progress message
        cat("Sanity:", mesh_name, "-", response, "-", family_name, "\n")
        
        # Extract the fitted model
        current_checked_model <- models_fitted[[family_name]]
        
        # Run sdmTMB sanity checks on the fitted model
        # Run convergence diagnostics without interrupting the loop on errors
        sanity_res <- tryCatch(
          sdmTMB::sanity(current_checked_model), 
          error = function(e) e
        )
        
        # Classify models according to their diagnostic results
        # If sanity() fails or returns all_ok = FALSE, store in sanity_failed
        # all_ok = FALSE indicates that at least one convergence diagnostic has failed
        if (inherits(sanity_res, "error") || !isTRUE(sanity_res$all_ok)) {
          # Store models that failed the sanity checks
          models_sanity_failed[[family_name]] <- sanity_res
        } else {
          # Retain models passing all sanity checks
          models_valid[[family_name]] <- current_checked_model
        }
      }
      
      # Store results for the current response variable
      sanity_results[[mesh_name]][[response]] <- list(
        # Models that fitted successfully and passed sanity checks for this response
        models_valid  = models_valid,                    
        # Models that failed during model fitting for this response
        models_fit_failed  = models_fit_failed,    
        # Models that failed sanity diagnostics for this response
        models_sanity_failed  = models_sanity_failed    
      )
    }
    
  }
  # Return diagnostic results by mesh, response variable and model family
  return(sanity_results)
}


# ------------------------------------------------------------------------------ #
#### 2. APPLY CONVERGENCE FILTERING BY REGION ####
# ------------------------------------------------------------------------------ #

# Initialise the list of regional convergence diagnostic results
sanity_by_region <- list()

# Apply sanity checks to West English Channel models
if (isTRUE(West_English_Channel)) {
  sanity_by_region$west <- sanity_filter_models(fitted_models_by_region$west)
}

# Apply sanity checks to East English Channel models
if (isTRUE(East_English_Channel)) {
  sanity_by_region$east <- sanity_filter_models(fitted_models_by_region$east)
}

# ------------------------------------------------------------------------------ #
#### 3. SAVE REGIONAL CONVERGENCE DIAGNOSTIC RESULTS ####
# ------------------------------------------------------------------------------ #

# Save valid models, fitting errors and convergence diagnostic failures
saveRDS(sanity_by_region, 
        file.path(new_repertory_model, 
                  paste0(sp_name_safe, "_", "sanity_by_region.rds")))





