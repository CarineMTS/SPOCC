# ============================================================================== #
# FIT CANDIDATE SPATIOTEMPORAL SDMTMB MODELS
#
# Objectives:
# - Fit candidate spatiotemporal models for each mesh configuration.
# - Use alternative distribution families for biomass and biomass density.
# - Account for swept area when modelling total catch biomass.
# - Store successfully fitted models and fitting errors separately.
# ============================================================================== #



# ------------------------------------------------------------------------------ #
#### 1. DEFINE CANDIDATE MODEL FITTING FUNCTION ####
# ------------------------------------------------------------------------------ #

# Fit candidate models across mesh configurations, response variables and
# distribution families for a given region.
fit_candidate_models <- function(region_name) {
  
  if (region_name == "west"){
    data_CGFS = data_CGFS_west  
    meshes_configurations = meshes_by_region$west
    }  
  else if (region_name == "east"){
    data_CGFS = data_CGFS_east
    meshes_configurations = meshes_by_region$east
  } else {
    stop("region_name must be 'west' or 'east'")
  }
  
  
  # ---------------------------------------------------------------------------- #
  #### 1.1. DEFINE CANDIDATE DISTRIBUTION FAMILIES / LIKELIHOODS ####
  # ---------------------------------------------------------------------------- #
  
  # Define candidate distributions for total biomass and biomass density.
  families_by_response <- list(
    
    # Candidate distributions for total biomass
    totalWeightKg = list(
      tweedie = sdmTMB::tweedie(link = "log"),
      deltagamma = sdmTMB::delta_gamma(link1 = "logit", link2 = "log"),
      deltalognormal = sdmTMB::delta_lognormal(link1 = "logit", link2 = "log"),
      deltagammapoissonlink = sdmTMB::delta_gamma(type = "poisson-link"),
      deltalognormalpoissonlink = sdmTMB::delta_lognormal(type = "poisson-link")
    ),
    # Candidate distributions for biomass density
    densityKgKm2 = list(
      tweedie = sdmTMB::tweedie(link = "log"),
      deltagamma = sdmTMB::delta_gamma(link1 = "logit", link2 = "log"),
      deltalognormal = sdmTMB::delta_lognormal(link1 = "logit", link2 = "log")
    )
  )
  
  # ---------------------------------------------------------------------------- #
  #### 1.2. DEFINE RESPONSE-SPECIFIC OFFSETS ####
  # ---------------------------------------------------------------------------- #
  
  # Initialise the list of fitted candidate models
  fitted_candidate_models <- list()
  
  # Use log-transformed swept area as an offset for total biomass.
  # Biomass density is already standardised per square kilometre.
  offset_by_response <- list(totalWeightKg = log(data_CGFS$sweptAreaKm2),
                             densityKgKm2  = NULL)
  
  
  # ---------------------------------------------------------------------------- #
  #### 1.3. LOOP OVER SPATIAL MESH CONFIGURATIONS ####
  # ---------------------------------------------------------------------------- #
  
  # Fit candidate models using each predefined regional mesh
  for (mesh_name in names(meshes_configurations)) {
    
    # Retrieve the current spatial mesh
    current_mesh <- meshes_configurations[[mesh_name]]$mesh
    
    # Initialise the model list for the current mesh
    fitted_candidate_models[[mesh_name]] <- list()
    
    # -------------------------------------------------------------------------- #
    #### 1.4. LOOP OVER RESPONSE VARIABLES ####
    # -------------------------------------------------------------------------- #
    for (response in responses) {
      
      # Construct the model formula for the current response
      model_formula <- as.formula(paste(response, "~", fixed_effect))
      
      # Select the candidate families corresponding to the current response
      families_chosen <- families_by_response[[response]]
      
      # Select the offset argument for the current response (if any)
      offset_arg <- offset_by_response[[response]]
      
      # Initialise lists for successful and failed model fits
      models_fitted <- list()              
      models_fit_failed <- list()          
      
      
      # ------------------------------------------------------------------------ #
      #### 1.5. FIT CANDIDATE DISTRIBUTION FAMILIES / LIKELIHOODS ####
      # ------------------------------------------------------------------------ #
      for (family_name in names(families_chosen)) {
        
        # Report the current model configuration
        cat("[",region_name, "English Channel ] Fitting:", mesh_name, "-", response, "-", family_name, "\n")
        
        # Fit the spatial and spatiotemporal model
        # Capture fitting errors without interrupting subsequent models
        current_candidate_model <- tryCatch({
          
          # Define the arguments passed to sdmTMB
          args <- list(
            data = data_CGFS,
            formula = model_formula,
            mesh = current_mesh,
            family = families_chosen[[family_name]],
            spatial = "on",
            time = "year",
            spatiotemporal = "IID"
          )
          
          # Apply the swept-area offset when required
          if (!is.null(offset_arg)) args$offset <- offset_arg
          
          # Fit the model using arguments
          do.call(sdmTMB::sdmTMB, args)
          
        }, error = function(e) e)   # Return the error object instead of stopping execution
        
        # Store fitting errors and continue to the next family
        if (inherits(current_candidate_model, "error")) {
          models_fit_failed[[family_name]] <- current_candidate_model$message    
          next
        }
        # Store successfully fitted models
        models_fitted[[family_name]] <- current_candidate_model
      }
      
      
      # ------------------------------------------------------------------------ #
      #### 1.6. STORE MODEL FITTING RESULTS ####
      # ------------------------------------------------------------------------ #
      
      # Store successful fits and errors for each response and mesh
      fitted_candidate_models[[mesh_name]][[response]] <- list(
        models_fitted = models_fitted,                
        models_fit_failed = models_fit_failed        
      )
    }
    
  }
  return(fitted_candidate_models)
  
}


# ------------------------------------------------------------------------------ #
#### 2. FIT CANDIDATE MODELS BY REGION ####
# ------------------------------------------------------------------------------ #

# Initialise the list of fitted models by region
fitted_models_by_region <- list()

# Fit candidate models for the Western English Channel
if (isTRUE(West_English_Channel)) {
  fitted_models_by_region$west <- fit_candidate_models(region_name = "west")
}

# Fit candidate models for the Eastern English Channel
if (isTRUE(East_English_Channel)) {
  fitted_models_by_region$east <- fit_candidate_models(region_name = "east")
}


# ------------------------------------------------------------------------------ #
#### 3. SAVE FITTED CANDIDATE MODELS ####
# ------------------------------------------------------------------------------ #

# Save all fitted candidate models 
saveRDS(fitted_models_by_region,
        file.path(new_repertory_model,
                  paste0(sp_name_safe, "_fitted_models_by_region.rds")))
