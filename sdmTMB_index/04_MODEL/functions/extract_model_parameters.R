
# ============================================================================== #
# EXTRACT AND VISUALISE PARAMETER ESTIMATES FROM CONVERGED SDMTMB MODELS
#
# Objectives:
# - Extract fixed-effect intercepts and spatial random-effect parameters.
# - Distinguish the two components of delta models.
# - Retrieve parameter estimates and confidence intervals.
# - Compare parameter estimates across regions, meshes, responses and families.
# - Store parameter estimates for subsequent analyses and visualisation.
# ============================================================================== #

# ------------------------------------------------------------------------------ #
#### 1. DEFINE PARAMETER EXTRACTION FUNCTION ####
# ------------------------------------------------------------------------------ #

# Extract selected fixed and random parameters from all converged models
# within a region, including both components of delta models.
extract_params_converged_models <- function(converged_models, region) {
  
  # Extract all model for the selected region
  models_by_mesh  <- converged_models[[region]]
  
  # Return an empty tibble if no converged models are available
  if (is.null(models_by_mesh) || length(models_by_mesh) == 0) {
    return(tibble::tibble())
  }
  
  # LOOP OVER SPATIAL MESH CONFIGURATIONS 
  params_by_mesh  <- lapply(names(models_by_mesh), function(mesh_name) {
    
    models_by_response <- models_by_mesh[[mesh_name]]
    
    # LOOP OVER RESPONSE VARIABLES 
    params_by_response <- lapply(names(models_by_response), function(response) {
      
      models_by_family <- models_by_response[[response]]
      
      # LOOP OVER DISTRIBUTION FAMILIES 
      params_by_family <- lapply(names(models_by_family), function(family) {
      
      # Extract all model for the selected family
      model <- models_by_family[[family]]
      
      # Identify whether the model is a delta model
      is_delta <- grepl("^delta", family, ignore.case = TRUE)
      
      # ----------------------------------------------------------------------- #
      #### 1.1. EXTRACT FIXED-EFFECT PARAMETER ESTIMATES ####
      # ------------------------------------------------------------------------ #
      
      # Extract fixed-effect estimates and confidence intervals
      # For delta models, process the two model components separately.
      fixed_params <- if (is_delta) {
        dplyr::bind_rows(
          # First delta component: occurrence or encounter process
          sdmTMB::tidy(model, conf.int = TRUE, model = 1) %>% 
            dplyr::mutate(component = "delta_1"),
          # Second delta component: positive-response process
          sdmTMB::tidy(model, conf.int = TRUE, model = 2) %>% 
            dplyr::mutate(component = "delta_2")
        )
      } else {
        # For single-component model
        sdmTMB::tidy(model, conf.int = TRUE) %>% 
          dplyr::mutate(component = "single")
      }
      
      # Retain only the fixed-effect intercept
      fixed_params <- fixed_params %>%
        dplyr::filter(term == "(Intercept)") %>%
        dplyr::mutate(param_type = "fixed")
      
      
      # ------------------------------------------------------------------------------ #
      #### 1.2. EXTRACT RANDOM-EFFECT PARAMETER ESTIMATES ####
      # ------------------------------------------------------------------------------ #
      
      # Extract spatial and spatiotemporal random-effect parameters.
      # For delta models, process each component separately.
      random_params <- if (is_delta) {
        dplyr::bind_rows(
          # Random-effect parameters for the first delta component
          sdmTMB::tidy(model, effects = "ran_pars", conf.int = TRUE, model = 1) %>%
            dplyr::mutate(component = "delta_1"),
          # Random-effect parameters for the second delta component
          sdmTMB::tidy(model, effects = "ran_pars", conf.int = TRUE, model = 2) %>%
            dplyr::mutate(component = "delta_2")
        )
      } else {
        # Random-effect parameters for single-component models
        sdmTMB::tidy(model, effects = "ran_pars", conf.int = TRUE) %>%
          dplyr::mutate(component = "single")
      }
      
      # Keep only spatial and spatiotemporal variance parameters
      random_params <- random_params %>%
        dplyr::filter(term %in% c("range", "sigma_O", "sigma_E")) %>%
        dplyr::mutate(param_type = "random")
      
      
      # ------------------------------------------------------------------------ #
      #### 1.3. COMBINE FIXED AND RANDOM PARAMETER ESTIMATES ####
      # ------------------------------------------------------------------------ #
      
      # Combine fixed and random parameters 
      dplyr::bind_rows(fixed_params, random_params) %>%
        dplyr::mutate(region = region,
                      mesh = mesh_name,
                      response = response,
                      family = family,
                      parameter = if_else(term == "(Intercept)", "intercept", term)) %>%
        dplyr::select(region, response, mesh, family, component, param_type,
                      parameter, estimate, std.error, conf.low, conf.high)
    })
      # Combine parameters across model families
      dplyr::bind_rows(params_by_family)
    })
    # Combine parameters across response variables
    dplyr::bind_rows(params_by_response)
  })
  # Return parameter estimates across all mesh configurations
  dplyr::bind_rows(params_by_mesh)
}



# ------------------------------------------------------------------------------ #
#### 2. EXTRACT PARAMETER ESTIMATES BY REGION ####
# ------------------------------------------------------------------------------ #

# Initialise the list of regional parameter estimates
params_by_region <- list()

# Extract parameter estimates for the Western English Channel
if ("west" %in% names(converged_models)) {
  params_by_region$west <- extract_params_converged_models(
    converged_models = converged_models,
    region = "west"
  )
}

# Extract parameter estimates for the Eastern English Channel
if ("east" %in% names(converged_models)) {
  params_by_region$east <- extract_params_converged_models(
    converged_models = converged_models,
    region = "east"
  )
}


# ------------------------------------------------------------------------------ #
#### 3. SAVE PARAMETER ESTIMATES ####
# ------------------------------------------------------------------------------ #

# Save fixed-effect intercepts and random-effect parameter estimates,
# including uncertainty intervals, for all converged models.
saveRDS(params_by_region,
        file.path(new_repertory_model,
                  paste0(sp_name_safe, "_model_parameters.rds")))

