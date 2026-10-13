# ============================================================================== #
# REPEATED RANDOM CROSS-VALIDATION OF CONVERGED MODELS
#
# Objectives:
# - Perform repeated random k-fold cross-validation on converged models.
# - Evaluate each model family, response variable and mesh configuration.
# - Assess predictive performance using log-likelihood, RMSE, MAE and R².
# - Compute global metrics from concatenated cross-validation predictions.
# - Summarise model convergence and predictive performance across repetitions.
# ============================================================================== #

# ------------------------------------------------------------------------------ #
#### 1. DEFINE REPEATED CROSS-VALIDATION FUNCTION ####
# ------------------------------------------------------------------------------ #

# Perform repeated random k-fold CV for all converged models in a given region.
repeated_random_CV <- function(region_name) {
  
  # Select survey data for the specified region
  if (region_name == "west") {
    data_CGFS <- data_CGFS_west
  } 
  else if (region_name == "east") {
    data_CGFS <- data_CGFS_east
  } 
  else {
    stop("region_name must be 'west' or 'east'")
  }
  
  # Retrieve converged models and spatial meshes for the selected region
  models_by_mesh <- converged_models[[region_name]]
  meshes_region <- meshes_by_region[[region_name]]
  
  # Check that the region contains converged models and mesh configurations
  if (is.null(models_by_mesh) || length(models_by_mesh) == 0) {
    stop("No converged models available for region: ", region_name)
  }
  
  if (is.null(meshes_region) || length(meshes_region) == 0) {
    stop("No spatial meshes available for region: ", region_name)
  }
  
  # Calculate the log-transformed swept area for models using an offset
  data_CGFS <- data_CGFS %>%
    mutate(logSweptAreaKm2 = log(sweptAreaKm2))
  
  # Initialise nested lists for CV fits, scores and predictions
  cv_outputs <- list()
  cv_scores <- list()
  cv_predictions <- list()
  
  # Loop over mesh configurations
  for (mesh_name in names(models_by_mesh)) {
    
    models_by_response <- models_by_mesh[[mesh_name]]
    
    cv_outputs[[mesh_name]] <- list()
    cv_scores[[mesh_name]] <- list()
    cv_predictions[[mesh_name]] <- list()
    
    # Loop over response variables
    for (response_name in names(models_by_response)) {
      
      models_by_family <- models_by_response[[response_name]]
      
      cv_outputs[[mesh_name]][[response_name]] <- list()
      cv_scores[[mesh_name]][[response_name]] <- list()
      cv_predictions[[mesh_name]][[response_name]] <- list()
      
      # Loop over model distribution likelihoods/families
      for (family_name in names(models_by_family)) {
        
        fitted_model <- models_by_family[[family_name]]
        
        # Retrieve the fitted model specification and corresponding mesh
        model_formula <- formula(fitted_model)
        distr_family <- fitted_model$family
        mesh <- meshes_region[[mesh_name]]$mesh
        
        cv_outputs[[mesh_name]][[response_name]][[family_name]] <- list()
        cv_scores[[mesh_name]][[response_name]][[family_name]] <- list()
        cv_predictions[[mesh_name]][[response_name]][[family_name]] <- list()
        
        # ---------------------------------------------------------------------- #
        #### 1.1. PERFORM REPEATED RANDOM K-FOLD CROSS-VALIDATION ####
        # ---------------------------------------------------------------------- #
        
        # Repeat random k-fold partitioning
        for (rep_id in seq_len(n_repeats)) {
          
          # Generate reproducible, approximately balanced random folds
          set.seed(123 + rep_id)
          
          fold_ids <- sample(
            rep(seq_len(k_folds), length.out = nrow(data_CGFS))
          )
          
          # Specify the spatial and spatiotemporal CV model
          sdmTMB_cv_args <- list(
            formula = model_formula,
            data = data_CGFS,
            mesh = mesh,
            family = distr_family,
            spatial = "on",
            time = "year",
            spatiotemporal = "iid",
            fold_ids = fold_ids,
            k_folds = k_folds
          )
          
          # Apply a swept-area offset to total catch weight models
          if (response_name == "totalWeightKg") {
            sdmTMB_cv_args$offset <- "logSweptAreaKm2"
          }
          
          # Fit the CV model while capturing errors without stopping the loop
          cv_fit <- tryCatch(
            do.call(sdmTMB::sdmTMB_cv, sdmTMB_cv_args),
            error = function(e) e
          )
          
          # Store the complete cross-validation (cv) output
          cv_outputs[[mesh_name]][[response_name]][[family_name]][[rep_id]] <- cv_fit
          
          
          # -------------------------------------------------------------------- #
          #### 1.2. RECORD FAILED CROSS-VALIDATION RUNS ####
          # -------------------------------------------------------------------- #
          
          # Record failed CV runs and continue to the next repetition
          if (inherits(cv_fit, "error")) {
            
            cv_scores[[mesh_name]][[response_name]][[family_name]][[rep_id]] <- tibble(
              region = region_name,
              mesh = mesh_name,
              response = response_name,
              family = family_name,
              repeat_id = rep_id,
              sum_loglik = NA_real_,
              rmse = NA_real_,
              mae = NA_real_,
              n = NA_integer_,
              n_folds_tot = NA_integer_,
              n_converged = NA_integer_,
              percent_converged = NA_real_,
              all_converged = FALSE,
              error = cv_fit$message
            )
            
            next
          }
          
          # ------------------------------------------------------------------------------ #
          #### 1.3. EXTRACT CROSS-VALIDATED PREDICTIONS ####
          # ------------------------------------------------------------------------------ #
          
          # Extract observed and out-of-fold predicted values
          obs <- cv_fit$data[[response_name]]
          pred <- cv_fit$data$cv_predicted
          
          # Retain finite observation-prediction pairs
          ok <- is.finite(obs) & is.finite(pred)
          
          # Store individual CV predictions for global performance assessment
          cv_predictions[[mesh_name]][[response_name]][[family_name]][[rep_id]] <- 
            tibble::tibble(
              region = region_name,
              mesh = mesh_name,
              response = response_name,
              family = family_name,
              repeat_id = rep_id,
              row_id = seq_along(obs),
              observed = obs,
              predicted = pred
              ) %>%
            dplyr::filter(is.finite(observed), is.finite(predicted))
          
          
          # ------------------------------------------------------------------------------ #
          #### 1.4. CALCULATE REPETITION-LEVEL PERFORMANCE ####
          # ------------------------------------------------------------------------------ #
          
          # Compute predictive errors and convergence diagnostics per repetition
          cv_scores[[mesh_name]][[response_name]][[family_name]][[rep_id]] <- 
            tibble::tibble(
              region = region_name,
              mesh = mesh_name,
              response = response_name,
              family = family_name,
              repeat_id = rep_id,
              sum_loglik = cv_fit$sum_loglik,
              rmse = sqrt(mean((obs[ok] - pred[ok])^2, na.rm = TRUE)),
              mae = mean(abs(obs[ok] - pred[ok]), na.rm = TRUE),
              n = sum(ok),
              n_folds_tot = length(cv_fit$pdHess),
              n_converged = sum(cv_fit$pdHess),
              percent_converged = 100 * mean(cv_fit$pdHess),
              all_converged = isTRUE(cv_fit$converged),
              error = NA_character_
              )
        }
      }
    }
  }
  
  # ---------------------------------------------------------------------------- #
  #### 2. COMBINE CROSS-VALIDATION RESULTS ####
  # ---------------------------------------------------------------------------- #
  
  # Combine repetition-level scores across meshes, responses and families
  cv_scores_df <- map_dfr(cv_scores, function(mesh_list) {
    map_dfr(mesh_list, function(response_list) {
      map_dfr(response_list, function(family_list) {
        bind_rows(family_list)
      })
    })
  })
  
  # Concatenate individual predictions from all CV repetitions
  cv_predictions_df <- map_dfr(cv_predictions, function(mesh_list) {
    map_dfr(mesh_list, function(response_list) {
      map_dfr(response_list, function(family_list) {
        bind_rows(family_list)
      })
    })
  })
  
  
  # ------------------------------------------------------------------------------ #
  #### 3. SUMMARISE REPETITION-LEVEL PERFORMANCE ####
  # ------------------------------------------------------------------------------ #
  
  # Calculate global predictive metrics from pooled out-of-fold predictions.
  # Summarise log-likelihood and convergence across CV repetitions.
  cv_scores_summary <- cv_predictions_df %>%
    dplyr::group_by(region, mesh, response, family) %>%
    dplyr::summarise(
      n_predictions = n(),
      n_repeats = n_distinct(repeat_id),
      
      # Mean cross-validated log-likelihood across repetitions
      sum_loglik_mean = mean(
        cv_scores_df$sum_loglik[
          cv_scores_df$region == first(region) &
            cv_scores_df$mesh == first(mesh) &
            cv_scores_df$response == first(response) &
            cv_scores_df$family == first(family)
        ],
        na.rm = TRUE
      ),
      
      # Between-repetition variability in log-likelihood
      sum_loglik_sd = sd(
        cv_scores_df$sum_loglik[
          cv_scores_df$region == first(region) &
            cv_scores_df$mesh == first(mesh) &
            cv_scores_df$response == first(response) &
            cv_scores_df$family == first(family)
        ],
        na.rm = TRUE
      ),
      
      # Global errors calculated from all concatenated CV predictions
      RMSE_global = sqrt(mean((observed - predicted)^2, na.rm = TRUE)),
      MAE_global = mean(abs(observed - predicted), na.rm = TRUE),
      
      # Global errors calculated from all concatenated CV predictions
      R2_global = stats::cor(observed, predicted, use = "complete.obs")^2,
      
      # Average percentage of converged CV folds across repetitions
      mean_percent_converged = mean(
        cv_scores_df$percent_converged[
          cv_scores_df$region == first(region) &
            cv_scores_df$mesh == first(mesh) &
            cv_scores_df$response == first(response) &
            cv_scores_df$family == first(family)
        ],
        na.rm = TRUE
      ),
      
      .groups = "drop"
    )
  
  # ------------------------------------------------------------------------------ #
  #### 4. RETURN CROSS-VALIDATION OUTPUTS ####
  # ------------------------------------------------------------------------------ #
  
  # Return complete CV fits, repetition-level scores, predictions and summary
  return(list(
    cv_outputs = cv_outputs,
    cv_scores = cv_scores_df,
    cv_predictions = cv_predictions_df,
    cv_scores_summary = cv_scores_summary
  ))
  
}

# ------------------------------------------------------------------------------ #
#### 5. RUN REPEATED CROSS-VALIDATION BY REGION ####
# ------------------------------------------------------------------------------ #

repeated_randomCV_by_region <- list()

if (isTRUE(West_English_Channel)) {
  repeated_randomCV_by_region$west <- repeated_random_CV(region_name = "west")
}

if (isTRUE(East_English_Channel)) {
  repeated_randomCV_by_region$east <- repeated_random_CV(region_name = "east")
}


# ------------------------------------------------------------------------------ #
#### 6. SAVE REPEATED CROSS-VALIDATION RESULTS ####
# ------------------------------------------------------------------------------ #

saveRDS(repeated_randomCV_by_region,
        file.path(new_repertory_model,
                  paste0(sp_name_safe, "_repeated_randomCV_by_region.rds")))
