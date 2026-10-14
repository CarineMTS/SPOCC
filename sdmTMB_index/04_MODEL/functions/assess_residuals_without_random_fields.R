# ============================================================================== #
# COMPUTE AND MAP RESIDUALS FROM MODELS WITHOUT SPATIAL RANDOM FIELDS
#
# Objectives:
# - Fit an intercept-only Tweedie model without spatial random fields.
# - Exclude spatiotemporal random effects to establish a non-spatial baseline.
# - Extract model residuals for each survey observation.
# - Visualise the spatial distribution of residuals by survey year.
# - Save residual diagnostic plots for each selected region.
# ============================================================================== #

# ------------------------------------------------------------------------------ #
#### 1. DEFINE NON-SPATIAL MODEL RESIDUAL FUNCTION ####
# ------------------------------------------------------------------------------ #

# Fit a non-spatial baseline model and map its residuals for a given region.
residuals_withoutRF <- function(region_name) {
  
  # Select survey data for the specified region
  if (region_name == "west"){
    data_CGFS = data_CGFS_west  }
  else if (region_name == "east"){
    data_CGFS = data_CGFS_east
  } else {
    stop("region_name must be 'west' or 'east'")
  }
  
  
  # ------------------------------------------------------------------------------ #
  #### 1.1. FIT AN INTERCEPT-ONLY MODEL WITHOUT RANDOM FIELDS ####
  # ------------------------------------------------------------------------------ #
  
  # Fit a Tweedie model without spatial or spatiotemporal random fields.
  # The model assumes a constant expected biomass density across space and time.
  model_withoutRF <- sdmTMB::sdmTMB(data = data_CGFS,
                                    formula = densityKgKm2 ~ 1,
                                    family = tweedie(link = "log"), 
                                    spatial = "off", 
                                    time = NULL, 
                                    spatiotemporal = "off")
  
  # ------------------------------------------------------------------------------ #
  #### 1.2.. EXTRACT AND PREPARE MODEL RESIDUALS ####
  # ------------------------------------------------------------------------------ #
  
  # Extract residuals using the specified sdmTMB residual method
  resids_withoutRF <- residuals(model_withoutRF, type = "mle-mvn")
  
  # Convert residuals into a tibble
  resids_withoutRF <- tibble::tibble(residuals = resids_withoutRF)
  
  # Combine survey observations with their corresponding residuals
  resids_for_plot <- dplyr::bind_cols(data_CGFS, resids_withoutRF)
  
  
  # ------------------------------------------------------------------------------ #
  #### 1.3.. MAP ANNUAL SPATIAL DISTRIBUTIONS OF RESIDUALS ####
  # ------------------------------------------------------------------------------ #
  
  # Display residual values at survey stations for each sampling year
  plot_residuals <- ggplot2::ggplot(resids_for_plot, aes(lon, lat, fill = residuals)) +
    ggplot2::geom_point(color = "grey", size = 2.5, shape = 21) +
    ggplot2::scale_fill_gradient2(low = "red", high = "blue") +
    ggplot2::facet_wrap(~year) +
    ggplot2::labs(x = "", y = "", fill = "Quantiles\nrésiduals",
                  title = bquote(italic(.(sp_scientific)) ~ " - " ~ .(region_name))) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), 
                   strip.background = ggplot2::element_rect(fill = "grey90"),
                   strip.text = ggplot2::element_text(size = 8, margin = margin(t=2, b=2)))
  
  
  # Return the annual residual diagnostic plot
  return(plot_residuals)
  
}


# ------------------------------------------------------------------------------ #
#### 2. COMPUTE RESIDUAL DIAGNOSTICS BY REGION ####
# ------------------------------------------------------------------------------ #

# Initialise the list of regional residual diagnostic plots
plot_resids_withoutRF <- list()

# Fit the baseline model and map residuals for the Western English Channel
plot_resids_withoutRF <- list()
if (isTRUE(West_English_Channel)) {
  plot_resids_withoutRF$west <- residuals_withoutRF(region_name = "west")
}

# Fit the baseline model and map residuals for the Eastern English Channel
if (isTRUE(East_English_Channel)) {
  plot_resids_withoutRF$east <- residuals_withoutRF(region_name = "east")
}


# ------------------------------------------------------------------------------ #
#### 3. SAVE REGIONAL RESIDUAL DIAGNOSTIC PLOTS ####
# ------------------------------------------------------------------------------ #

# Save annual residual maps from non-spatial baseline models
saveRDS(plot_resids_withoutRF,
        file.path(new_repertory_model, 
                  paste0(sp_name_safe, "_residuals_without_random_fields.rds")))
