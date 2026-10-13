
# ============================================================================== #
# ASSESS SPATIAL AUTOCORRELATION USING MORAN'S I
#
# Objectives:
# - Calculate annual Moran's I for observed biomass density.
# - Test for spatial autocorrelation using inverse-distance weights.
# - Compare observed and expected Moran's I values.
# - Visualise annual spatial autocorrelation and its significance.
# - Store diagnostic plots by region for subsequent analyses.
# ============================================================================== #


# ------------------------------------------------------------------------------ #
#### 1. DEFINE ANNUAL MORAN'S I COMPUTATION FUNCTION ####
# ------------------------------------------------------------------------------ #

# Compute annual Moran's I and plot observed values against expected values 
# under the null hypothesis of spatial randomness
compute_plot_Moran <- function(region_name) {
  
  # Select survey data for the specified region
  if (region_name == "west"){
    data_CGFS = data_CGFS_west  }
  else if (region_name == "east"){
    data_CGFS = data_CGFS_east
  } else {
    stop("region_name must be 'west' or 'east'")
  }
  
  # ---------------------------------------------------------------------------- #
  #### 1.1. COMPUTE ANNUAL SPATIAL AUTOCORRELATION ####
  # ---------------------------------------------------------------------------- #
  
  # Calculate Moran's I separately for each year
  resultats_moran <- data_CGFS %>%
    dplyr::group_by(year) %>%
    dplyr::filter(!all(densityKgKm2 == 0)) |>
    dplyr::group_modify(~{
      # Compute pairwise distances and inverse-distance spatial weights
      dists <- as.matrix(dist(.x[, c("lat", "lon")]))
      inv_dists <- 1 / dists
      diag(inv_dists) <- 0
      # Calculate Moran's I and associated statistics
      moran <- ape::Moran.I(.x$densityKgKm2, inv_dists, scaled = TRUE)
      data.frame(observed = moran$observed,
                 expected = moran$expected,
                 sd = moran$sd,
                 p_value = moran$p.value)
    }) %>%
    dplyr::ungroup() %>%
    # Define expected-value intervals
    dplyr::mutate(ymin = expected - sd,
           ymax = expected + sd,
           ymin95 = expected - 1.96 * sd,
           ymax95 = expected + 1.96 * sd,
           # Assign significance levels
           signif = dplyr::case_when(p_value < 0.001 ~ "***",
                                     p_value < 0.01 ~ "**", 
                                     p_value < 0.05 ~ "*",
                                     TRUE ~ "ns"))

  
  # ---------------------------------------------------------------------------- #
  #### 1.2. VISUALISE ANNUAL MORAN'S I STATISTICS ####
  # ---------------------------------------------------------------------------- #
  
  # Plot annual observed and expected Moran's I values
  ggplot2::ggplot(resultats_moran, aes(x = year)) +
    ggplot2::geom_errorbar(aes(ymin = ymin95, ymax = ymax95), 
                           width = 0.2, color = "darkred") +
    ggplot2::geom_point(aes(y = expected), 
                        shape = 1, size = 3, color = "darkred") +
    ggplot2::geom_point(aes(y = observed), 
                        size = 3, color = "darkblue") +
    ggplot2::geom_text(aes(y = max(ymax95) + 0.05, label = signif), 
                       nudge_y = 0.02, size = 5) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = bquote(italic(.(sp_scientific)) ~ " - " ~ .(region_name)),
                  subtitle = "*** p<0.001, ** p<0.01, *p<0.05, \nwhite point : expected value without SA",
                  y = "Moran's index")
  
}



# ------------------------------------------------------------------------------ #
#### 2. COMPUTE MORAN'S I DIAGNOSTICS BY REGION ####
# ------------------------------------------------------------------------------ #

# Initialise the list of regional Moran's I plots
moran_by_region <- list()

# Compute Moran's I for the Western English Channel
if (isTRUE(West_English_Channel)) {
  moran_by_region$west <- compute_plot_Moran(region_name = "west")
}

# Compute Moran's I for the Eastern English Channel
if (isTRUE(East_English_Channel)) {
  moran_by_region$west <- compute_plot_Moran(region_name = "east")
}


# ------------------------------------------------------------------------------ #
#### 3. SAVE REGIONAL SPATIAL AUTOCORRELATION DIAGNOSTICS ####
# ------------------------------------------------------------------------------ #

saveRDS(moran_by_region,
        file.path(new_repertory_model, 
                  paste0(sp_name_safe, "_moran_by_region.rds")))

