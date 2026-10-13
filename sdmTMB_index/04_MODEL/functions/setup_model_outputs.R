# ============================================================================== #
# SAVE SDMTMB MODELLING OUTPUTS
#
# Objectives:
# - Create a species-specific directory for modelling outputs.
# - Generate a timestamped folder to distinguish modelling runs.
# - Save spatial meshes, model diagnostics and converged models.
# - Store parameter estimates, model selection and cross-validation results.
# - Export spatial predictions, uncertainty estimates and associated maps.
# ============================================================================== #

# ------------------------------------------------------------------------------ #
#### 1. DEFINE SPECIES-SPECIFIC OUTPUT DIRECTORY ####
# ------------------------------------------------------------------------------ #

# Convert the scientific name into a filesystem-compatible identifier
sp_name_safe <- gsub("[^A-Za-z0-9_]", "_", sp_scientific)

# Define the species-specific output directory
species_output_dir <- here::here("05_OUTPUTS", sp_name_safe)

# Create the species directory if it does not already exist
if (!dir.exists(species_output_dir)) {
  dir.create(species_output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------ #
#### 2. CREATE A TIMESTAMPED MODELLING DIRECTORY ####
# ------------------------------------------------------------------------------ #

# Generate a timestamp to identify the current modelling run
run_timestamp <- format(Sys.time(), "%d-%b-%Y-%H.%M")

# Define the directory for the current modelling run
new_repertory_model <- file.path(species_output_dir,
                                 paste0(sp_name_safe, "_", run_timestamp))

# Create the run-specific output directory
dir.create(new_repertory_model, showWarnings = FALSE)

