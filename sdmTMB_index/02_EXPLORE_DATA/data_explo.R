
# ============================================================================== #
# EXPLORATORY COMPARISON OF DATRAS AND CGFS SURVEY DATA
#
# Species: Black seabream (Spondyliosoma cantharus)
# Study area: Eastern (ICES 7d) and Western (ICES 7e) English Channel
#
# Objectives:
# - Compare temporal and spatial coverage between DATRAS and CGFS survey datasets (ECGFS and WCGFS)
# - Identify differences in sampling effort and survey coverage
# - Compare the number and proportion of positive hauls
# - Explore discrepancies in black seabream catch records between data sources.
# ============================================================================== #


# ------------------------------------------------------------------------------ #
#### 1. Load and subset DATRAS data #### 
# ------------------------------------------------------------------------------ #
Datras_BRB <- readr::read_csv("sdmTMB_index/01_DATA/Datras_BRB.csv", 
                              col_types = cols(StatRec = col_character(), 
                                               ICES_strat = col_character()))



# Subset data by ICES division:
# 7d: Eastern English Channel
# 7e: Western English Channel
data_east7d <- Datras_BRB %>% dplyr::filter(ICES_strat == "7d")
data_west7e <- Datras_BRB %>% dplyr::filter(ICES_strat == "7e")


# ------------------------------------------------------------------------------ #
#### 2. Explore temporal survey coverage and catch weights #### 
# ------------------------------------------------------------------------------ #

# Examine the annual number of records by survey in the Eastern English Channel
data_east7d %>%
  ggplot2::ggplot(aes(x = Year, fill = Survey))+
  ggplot2::geom_bar()

# Examine the annual number of records by survey in the Western English Channel
data_west7e %>%
  ggplot2::ggplot(aes(x = Year, fill = Survey))+
  ggplot2::geom_bar()


# Plot annual catch weights by survey in the Eastern English Channel
data_east7d %>%
  ggplot2::ggplot(aes(x = Year, y = CatchWgt, fill = Survey))+
  ggplot2::geom_col()

# Plot annual catch weights by survey in the Western English Channel
data_west7e %>% filter(Year > 2005)%>%
  ggplot2::ggplot(aes(x = Year, y = CatchWgt, fill = Survey))+
  ggplot2::geom_col()



# ------------------------------------------------------------------------------ #
####  3. Annual proportion of positive hauls by ICES division #### 
# ------------------------------------------------------------------------------ #

# EAST: Calculate the annual proportion of hauls with presence of black seabream 
data_east7d_prop_pres <- data_east7d %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(nb_haul_per_year = n_distinct(HaulNo),
                   nb_haul_with_pres = n_distinct(HaulNo[CatchWgt>0]), 
                   annual_prop_pres = nb_haul_with_pres*100/nb_haul_per_year)


# EAST : Plot annual number of hauls with bar colours representing the proportion of 
# hauls with presence
data_east7d_prop_pres %>%
  ggplot2::ggplot(aes(x = Year, y = nb_haul_per_year, fill = annual_prop_pres))+
  ggplot2::geom_col()

# WEST: Calculate the annual proportion of hauls with presence of black seabream 
data_west7e_prop_pres <- data_west7e %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(nb_haul_per_year = n_distinct(HaulNo),
                   nb_haul_with_pres = n_distinct(HaulNo[CatchWgt>0]), 
                   annual_prop_pres = nb_haul_with_pres*100/nb_haul_per_year)

# WEST : Plot annual number of hauls with bar colours representing the proportion of 
# hauls with presence
data_west7e_prop_pres %>%
  gggplot2::gplot(aes(x = Year, y = nb_haul_per_year, fill = annual_prop_pres))+
  ggplot2::geom_col()

# WEST : Plot annual number of hauls with bar colours representing the proportion of 
# hauls with presence after 2005
data_west7e_prop_pres %>% 
  dplyr::filter(Year > 2005)%>%
  ggplot2::ggplot(aes(x = Year, y = nb_haul_per_year, fill = annual_prop_pres))+
  ggplot2::geom_col()



# ------------------------------------------------------------------------------ #
####  4. Annual proportion of positive hauls by survey #### 
# ------------------------------------------------------------------------------ #

# EAST: Calculate annual number of hauls and occurrence separately for each survey 
data_east7d_prop_pres <- data_east7d %>%
  dplyr::group_by(Year, Survey) %>%
  dplyr::summarise(nb_haul_per_year = n_distinct(HaulNo),
                   nb_haul_with_pres = n_distinct(HaulNo[CatchWgt>0]), 
                   annual_prop_pres = nb_haul_with_pres*100/nb_haul_per_year)

# Plot annual sampling effort for each survey
p1_east <- data_east7d_prop_pres %>%
  ggplot2::ggplot(aes(x = Year, y = nb_haul_per_year))+
  ggplot2::geom_col()+
  ggplot2::facet_wrap(~Survey, scales = "free_y")

# Plot the annual number of positive hauls for each survey
p2_east <- data_east7d_prop_pres %>%
  ggplot2::ggplot(aes(x = Year, y = nb_haul_with_pres))+
  ggplot2::geom_col()+
  ggplot2::facet_wrap(~Survey, scales = "free_y")

# Plot the annual proportion of positive hauls for each survey
p3_east <- data_east7d_prop_pres %>%
  ggplot2::ggplot(aes(x = Year, y = annual_prop_pres))+
  ggplot2::geom_col()+
  ggplot2::facet_wrap(~Survey, scales = "free_y")

# Combine the three plots vertically to compare sampling effort,
# number of positive hauls and occurrence rates.
p1_east/p2_east/p3_east


# WEST : Calculate annual number of hauls and occurrence separately for each survey 
data_west7e_prop_pres <- data_west7e %>%
  dplyr::filter(Year > 2005)%>%
  dplyr::group_by(Year, Survey) %>%
  dplyr::summarise(nb_haul_per_year = n_distinct(HaulNo),
                   nb_haul_with_pres = n_distinct(HaulNo[CatchWgt>0]), 
                   annual_prop_pres = nb_haul_with_pres*100/nb_haul_per_year)

# Plot annual sampling effort for each survey
p1_west <- data_west7e_prop_pres %>%
  ggplot2::ggplot(aes(x = Year, y = nb_haul_per_year))+
  ggplot2::geom_col()+
  ggplot2::facet_wrap(~Survey, scales = "free_y")

# Plot the annual number of positive hauls for each survey
p2_west <- data_west7e_prop_pres %>%
  ggplot2::ggplot(aes(x = Year, y = nb_haul_with_pres))+
  ggplot2::geom_col()+
  ggplot2::facet_wrap(~Survey, scales = "free_y")

# Plot the annual proportion of positive hauls for each survey
p3_west <- data_west7e_prop_pres %>%
  ggplot2::ggplot(aes(x = Year, y = annual_prop_pres))+
  ggplot2::geom_col()+
  ggplot2::facet_wrap(~Survey, scales = "free_y")

# Combine the three plots vertically to compare sampling effort,
# number of positive hauls and occurrence rates.
p1_west/p2_west/p3_west



# ------------------------------------------------------------------------------ #
####  5. Import and process WCGFS & ECGFS surveys data #### 
# ------------------------------------------------------------------------------ #

# WEST
# Import catch data from the Western Channel Ground Fish Survey
WCGFS_2018_2024 <- readr::read_delim("sdmTMB_index/01_DATA/survey/WCGFS/WCGFS_2018_2024_ELFIC.V1.5_catch_2025-03-31.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Import corresponding survey operation data, including spatial and sampling information
WGFS_operation <- readr::read_delim("sdmTMB_index/01_DATA/survey/WCGFS/WCGFS_2018_2024_ELFIC.V1.5_operation_2025-03-31.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Retain black seabream catch records before 2024
black_seabream_WCGFS_2018_2024 <- WCGFS_2018_2024 %>% 
  filter(scientificName  == "Spondyliosoma cantharus", year < 2024)

# Keep operation data before 2024
WGFS_operation <- WGFS_operation %>% 
  dplyr::filter(year < 2024)

# Join species catch records with corresponding survey operations
# using the shared identifiers
black_seabream_WGFS_catch_operation <- black_seabream_WCGFS_2018_2024 %>%
  dplyr::left_join(WGFS_operation,
                   by = c("serie", "year", "partialSerie", "stationCode",
                          "operationID", "haulID"))

# EAST
# Import catch data from the Eastern Channel Ground Fish Survey
ECGFS_2018_2024 <- readr::read_delim("sdmTMB_index/01_DATA/survey/ECGFS/CGFS_1988_2024_ELFIC.V1.4_catch_2025-03-28.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Import corresponding survey operation data, including spatial and sampling information
EGFS_operation <- readr::read_delim("sdmTMB_index/01_DATA/survey/ECGFS/CGFS_1988_2024_ELFIC.V1.4_operation_2025-03-28.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Retain black seabream catch records before 2024
black_seabream_ECGFS_2018_2024 <- ECGFS_2018_2024 %>% 
  dplyr::filter(scientificName  == "Spondyliosoma cantharus", year < 2024)

# Keep operation data before 2024
EGFS_operation <- EGFS_operation %>% 
  dplyr::filter(year < 2024)

# Join species catch records with corresponding survey operations
# using the shared identifiers
black_seabream_EGFS_catch_operation <- black_seabream_ECGFS_2018_2024 %>%
  dplyr::left_join(EGFS_operation,
                   by = c("serie", "year", "partialSerie", "stationCode",
                          "operationID", "haulID"))

# ------------------------------------------------------------------------------ #
#### 6. Explore survey catches in the Western English Channel ####
# ------------------------------------------------------------------------------ #

# Restrict WCGFS records to statistical rectangles represented in the DATRAS 
# dataset for ICES division 7e
black_seabream_WGFS_clear_stratum_west <- black_seabream_WGFS_catch_operation %>%
  dplyr::filter(stratum %in% c(unique(data_west7e$StatRec)))

# Apply the same spatial selection to ECGFS data, retaining observations after 2005
black_seabream_EGFS_clear_stratum_west <- black_seabream_EGFS_catch_operation %>%
  dplyr::filter(stratum %in% c(unique(data_west7e$StatRec)), year > 2005)

# Plot the annual number of black seabream catch records in WCGFS,
# distinguishing survey series
p_pres_WCGFS_west <- black_seabream_WGFS_clear_stratum_west %>%
  ggplot2::ggplot(aes(x = year))+
  ggplot2::geom_bar()+
  ggplot2::facet_wrap(~serie)

# Plot annual black seabream catch weights in WCGFS
p_weightkg_WCGFS_west <- black_seabream_WGFS_clear_stratum_west %>%
  ggplot2::ggplot(aes(x = year, y =totalWeightKg))+
  ggplot2::geom_col()

# Plot the annual number of black seabream catch records in ECGFS within the 
# Western English Channel (7e division)
p_pres_ECGFS_west <- black_seabream_EGFS_clear_stratum_west %>%
  ggplot2::ggplot(aes(x = year))+
  ggplot2::geom_bar()+
  ggplot2::facet_wrap(~serie)

# Plot annual black seabream catch weights in ECGFS
p_weightkg_ECGFS_west <- black_seabream_EGFS_clear_stratum_west %>%
  ggplot2::ggplot(aes(x = year, y =totalWeightKg))+
  ggplot2::geom_col()

# Compare the annual number of positive hauls from DATRAS with the temporal 
# distribution of species catch records extracted from ECGFS and WCGFS.
p2_west / (patchwork::plot_spacer() + p_pres_ECGFS_west  + p_pres_WCGFS_west)


# ------------------------------------------------------------------------------ #
#### 7. Explore survey catches in the Eastern English Channel ####
# ------------------------------------------------------------------------------ #

# Restrict ECGFS data to statistical rectangles represented in the DATRAS 
# dataset for ICES division 7d
black_seabream_EGFS_clear_stratum_east <- black_seabream_EGFS_catch_operation %>%
  dplyr::filter(stratum %in% c(unique(data_east7d$StatRec)))

# Apply the same spatial selection to WCGFS data
black_seabream_WGFS_clear_stratum_east <- black_seabream_WGFS_catch_operation %>%
  dplyr::filter(stratum %in% c(unique(data_east7d$StatRec)))

# Plot the annual number of black seabream catch records in ECGFS within the 
# Eastern English Channel (7d)
p_pres_ECGFS_east <- black_seabream_EGFS_clear_stratum_east %>%
  ggplot2::ggplot(aes(x = year))+
  ggplot2::geom_bar()+
  ggplot2::facet_wrap(~serie)

# Plot annual black seabream catch weights in ECGFS
p_weightkg_ECGFS_east <- black_seabream_EGFS_clear_stratum_east %>%
  ggplot2::ggplot(aes(x = year, y =totalWeightKg))+
  ggplot2::geom_col()

# Plot the annual number of black seabream catch records in WCGFS within the 
# Eastern English Channel (7d)
p_pres_WCGFS_east <- black_seabream_WGFS_clear_stratum_east %>%
  ggplot2::ggplot(aes(x = year))+
  ggplot2::geom_bar()+
  ggplot2::facet_wrap(~serie)

# Plot annual black seabream catch weights in WCGFS
p_weightkg_WCGFS_east <- black_seabream_WGFS_clear_stratum_east %>%
  ggplot2::ggplot(aes(x = year, y =totalWeightKg))+
  ggplot2::geom_col()

# Compare the annual number of positive hauls from DATRAS with the temporal 
# distribution of black seabream catch records extracted from ECGFS and WCGFS 
# in the Eastern English Channel (7d division)
p2_east / (patchwork::plot_spacer() +p_pres_ECGFS_east + p_pres_WCGFS_east)
