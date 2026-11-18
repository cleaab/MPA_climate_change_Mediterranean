# Plot - CATCH - all species
library(here)
library(ncdf4)
library(sf)
# Load csfd# Load countries shapefile
worldbounds = st_read(here("data-mpa-CC-paper/shapefiles/world_map/world-bounds/world-bounds-a.shp"))
worldbounds_3035 = st_transform(worldbounds, 3035)

nc = nc_open(here("data-mpa-CC-paper/model_outputs/spatial_nompa/Yield_nompa_scenario_CC.nc"))
yield_nompa_future <- ncvar_get(nc, varid = "Catches")
yield_nompa_future <- apply(yield_nompa_future, c(1,2,3), mean, na.rm = T) # mean over reps

nc = nc_open(here("data-mpa-CC-paper/model_outputs/spatial_nompa/Yield_nompa_scenario_noCC.nc"))
yield_nompa_present <- ncvar_get(nc, varid = "Catches")
yield_nompa_present <- apply(yield_nompa_present, c(1,2,3), mean, na.rm = T) # mean over reps

# Sum over species  ------
position_categories = c("Pelagic", "Demersal", "Benthic")

for (v in position_categories){
  
  # Highly commercial
  species_classif = read.csv(here("data-mpa-CC-paper/classification_species_commercial_class.csv")) %>%
    filter(! species_id %in% c(0,1,2)) %>%
    filter(commercial_class %in% c("HC", "C")) %>%
    filter(vertical_position %in% v)
  
  
  cat_name_HC = paste0("HC_C_", v)
  species_id_list = species_classif$species_id
  species_name_list = species_classif$species_name
  
  length(species_id_list)
  print(species_name_list)
  tot_yield_nompa_future = yield_nompa_future[1:190, 1:83, species_id_list+1]
  tot_yield_nompa_future = apply(tot_yield_nompa_future, c(1,2), sum)
  
  tot_yield_nompa_present = yield_nompa_present[1:190, 1:83, species_id_list+1]
  tot_yield_nompa_present = apply(tot_yield_nompa_present, c(1,2), sum)
  
  tot_yield_nompa_future2 = tot_yield_nompa_future[,83:1]
  tot_yield_nompa_future2_vect = as.vector(tot_yield_nompa_future2)
  
  tot_yield_nompa_present2 = tot_yield_nompa_present[,83:1]
  tot_yield_nompa_present2_vect = as.vector(tot_yield_nompa_present2)
  
  Spatial_yield_delta_species_cat = Med_grid_GSA_EEZ_df %>%
    mutate(cell_ID = as.numeric(cell_ID)) %>%
    arrange(cell_ID) %>%
    mutate(Y_nompa_future = tot_yield_nompa_future2_vect,
           Y_nompa_present = tot_yield_nompa_present2_vect) %>%
    mutate(delta_Y = (Y_nompa_future - Y_nompa_present)/Y_nompa_present * 100) %>%
    mutate(delta_Y = case_when(VALUE == 0 & Y_nompa_present == 0 & Y_nompa_future == 0 ~ 0,
                               TRUE ~ delta_Y))
  
  #### PLOT ----------------------------------------------------------------------

  all_vals <- c(Spatial_yield_delta_species_cat$delta_Y)
  
  common_min <- min(all_vals, na.rm = TRUE)
  common_max <- max(all_vals, na.rm = TRUE)
  
  # Plot 1: MPA effect catch change
  map_effect_yield_cats_HC <- ggplot(Spatial_yield_delta_species_cat) +
    geom_sf(aes(fill = delta_Y), color = NA) +  # remove border lines for a cleaner look
    geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +
    
    scale_fill_gradient2(
      low = "darkred",
      mid = "lightyellow",
      high = "deepskyblue4",
      midpoint = 0,
      limits = c(-30, 30),
      oob = scales::squish,
      name = expression(Delta~Y~"(%)"),
      breaks = c(-30, -15, 0, 15, 30),
      labels = c("≤ -30%", -15, 0, 15, "≥ 30%")) +
    
    coord_sf(expand = FALSE) +  # remove whitespace around the map
    labs(
      title = "",
      fill = expression(Delta~Y~"(%)")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      legend.position = "right",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  
  ggsave(
    here(paste0("figures/after_revision1/FINAL/Delta_Y_future_present_30_limit_", cat_name_HC, ".png")),
    plot = map_effect_yield_cats_HC,
    dpi = 300,
    width = 10,
    height = 6
  )
  
  # Minor commercial
  species_classif = read.csv(here("data-mpa-CC-paper/classification_species_commercial_class.csv")) %>%
    filter(! species_id %in% c(0,1,2)) %>%
    filter(commercial_class %in% c("LC", "NC")) %>%
    filter(vertical_position %in% v)
  
  
  cat_name_LC = paste0("LC_NC_", v)
  species_id_list = species_classif$species_id
  species_name_list = species_classif$species_name
  
  length(species_id_list)
  print(species_name_list)
  tot_yield_nompa_future = yield_nompa_future[1:190, 1:83, species_id_list+1]
  tot_yield_nompa_future = apply(tot_yield_nompa_future, c(1,2), sum)
  
  tot_yield_nompa_present = yield_nompa_present[1:190, 1:83, species_id_list+1]
  tot_yield_nompa_present = apply(tot_yield_nompa_present, c(1,2), sum)
  
  tot_yield_nompa_future2 = tot_yield_nompa_future[,83:1]
  tot_yield_nompa_future2_vect = as.vector(tot_yield_nompa_future2)
  
  tot_yield_nompa_present2 = tot_yield_nompa_present[,83:1]
  tot_yield_nompa_present2_vect = as.vector(tot_yield_nompa_present2)
  
  Spatial_yield_delta_species_cat = Med_grid_GSA_EEZ_df %>%
    mutate(cell_ID = as.numeric(cell_ID)) %>%
    arrange(cell_ID) %>%
    mutate(Y_nompa_future = tot_yield_nompa_future2_vect,
           Y_nompa_present = tot_yield_nompa_present2_vect) %>%
    mutate(delta_Y = (Y_nompa_future - Y_nompa_present)/Y_nompa_present * 100) %>%
    mutate(delta_Y = case_when(VALUE == 0 & Y_nompa_present == 0 & Y_nompa_future == 0 ~ 0,
                               TRUE ~ delta_Y))
  
  #### PLOT ----------------------------------------------------------------------
  
  all_vals <- c(Spatial_yield_delta_species_cat$delta_Y)
  
  common_min <- min(all_vals, na.rm = TRUE)
  common_max <- max(all_vals, na.rm = TRUE)
  
  # Plot 1: MPA effect catch change
  map_effect_yield_cats_LC <- ggplot(Spatial_yield_delta_species_cat) +
    geom_sf(aes(fill = delta_Y), color = NA) +  # remove border lines for a cleaner look
    geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +
    
    scale_fill_gradient2(
      low = "darkred",
      mid = "lightyellow",
      high = "deepskyblue4",
      midpoint = 0,
      limits = c(-30, 30),
      oob = scales::squish,
      name = expression(Delta~Y~"(%)"),
      breaks = c(-30, -15, 0, 15, 30),
      labels = c("≤ -30%", -15, 0, 15, "≥ 30%")) +
    
    coord_sf(expand = FALSE) +  # remove whitespace around the map
    labs(
      title = "",
      fill = expression(Delta~Y~"(%)")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      legend.position = "right",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  
  
  ggsave(
    here(paste0("figures/after_revision1/FINAL/Delta_Y_future_present_30_limit_", cat_name_LC, ".png")),
    plot = map_effect_yield_cats_LC,
    dpi = 300,
    width = 10,
    height = 6
  )
  
}



#