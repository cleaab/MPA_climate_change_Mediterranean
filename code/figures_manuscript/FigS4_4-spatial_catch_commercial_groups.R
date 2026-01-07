# Plot - CATCH - all species
library(here)
library(ncdf4)
library(sf)
# Load csfd# Load countries shapefile
worldbounds = st_read(here("data/shapefiles/world_map/world-bounds/world-bounds-a.shp"))
worldbounds_3035 = st_transform(worldbounds, 3035)

nc = nc_open(here("data/model_outputs/spatial_nompa/Yield_nompa_scenario_CC.nc"))
yield_nompa_future <- ncvar_get(nc, varid = "Catches")
yield_nompa_future <- apply(yield_nompa_future, c(1,2,3), mean, na.rm = T) # mean over reps

nc = nc_open(here("data/model_outputs/spatial_nompa/Yield_nompa_scenario_noCC.nc"))
yield_nompa_present <- ncvar_get(nc, varid = "Catches")
yield_nompa_present <- apply(yield_nompa_present, c(1,2,3), mean, na.rm = T) # mean over reps

# Sum over species  ------

species_classif = read.csv(here("data/classification_species.csv")) %>%
  filter(! species_id %in% c(0,1,2)) %>%
  filter(updated_commercial_interest %in% c("LC", "NC")) %>%
  filter(vertical_position == "Pelagic")
  

sp_name = "LC_NC_pelagic_updated"
species_id_list = species_classif$species_id
species_name_list = species_classif$species_name

length(species_id_list)
print(species_name_list)
tot_yield_nompa_future = yield_nompa_future[1:190, 1:83, species_id_list+1]
tot_yield_nompa_future = apply(tot_yield_nompa_future, c(1,2), sum)

tot_yield_nompa_present = yield_nompa_present[1:190, 1:83, species_id_list+1]
tot_yield_nompa_present = apply(tot_yield_nompa_present, c(1,2), sum)

for (s in 1:10){
  nc = nc_open(here(paste0("data/model_outputs/spatial_mpa_10/EEZ-conservation-rep", s, "/Yield_mpa_scenario.nc")))
  yield_mpa_future <- ncvar_get(nc, varid = "Catches")
  yield_mpa_future <- apply(yield_mpa_future, c(1,2,3), mean, na.rm = T) # mean over reps
  tot_yield_mpa_future = yield_mpa_future[1:190, 1:83, species_id_list+1]
  tot_yield_mpa_future = apply(tot_yield_mpa_future, c(1,2), sum)
  
  tot_yield_mpa_future2 = tot_yield_mpa_future[,83:1]
  tot_yield_mpa_future2_vect = as.vector(tot_yield_mpa_future2)
  
  if (s == 1){
    Y_diff_all_scens = tot_yield_mpa_future2_vect
    
  } else{
    Y_diff_all_scens = cbind(Y_diff_all_scens, tot_yield_mpa_future2_vect)
  }
}


nrep = seq(1, 10, 1)
colnames(Y_diff_all_scens) = paste0("rep", nrep)


Spatial_yield_diff_EEZ_scenarios = Med_grid_GSA_EEZ_df %>%
  mutate(cell_ID = as.numeric(cell_ID)) %>%
  arrange(cell_ID) 

Spatial_yield_diff = cbind(Spatial_yield_diff_EEZ_scenarios, Y_diff_all_scens)

st_write(Spatial_yield_diff, here(paste0("data/model_outputs/yield-groups/spatial_yield_EEZ_scenarios_mpa10_", sp_name, ".gpkg")))


#### Open previously saved files -----------------------------------------------

sp_names = c("HC_C_benthic_updated", "HC_C_pelagic_updated", "HC_C_demersal_updated", "LC_NC_benthic_updated", "LC_NC_demersal_updated", "LC_NC_pelagic_updated")

for (sp_name in sp_names){
  Spatial_yield = st_read(here(paste0("data/model_outputs/yield-groups/spatial_yield_EEZ_scenarios_mpa10_", sp_name, ".gpkg")))
  
  # Convert rep1 to rep10 to numeric (handle potential factors/characters)
  Spatial_yield[paste0("rep", 1:10)] <- lapply(Spatial_yield[paste0("rep", 1:10)], function(x) as.numeric(as.character(x)))
  
  # Compute row-wise mean while ignoring geometry
  Spatial_yield$rep_mean <- rowMeans(st_drop_geometry(Spatial_yield)[, paste0("rep", 1:10)], na.rm = TRUE)
  
  tot_yield_nompa_future2 = tot_yield_nompa_future[,83:1]
  tot_yield_nompa_future2_vect = as.vector(tot_yield_nompa_future2)
  
  Spatial_yield$Y_nompa_future = tot_yield_nompa_future2_vect
  
  tot_yield_nompa_present2 = tot_yield_nompa_present[,83:1]
  tot_yield_nompa_present2_vect = as.vector(tot_yield_nompa_present2)
  
  Spatial_yield$Y_nompa_present = tot_yield_nompa_present2_vect
  
  ### -----
  # Plot difference by GSA
  Spatial_yield_by_GSA = Spatial_yield %>%
    # mutate(Y_nompa_present = case_when(Y_nompa_present == 0 ~ 10^-3,
    #                                    TRUE ~ Y_nompa_present)) %>%
    
    mutate(delta_rep1 = (rep1 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep2 = (rep2 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep3 = (rep3 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep4 = (rep4 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep5 = (rep5 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep6 = (rep6 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep7 = (rep7 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep8 = (rep8 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep9 = (rep9 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep10 = (rep10 - Y_nompa_present)/Y_nompa_present * 100) %>%
    
    mutate(diff_Y = Y_nompa_future - Y_nompa_present,
           delta_Y = (Y_nompa_future - Y_nompa_present)/Y_nompa_present * 100,
           diff_Y_mpa = rep_mean - Y_nompa_present,
           delta_Y_mpa = (rep_mean - Y_nompa_present)/Y_nompa_present * 100) %>%
    
    mutate(diff_delta = delta_Y_mpa - delta_Y)
  
  # Compute row-wise mean while ignoring geometry
  Spatial_yield_by_GSA$mean_delta_rep <- rowMeans(st_drop_geometry(Spatial_yield_by_GSA)[, paste0("delta_rep", 1:10)], na.rm = TRUE)
  
  # Compute standard deviation accross scenarios
  Spatial_yield_by_GSA$sd_delta_rep <- apply(
    st_drop_geometry(Spatial_yield_by_GSA)[, paste0("delta_rep", 1:10)],
    1,
    sd,
    na.rm = TRUE
  )
  
  #### RELATIVE CHANGE - calculate average Y accross scenarios by GSA and then calculate delta Y on the average
  
  common_min <- min(Spatial_yield_by_GSA$delta_Y, na.rm = TRUE)
  common_max <- max(Spatial_yield_by_GSA$delta_Y, na.rm = TRUE)
  
  # Plot 1: MPA effect catch change
  map_effect_biomass <- ggplot(Spatial_yield_by_GSA) +
    geom_sf(aes(fill = delta_Y), color = NA) +  # remove border lines for a cleaner look
    geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +
    
    scale_fill_gradient2(
      low = "deepskyblue4",
      mid = "lightyellow",
      high = "darkred",
      midpoint = 0,
      limits = c(-50, 50),
      oob = scales::squish,
      name = expression(Delta~Y~"(%)"),
      breaks = c(-50, -25, 0, 25, 50),
      labels = c("≤ -50%", -25, 0, 25, "≥ 50%")) +
    coord_sf(expand = FALSE) +  # remove whitespace around the map
    
    labs(
      title = paste0(sp_name),
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
  
  # Save the plot
  ggsave(
    here(paste0("figures/spatial_catch/FigS5/Delta_Y_without_MPAs_", sp_name, ".png")),
    plot = map_effect_biomass,
    dpi = 300,
    width = 10,
    height = 6
  )
  
}


# Average by GSA ---------------------------------------------------------------
sp_names = c("HC_C_benthic_updated", "HC_C_pelagic_updated", "HC_C_demersal_updated", "LC_NC_benthic_updated", "LC_NC_demersal_updated", "LC_NC_pelagic_updated")

for (sp_name in sp_names){
  Spatial_yield = st_read(here(paste0("data/model_outputs/yield-groups/spatial_yield_EEZ_scenarios_mpa10_", sp_name, ".gpkg")))
  
  # Convert rep1 to rep10 to numeric (handle potential factors/characters)
  Spatial_yield[paste0("rep", 1:10)] <- lapply(Spatial_yield[paste0("rep", 1:10)], function(x) as.numeric(as.character(x)))
  
  # Compute row-wise mean while ignoring geometry
  Spatial_yield$rep_mean <- rowMeans(st_drop_geometry(Spatial_yield)[, paste0("rep", 1:10)], na.rm = TRUE)
  
  tot_yield_nompa_future2 = tot_yield_nompa_future[,83:1]
  tot_yield_nompa_future2_vect = as.vector(tot_yield_nompa_future2)
  
  Spatial_yield$Y_nompa_future = tot_yield_nompa_future2_vect
  
  tot_yield_nompa_present2 = tot_yield_nompa_present[,83:1]
  tot_yield_nompa_present2_vect = as.vector(tot_yield_nompa_present2)
  
  Spatial_yield$Y_nompa_present = tot_yield_nompa_present2_vect
  
  ### -----
  # Plot difference by GSA
  Spatial_yield_by_GSA = Spatial_yield %>%
    # mutate(Y_nompa_present = case_when(Y_nompa_present == 0 ~ 10^-3,
    #                                    TRUE ~ Y_nompa_present)) %>%
    
    mutate(delta_rep1 = (rep1 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep2 = (rep2 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep3 = (rep3 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep4 = (rep4 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep5 = (rep5 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep6 = (rep6 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep7 = (rep7 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep8 = (rep8 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep9 = (rep9 - Y_nompa_present)/Y_nompa_present * 100,
           delta_rep10 = (rep10 - Y_nompa_present)/Y_nompa_present * 100) %>%
    
    mutate(diff_Y = Y_nompa_future - Y_nompa_present,
           delta_Y = (Y_nompa_future - Y_nompa_present)/Y_nompa_present * 100,
           diff_Y_mpa = rep_mean - Y_nompa_present,
           delta_Y_mpa = (rep_mean - Y_nompa_present)/Y_nompa_present * 100) %>%
    
    mutate(diff_delta = delta_Y_mpa - delta_Y)
  
  # Compute row-wise mean while ignoring geometry
  Spatial_yield_by_GSA$mean_delta_rep <- rowMeans(st_drop_geometry(Spatial_yield_by_GSA)[, paste0("delta_rep", 1:10)], na.rm = TRUE)
  
  # Compute standard deviation accross scenarios
  Spatial_yield_by_GSA$sd_delta_rep <- apply(
    st_drop_geometry(Spatial_yield_by_GSA)[, paste0("delta_rep", 1:10)],
    1,
    sd,
    na.rm = TRUE
  )
  
  #### RELATIVE CHANGE - calculate average Y accross scenarios by GSA and then calculate delta Y on the average
  
  common_min <- min(Spatial_yield_by_GSA$delta_Y, na.rm = TRUE)
  common_max <- max(Spatial_yield_by_GSA$delta_Y, na.rm = TRUE)
  
  # Plot 1: MPA effect catch change
  map_effect_biomass <- ggplot(Spatial_yield_by_GSA) +
    geom_sf(aes(fill = delta_Y), color = NA) +  # remove border lines for a cleaner look
    geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +
    
    scale_fill_gradient2(
      low = "deepskyblue4",
      mid = "lightyellow",
      high = "darkred",
      midpoint = 0,
      limits = c(-50, 50),
      oob = scales::squish,
      name = expression(Delta~Y~"(%)"),
      breaks = c(-50, -25, 0, 25, 50),
      labels = c("≤ -50%", -25, 0, 25, "≥ 50%")) +
    coord_sf(expand = FALSE) +  # remove whitespace around the map
    
    labs(
      title = paste0(sp_name),
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
  
  # Save the plot
  ggsave(
    here(paste0("figures/spatial_catch/FigS5/Delta_Y_without_MPAs_", sp_name, ".png")),
    plot = map_effect_biomass,
    dpi = 300,
    width = 10,
    height = 6
  )
  
}
