library(tidyverse)
library(ncdf4)
library(here)
library(sf)


# Load countries shapefile
worldbounds = st_read(here("data-mpa-CC-paper/shapefiles/world_map/world-bounds/world-bounds-a.shp"))
worldbounds_3035 = st_transform(worldbounds, 3035)

# Define bounding box around the Mediterranean region in EPSG:3035
med_bbox <- st_as_sfc(st_bbox(c(xmin = 2880851, xmax = 6680851, ymin = 2515444, ymax = 865443.7), crs = 3035))

# Clip worldbounds to this bbox
med_bounds <- st_intersection(worldbounds_3035, med_bbox)



nc = nc_open(here("data-mpa-CC-paper/model_outputs/spatial_nompa/Yield_nompa_scenario_CC.nc"))
yield_nompa_future <- ncvar_get(nc, varid = "Catches")
yield_nompa_future <- apply(yield_nompa_future, c(1,2,3), mean, na.rm = T) # mean over reps

nc = nc_open(here("data-mpa-CC-paper/model_outputs/spatial_nompa/Yield_nompa_scenario_noCC.nc"))
yield_nompa_present <- ncvar_get(nc, varid = "Catches")
yield_nompa_present <- apply(yield_nompa_present, c(1,2,3), mean, na.rm = T) # mean over reps

# Yy species  ------
species_classif = read.csv(here("data-mpa-CC-paper/classification_species.csv")) %>%
  filter(! species_id %in% c(0,1,2))

sp_name = "all_species_no_amphihaline"

species_id_list = species_classif$species_id
species_name_list = species_classif$species_name

length(species_id_list)
print(species_name_list)

# Calculate change 3 exotic species
# sp29 - E teres
tot_yield_nompa_future = yield_nompa_future[1:190, 1:83, 29+1]
sp_Y_future = sum(tot_yield_nompa_future, na.rm = T)

tot_yield_nompa_present = yield_nompa_present[1:190, 1:83, 29+1]
sp_Y_present = sum(tot_yield_nompa_present, na.rm = T)

Y_change = (sp_Y_future - sp_Y_present)/sp_Y_present * 100
print(Y_change)
# sp68 - S lessepsianus
tot_yield_nompa_future = yield_nompa_future[1:190, 1:83, 68+1]
sp_Y_future = sum(tot_yield_nompa_future, na.rm = T)

tot_yield_nompa_present = yield_nompa_present[1:190, 1:83, 68+1]
sp_Y_present = sum(tot_yield_nompa_present, na.rm = T)

Y_change = (sp_Y_future - sp_Y_present)/sp_Y_present * 100
print(Y_change)

# sp97 - U. moluccensis
tot_yield_nompa_future = yield_nompa_future[1:190, 1:83, 97+1]
sp_Y_future = sum(tot_yield_nompa_future, na.rm = T)

tot_yield_nompa_present = yield_nompa_present[1:190, 1:83, 97+1]
sp_Y_present = sum(tot_yield_nompa_present, na.rm = T)

Y_change = (sp_Y_future - sp_Y_present)/sp_Y_present * 100
print(Y_change)


# All species
tot_yield_nompa_future = yield_nompa_future[1:190, 1:83, species_id_list+1]
tot_yield_nompa_future = apply(tot_yield_nompa_future, c(1,2), sum)

tot_yield_nompa_present = yield_nompa_present[1:190, 1:83, species_id_list+1]
tot_yield_nompa_present = apply(tot_yield_nompa_present, c(1,2), sum)


spatial_yield_file <- here(
  paste0("data-mpa-CC-paper/model_outputs/spatial_mpa_10/CC_2100/spatial_yield_all_scenarios_mpa10_", sp_name, ".gpkg")
)

if (file.exists(spatial_yield_file)) {
  message("File exists — skipping: ", spatial_yield_file)
} else {
  mpa_scenarios = c("Existing_network",
                    "Micheli",
                    "Mazor_scenario_8",
                    "Mazor_scenario_9",
                    "EEZ-conservation-rep1",
                    "EEZ-conservation-rep2",
                    "EEZ-conservation-rep3",
                    "EEZ-conservation-rep4",
                    "EEZ-conservation-rep5",
                    "EEZ-conservation-rep6",
                    "EEZ-conservation-rep7",
                    "EEZ-conservation-rep8",
                    "EEZ-conservation-rep9",
                    "EEZ-conservation-rep10",
                    "Random-rep11",
                    "Random-rep12",
                    "Random-rep13",
                    "Random-rep14",
                    "Random-rep15",
                    "Random-rep16",
                    "Random-rep17",
                    "Random-rep18",
                    "Random-rep19",
                    "Random-rep20")
  
  for (mpa_scenario in mpa_scenarios){
    nc = nc_open(here(paste0("data-mpa-CC-paper/model_outputs/spatial_mpa_10/CC_2100/", mpa_scenario, "/Yield_mpa_scenario.nc")))
    yield_mpa_future <- ncvar_get(nc, varid = "Catches")
    yield_mpa_future <- apply(yield_mpa_future, c(1,2,3), mean, na.rm = T) # mean over reps
    tot_yield_mpa_future = yield_mpa_future[1:190, 1:83, species_id_list+1]
    tot_yield_mpa_future = apply(tot_yield_mpa_future, c(1,2), sum)
    
    tot_yield_mpa_future2 = tot_yield_mpa_future[,83:1]
    tot_yield_mpa_future2_vect = as.vector(tot_yield_mpa_future2)
    
    if (mpa_scenario == mpa_scenarios[1]){
      Y_diff_all_scens = tot_yield_mpa_future2_vect
      
    } else{
      Y_diff_all_scens = cbind(Y_diff_all_scens, tot_yield_mpa_future2_vect)
    }
  }
  
  nscen = seq(1, 24, 1)
  colnames(Y_diff_all_scens) = paste0("scen", nscen)
  
  
  Spatial_yield_diff_all_scenarios = Med_grid_GSA_EEZ_df %>%
    mutate(cell_ID = as.numeric(cell_ID)) %>%
    arrange(cell_ID) 
  
  Spatial_yield_diff = cbind(Spatial_yield_diff_all_scenarios, Y_diff_all_scens)
  
  st_write(Spatial_yield_diff, here(paste0("data-mpa-CC-paper/model_outputs/spatial_mpa_10/CC_2100/spatial_yield_all_scenarios_mpa10_", sp_name, ".gpkg")))
  
  
}



Spatial_yield = st_read(here(paste0("data-mpa-CC-paper/model_outputs/spatial_mpa_10/CC_2100/spatial_yield_all_scenarios_mpa10_", sp_name, ".gpkg")))


#### First - mean S2 & S3 and then average 6 scenarios -------------------------

mpa_scenarios = c("Existing_network",
                  "Micheli",
                  "Mazor_scenario_8",
                  "Mazor_scenario_9",
                  "EEZ-conservation-rep1",
                  "EEZ-conservation-rep2",
                  "EEZ-conservation-rep3",
                  "EEZ-conservation-rep4",
                  "EEZ-conservation-rep5",
                  "EEZ-conservation-rep6",
                  "EEZ-conservation-rep7",
                  "EEZ-conservation-rep8",
                  "EEZ-conservation-rep9",
                  "EEZ-conservation-rep10",
                  "Random-rep11",
                  "Random-rep12",
                  "Random-rep13",
                  "Random-rep14",
                  "Random-rep15",
                  "Random-rep16",
                  "Random-rep17",
                  "Random-rep18",
                  "Random-rep19",
                  "Random-rep20")

Spatial_yield = st_read(here(paste0("data-mpa-CC-paper/model_outputs/spatial_mpa_10/CC_2100/spatial_yield_all_scenarios_mpa10_", sp_name, ".gpkg"))) 

tot_yield_nompa_future2 = tot_yield_nompa_future[,83:1]
tot_yield_nompa_future2_vect = as.vector(tot_yield_nompa_future2)

Spatial_yield$Y_nompa_future = tot_yield_nompa_future2_vect

tot_yield_nompa_present2 = tot_yield_nompa_present[,83:1]
tot_yield_nompa_present2_vect = as.vector(tot_yield_nompa_present2)

Spatial_yield$Y_nompa_present = tot_yield_nompa_present2_vect

# Calculate MPA effect on basin-wide biomass for paper
Yield_all_scenarios = Spatial_yield %>%
  st_drop_geometry() %>%
  pivot_longer(10:33, names_to = "mpa_scenario", values_to = "Y_mpa_future") %>%
  mutate(mpa_scenario_group = case_when(mpa_scenario %in% c(paste0("scen", 5:14)) ~ "EEZ-conservation",
                                        mpa_scenario %in% c(paste0("scen", 15:24)) ~ "Random",
                                        TRUE ~ mpa_scenario)) %>%
  
  group_by(mpa_scenario, mpa_scenario_group) %>%
  # basin-wide sum
  summarize(Y_nompa_future = sum(Y_nompa_future, na.rm = T),
            Y_nompa_present = sum(Y_nompa_present, na.rm = T),
            Y_mpa_future = sum(Y_mpa_future, na.rm = T)) %>%
  ungroup() %>%
  mutate(diff_Y = Y_nompa_future - Y_nompa_present,
         delta_Y = (Y_nompa_future - Y_nompa_present)/Y_nompa_present * 100,
         diff_Y_mpa = Y_mpa_future - Y_nompa_present,
         delta_Y_mpa = (Y_mpa_future - Y_nompa_present)/Y_nompa_present * 100) %>%
  
  mutate(delta_Y = case_when(Y_nompa_present == 0 & Y_nompa_future == 0 ~ 0,
                             TRUE ~ delta_Y)) %>%
  
  mutate(diff_delta = delta_Y_mpa - delta_Y) %>%
  
  group_by(mpa_scenario_group) %>%
  summarize(delta_Y_6 = mean(delta_Y, na.rm = T),
            delta_Y_mpa_6 = mean(delta_Y_mpa, na.rm = T),
            diff_delta_6 = mean(diff_delta, na.rm = T)) %>%
  
  ungroup() 

mean_diff_delta = mean(Yield_all_scenarios$diff_delta_6, na.rm = T)

print(mean_diff_delta)

sd_diff_delta = sd(Yield_all_scenarios$diff_delta_6, na.rm = T)

print(sd_diff_delta)

# For figures ----------------------------

Spatial_yield_all_scenarios = Spatial_yield %>%
  st_drop_geometry() %>%
  pivot_longer(10:33, names_to = "mpa_scenario", values_to = "Y_mpa_future") %>%
  mutate(mpa_scenario_group = case_when(mpa_scenario %in% c(paste0("scen", 5:14)) ~ "EEZ-conservation",
                                        mpa_scenario %in% c(paste0("scen", 15:24)) ~ "Random",
                                        TRUE ~ mpa_scenario)) %>%
  mutate(diff_Y = Y_nompa_future - Y_nompa_present,
         delta_Y = (Y_nompa_future - Y_nompa_present)/Y_nompa_present * 100,
         diff_Y_mpa = Y_mpa_future - Y_nompa_present,
         delta_Y_mpa = (Y_mpa_future - Y_nompa_present)/Y_nompa_present * 100) %>%
  
  mutate(delta_Y = case_when(Y_nompa_present == 0 & Y_nompa_future == 0 ~ 0,
                             TRUE ~ delta_Y)) %>%
  
  mutate(diff_delta = delta_Y_mpa - delta_Y) %>%
  
  group_by(cell_ID, VALUE, GSA, mpa_scenario_group) %>%
  summarize(delta_Y_6 = mean(delta_Y, na.rm = T),
            delta_Y_mpa_6 = mean(delta_Y_mpa, na.rm = T),
            diff_delta_6 = mean(diff_delta, na.rm = T)) %>%
  
  ungroup() %>%
  group_by(cell_ID, VALUE, GSA) %>%
  summarize(delta_Y = mean(delta_Y_6, na.rm = T),
            delta_Y_mpa = mean(delta_Y_mpa_6, na.rm = T),
            diff_delta = mean(diff_delta_6, na.rm = T)) %>%
  ungroup() 


  
Spatial_catch_all_scenarios = Med_grid_GSA_EEZ_df %>%
  mutate(cell_ID = as.numeric(cell_ID)) %>%
  arrange(cell_ID) %>%
  left_join(Spatial_yield_all_scenarios, by = c("cell_ID", "GSA", "VALUE")) %>%
  mutate(delta_Y_mpa = case_when(VALUE == 0 & is.na(delta_Y_mpa) ~ 0,
                                 TRUE ~ delta_Y_mpa),
         delta_Y = case_when(VALUE == -99 ~ NA,
                             TRUE ~ delta_Y),
         diff_delta = case_when(VALUE == 0 & is.na(diff_delta) ~ 0,
                                TRUE ~ diff_delta)) # so no NANs in the Med and NAN in other seas


### -----
# Plot difference by GSA

all_vals <- c(Spatial_catch_all_scenarios$delta_Y,
              Spatial_catch_all_scenarios$delta_Y_mpa)

common_min <- min(all_vals, na.rm = TRUE)
common_max <- max(all_vals, na.rm = TRUE)

# Plot 1: MPA effect catch change
map_effect_yield <- ggplot(Spatial_catch_all_scenarios) +
  geom_sf(aes(fill = delta_Y), color = NA) +  # remove border lines for a cleaner look
  geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +

  
  scale_fill_gradient2(
    low = "deepskyblue4",
    mid = "lightyellow",
    high = "darkred",
    midpoint = 0,
    limits = c(-30, 30),
    oob = scales::squish,
    name = expression(Delta~Y~"(%)"),
    breaks = c(-30, -15, 0, 15, 30),
    labels = c("≤ -30%", -15, 0, 15, "≥ 30%")) +
  
  
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = "Relative change in total yield between current and future periods",
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

print(map_effect_yield)
# Save the plot
ggsave(
  here(paste0("figures/spatial_yield/total_yield/Delta_Y_future_present_30_limit_mean_6_scens.png")),
  plot = map_effect_yield,
  dpi = 300,
  width = 10,
  height = 6
)

# Plot 2: catch change before MPAs
map_effect_yield_mpa <- ggplot(Spatial_catch_all_scenarios) +
  geom_sf(aes(fill = delta_Y_mpa), color = NA) +  # remove border lines for a cleaner look
  geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +
  
  scale_fill_gradient2(
    low = "deepskyblue4",
    mid = "lightyellow",
    high = "darkred",
    midpoint = 0,
    limits = c(-30, 30),
    oob = scales::squish,
    name = expression(Delta~Y~"(%)"),
    breaks = c(-30, -15, 0, 15, 30),
    labels = c("≤ -30%", -15, 0, 15, "≥ 30%")) +

  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = paste0("Change in yield with climate change and 10% MPAs - ", sp_name) ,
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

print(map_effect_yield_mpa)
# Save the plot
ggsave(
  here("figures/spatial_yield/total_yield/Delta_Y_with_MPAs_all_scens_30_limit_mean_6_scens.png"),
  plot = map_effect_yield_mpa,
  dpi = 300,
  width = 10,
  height = 6
)

# Plot 3: diff delta
common_min = min(Spatial_catch_all_scenarios$diff_delta, na.rm = T)
common_max = max(Spatial_catch_all_scenarios$diff_delta, na.rm = T)

map_effect_mpa_yield <- ggplot(Spatial_catch_all_scenarios) +
  
  geom_sf(aes(fill = diff_delta), color = NA) +  # remove border lines for a cleaner look
  geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +
  

  scale_fill_gradient2(
    low = "darkgreen",
    mid = "lightyellow",
    high = "purple",
    midpoint = 0,
    limits = c(-10, 10),
    oob = scales::squish,
    name = expression(Delta~Y~"(%)"),
    breaks = c(-10, -5, 0, 5, 10),
    labels = c("≤ -10%", -5, 0, 5, "≥ 10%")) +
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = "MPA impact of yield change",
    fill = expression(Delta~Y~"(%)")
  ) +
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = paste0("Diff delta - ", sp_name),
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

print(map_effect_mpa_yield)
# Save the plot
ggsave(
  here(paste0("figures/spatial_yield/total_yield/MPA_effect_total_yield_all_scens_mean_6_scens.png")),
  plot = map_effect_mpa_yield,
  dpi = 300,
  width = 10,
  height = 6
)


