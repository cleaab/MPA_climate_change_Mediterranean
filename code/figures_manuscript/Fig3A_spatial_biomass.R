library(tidyverse)
library(ncdf4)
library(here)
library(sf)


# Load countries shapefile
worldbounds = st_read(here("data/shapefiles/world_map/world-bounds/world-bounds-a.shp"))
worldbounds_3035 = st_transform(worldbounds, 3035)

# Define bounding box around the Mediterranean region in EPSG:3035
med_bbox <- st_as_sfc(st_bbox(c(xmin = 2880851, xmax = 6680851, ymin = 2515444, ymax = 865443.7), crs = 3035))

# Clip worldbounds to this bbox
med_bounds <- st_intersection(worldbounds_3035, med_bbox)



nc = nc_open(here("data/model_outputs/spatial_nompa/Biomass_nompa_scenario_CC.nc"))
biomass_nompa_future <- ncvar_get(nc, varid = "Biomass")
biomass_nompa_future <- apply(biomass_nompa_future, c(1,2,3), mean, na.rm = T) # mean over reps

nc = nc_open(here("data/model_outputs/spatial_nompa/Biomass_nompa_scenario_noCC.nc"))
biomass_nompa_present <- ncvar_get(nc, varid = "Biomass")
biomass_nompa_present <- apply(biomass_nompa_present, c(1,2,3), mean, na.rm = T) # mean over reps

# By species  ------
species_classif = read.csv(here("data/classification_species.csv")) %>%
  filter(! species_id %in% c(0,1,2))

sp_name = "all_species_no_amphihaline"

species_id_list = species_classif$species_id
species_name_list = species_classif$species_name

length(species_id_list)
print(species_name_list)

# Calculate change 3 exotic species
# sp29 - E teres
tot_biomass_nompa_future = biomass_nompa_future[1:190, 1:83, 29+1]
sp_B_future = sum(tot_biomass_nompa_future, na.rm = T)

tot_biomass_nompa_present = biomass_nompa_present[1:190, 1:83, 29+1]
sp_B_present = sum(tot_biomass_nompa_present, na.rm = T)

B_change = (sp_B_future - sp_B_present)/sp_B_present * 100
print(B_change)
# sp68 - S lessepsianus
tot_biomass_nompa_future = biomass_nompa_future[1:190, 1:83, 68+1]
sp_B_future = sum(tot_biomass_nompa_future, na.rm = T)

tot_biomass_nompa_present = biomass_nompa_present[1:190, 1:83, 68+1]
sp_B_present = sum(tot_biomass_nompa_present, na.rm = T)

B_change = (sp_B_future - sp_B_present)/sp_B_present * 100
print(B_change)

# sp97 - U. moluccensis
tot_biomass_nompa_future = biomass_nompa_future[1:190, 1:83, 97+1]
sp_B_future = sum(tot_biomass_nompa_future, na.rm = T)

tot_biomass_nompa_present = biomass_nompa_present[1:190, 1:83, 97+1]
sp_B_present = sum(tot_biomass_nompa_present, na.rm = T)

B_change = (sp_B_future - sp_B_present)/sp_B_present * 100
print(B_change)

# sp44 - M poutassou
tot_biomass_nompa_future = biomass_nompa_future[1:190, 1:83, 51+1]
sp_B_future = sum(tot_biomass_nompa_future, na.rm = T)

tot_biomass_nompa_present = biomass_nompa_present[1:190, 1:83, 51+1]
sp_B_present = sum(tot_biomass_nompa_present, na.rm = T)

B_change = (sp_B_future - sp_B_present)/sp_B_present * 100
print(B_change)

# All species
tot_biomass_nompa_future = biomass_nompa_future[1:190, 1:83, species_id_list+1]
tot_biomass_nompa_future = apply(tot_biomass_nompa_future, c(1,2), sum)

tot_biomass_nompa_present = biomass_nompa_present[1:190, 1:83, species_id_list+1]
tot_biomass_nompa_present = apply(tot_biomass_nompa_present, c(1,2), sum)

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
  nc = nc_open(here(paste0("data/model_outputs/spatial_mpa_10/CC_2100/", mpa_scenario, "/Biomass_mpa_scenario.nc")))
  biomass_mpa_future <- ncvar_get(nc, varid = "Biomass")
  biomass_mpa_future <- apply(biomass_mpa_future, c(1,2,3), mean, na.rm = T) # mean over reps
  tot_biomass_mpa_future = biomass_mpa_future[1:190, 1:83, species_id_list+1]
  tot_biomass_mpa_future = apply(tot_biomass_mpa_future, c(1,2), sum)
  
  tot_biomass_mpa_future2 = tot_biomass_mpa_future[,83:1]
  tot_biomass_mpa_future2_vect = as.vector(tot_biomass_mpa_future2)
  
  if (mpa_scenario == mpa_scenarios[1]){
    B_diff_all_scens = tot_biomass_mpa_future2_vect
    
  } else{
    B_diff_all_scens = cbind(B_diff_all_scens, tot_biomass_mpa_future2_vect)
  }
}

nscen = seq(1, 24, 1)
colnames(B_diff_all_scens) = paste0("scen", nscen)


Spatial_biomass_diff_all_scenarios = Med_grid_GSA_EEZ_df %>%
  mutate(cell_ID = as.numeric(cell_ID)) %>%
  arrange(cell_ID) 

Spatial_biomass_diff = cbind(Spatial_biomass_diff_all_scenarios, B_diff_all_scens)

st_write(Spatial_biomass_diff, here(paste0("data/model_outputs/spatial_mpa_10/CC_2100/spatial_biomass_all_scenarios_mpa10_", sp_name, ".gpkg")))


# If file exists: start from here
# Average of 24 MPA scenarios ---------------------------------------------------
Spatial_biomass = st_read(here(paste0("data/model_outputs/spatial_mpa_10/CC_2100/spatial_biomass_all_scenarios_mpa10_", sp_name, ".gpkg")))


# Compute row-wise mean while ignoring geometry
Spatial_biomass$B_mpa_future <- rowMeans(st_drop_geometry(Spatial_biomass)[, 10:33], na.rm = TRUE)

# Convert mpa scenarios colnames numeric (handle potential factors/characters)
Spatial_biomass[paste0("scen", 1:24)] <- lapply(Spatial_biomass[paste0("scen", 1:24)], function(x) as.numeric(as.character(x)))

# Compute row-wise mean while ignoring geometry
Spatial_biomass$scen_mean <- rowMeans(st_drop_geometry(Spatial_biomass)[, paste0("scen", 1:24)], na.rm = TRUE)

tot_biomass_nompa_future2 = tot_biomass_nompa_future[,83:1]
tot_biomass_nompa_future2_vect = as.vector(tot_biomass_nompa_future2)

Spatial_biomass$B_nompa_future = tot_biomass_nompa_future2_vect

tot_biomass_nompa_present2 = tot_biomass_nompa_present[,83:1]
tot_biomass_nompa_present2_vect = as.vector(tot_biomass_nompa_present2)

Spatial_biomass$B_nompa_present = tot_biomass_nompa_present2_vect

### -----
# Plot difference by GSA
Spatial_biomass = Spatial_biomass %>%
  mutate(B_nompa_present = case_when(B_nompa_present == 0 ~ 10^-3,
                                     TRUE ~ B_nompa_present)) %>%
  
  mutate(delta_scen1 = (scen1 - B_nompa_present)/B_nompa_present * 100,
         delta_scen2 = (scen2 - B_nompa_present)/B_nompa_present * 100,
         delta_scen3 = (scen3 - B_nompa_present)/B_nompa_present * 100,
         delta_scen4 = (scen4 - B_nompa_present)/B_nompa_present * 100,
         delta_scen5 = (scen5 - B_nompa_present)/B_nompa_present * 100,
         delta_scen6 = (scen6 - B_nompa_present)/B_nompa_present * 100,
         delta_scen7 = (scen7 - B_nompa_present)/B_nompa_present * 100,
         delta_scen8 = (scen8 - B_nompa_present)/B_nompa_present * 100,
         delta_scen9 = (scen9 - B_nompa_present)/B_nompa_present * 100,
         delta_scen10 = (scen10 - B_nompa_present)/B_nompa_present * 100,
         delta_scen11 = (scen11 - B_nompa_present)/B_nompa_present * 100,
         delta_scen12 = (scen12 - B_nompa_present)/B_nompa_present * 100,
         delta_scen13 = (scen13 - B_nompa_present)/B_nompa_present * 100,
         delta_scen14 = (scen14 - B_nompa_present)/B_nompa_present * 100,
         delta_scen15 = (scen15 - B_nompa_present)/B_nompa_present * 100,
         delta_scen16 = (scen16 - B_nompa_present)/B_nompa_present * 100,
         delta_scen17 = (scen17 - B_nompa_present)/B_nompa_present * 100,
         delta_scen18 = (scen18 - B_nompa_present)/B_nompa_present * 100,
         delta_scen19 = (scen19 - B_nompa_present)/B_nompa_present * 100,
         delta_scen20 = (scen20 - B_nompa_present)/B_nompa_present * 100,
         delta_scen21 = (scen21 - B_nompa_present)/B_nompa_present * 100,
         delta_scen22 = (scen22 - B_nompa_present)/B_nompa_present * 100,
         delta_scen23 = (scen23 - B_nompa_present)/B_nompa_present * 100,
         delta_scen24 = (scen24 - B_nompa_present)/B_nompa_present * 100) %>%
  
  mutate(diff_B = B_nompa_future - B_nompa_present,
         delta_B = (B_nompa_future - B_nompa_present)/B_nompa_present * 100,
         diff_B_mpa = scen_mean - B_nompa_present,
         delta_B_mpa = (scen_mean - B_nompa_present)/B_nompa_present * 100) %>%
  
  mutate(diff_delta = delta_B_mpa - delta_B)

# Total basin:
B_future_MPA = sum(Spatial_biomass$scen_mean, na.rm = T)
print(B_future_MPA)
B_baseline_NoMPA = sum(Spatial_biomass$B_nompa_present, na.rm = T)
print(B_baseline_NoMPA)
B_future_NoMPA = sum(Spatial_biomass$B_nompa_future, na.rm = T)
print(B_future_NoMPA)

delta_Ind_futureMPA = (B_future_MPA - B_baseline_NoMPA)/B_baseline_NoMPA * 100
print(delta_Ind_futureMPA)
delta_Ind_futureNoMPA = (B_future_NoMPA - B_baseline_NoMPA)/B_baseline_NoMPA * 100
print(delta_Ind_futureNoMPA)

diff = delta_Ind_futureMPA - delta_Ind_futureNoMPA
print(diff)

# Compute row-wise mean while ignoring geometry
Spatial_biomass$mean_delta_scen <- rowMeans(st_drop_geometry(Spatial_biomass)[, paste0("delta_scen", 1:24)], na.rm = TRUE)

# Compute standard deviation accross scenarios
Spatial_biomass$sd_delta_scen <- apply(
  st_drop_geometry(Spatial_biomass)[, paste0("delta_scen", 1:24)],
  1,
  sd,
  na.rm = TRUE
)

#### RELATIVE CHANGE - calculate average Y accross scenarios by GSA and then calculate delta Y on the average

all_vals <- c(Spatial_biomass$delta_B,
              Spatial_biomass$delta_B_mpa)

common_min <- min(all_vals, na.rm = TRUE)
common_max <- max(all_vals, na.rm = TRUE)

# Plot 1: MPA effect catch change
map_effect_biomass <- ggplot(Spatial_biomass) +
  geom_sf(aes(fill = delta_B), color = NA) +  # remove border lines for a cleaner look
  geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +
  
  # scale_fill_gradient2(
  #   low = "deepskyblue4",     
  #   mid = "lightyellow",      
  #   high = "darkred",         
  #   midpoint = 0,
  #   trans = "pseudo_log",
  #   limits = c(common_min, common_max),
  #   name = expression(Delta~B~"(%)")  # nicer label formatting
  # )

scale_fill_gradient2(
  low = "deepskyblue4",
  mid = "lightyellow",
  high = "darkred",
  midpoint = 0,
  limits = c(-50, 50),
  oob = scales::squish,
  name = expression(Delta~B~"(%)"),
  breaks = c(-50, -25, 0, 25, 50),
  labels = c("≤ -50%", -25, 0, 25, "≥ 50%")) +


  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = "Relative change in total biomass between current and future periods",
    fill = expression(Delta~B~"(%)")
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
  here(paste0("figures/spatial_biomass/total_biomass/Delta_B_future_present.png")),
  plot = map_effect_biomass,
  dpi = 300,
  width = 10,
  height = 6
)

# Plot 2: catch change before MPAs
map_effect_biomass <- ggplot(Spatial_biomass) +
  geom_sf(aes(fill = delta_B_mpa), color = NA) +  # remove border lines for a cleaner look
  scale_fill_gradient2(
    low = "deepskyblue4",     
    mid = "lightyellow",      
    high = "darkred",         
    midpoint = 0,
    trans = "pseudo_log",
    limits = c(common_min, common_max),
    name = expression(Delta~B~"(%)")  # nicer label formatting
  ) +
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = paste0("Change in biomass with climate change and 10% MPAs - ", sp_name) ,
    fill = expression(Delta~B~"(%)")
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
  here(paste0("figures/spatial_biomass/groups/Delta_B_by_GSA_with_MPAs_", sp_name, ".png")),
  plot = map_effect_biomass,
  dpi = 300,
  width = 10,
  height = 6
)

# Plot 3: diff delta
common_min = min(Spatial_biomass$diff_delta, na.rm = T)
common_max = max(Spatial_biomass$diff_delta, na.rm = T)

map_effect_biomass <- ggplot(Spatial_biomass) +

  geom_sf(aes(fill = diff_delta), color = NA) +  # remove border lines for a cleaner look
  geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +
  
  # scale_fill_gradient2(
  #   low = "darkgreen",     
  #   mid = "lightyellow",      
  #   high = "purple",         
  #   midpoint = 0,
  #   trans = "pseudo_log",
  #   limits = c(common_min, common_max),
  #   name = expression(Delta~B~"(%)")  # nicer label formatting
  # ) 
scale_fill_gradient2(
  low = "darkgreen",
  mid = "lightyellow",
  high = "purple",
  midpoint = 0,
  limits = c(-10, 10),
  oob = scales::squish,
  name = expression(Delta~B~"(%)"),
  breaks = c(-10, -5, 0, 5, 10),
  labels = c("≤ -10%", -5, 0, 5, "≥ 10%")) +
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = "MPA impact of biomass change",
    fill = expression(Delta~B~"(%)")
  ) +
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = paste0("Diff delta - ", sp_name),
    fill = expression(Delta~B~"(%)")
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
  here(paste0("figures/spatial_biomass/total_biomass/MPA_effect_total_biomass_all_scens.png")),
  plot = map_effect_biomass,
  dpi = 300,
  width = 10,
  height = 6
)



#### ABSOLUTE CHANGES
all_vals <- c(Spatial_biomass$diff_B,
              Spatial_biomass$diff_B_mpa)

common_min <- min(all_vals, na.rm = TRUE)
common_max <- max(all_vals, na.rm = TRUE)

# Plot 1: MPA effect catch change
map_effect_biomass <- ggplot(Spatial_biomass) +
  geom_sf(aes(fill = diff_B), color = NA) +  # remove border lines for a cleaner look
  scale_fill_gradient2(
    low = "deepskyblue4",     
    mid = "lightyellow",      
    high = "darkred",         
    midpoint = 0,
    trans = "pseudo_log",
    limits = c(common_min, common_max),
    name = expression(Delta~B~"(tons)")  # nicer label formatting
  ) +
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = paste0("Mean change in biomass with climate change -", sp_name),
    fill = expression(Delta~B~"(tons)")
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
  here("figures/spatial_biomass/total_biomass/Diff_B_future_present_all_scens.png"),
  plot = map_effect_biomass,
  dpi = 300,
  width = 10,
  height = 6
)

# Plot 2: catch change before MPAs
map_effect_biomass <- ggplot(Spatial_biomass) +
  geom_sf(aes(fill = diff_B_mpa), color = NA) +  # remove border lines for a cleaner look
  scale_fill_gradient2(
    low = "deepskyblue4",     
    mid = "lightyellow",      
    high = "darkred",         
    midpoint = 0,
    trans = "pseudo_log",
    limits = c(common_min, common_max),
    name = expression(Delta~B~"(tons)")  # nicer label formatting
  ) +
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = paste0("Change in biomass with climate change and 10% MPAs - ", sp_name) ,
    fill = expression(Delta~B~"(tons)")
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
  here("figures/spatial_biomass/total_biomass/Diff_B_future_present_with_MPAs_all_scens.png"),
  plot = map_effect_biomass,
  dpi = 300,
  width = 10,
  height = 6
)


#### First - mean S2 & S3 and then average 6 scenarios -------------------------
Spatial_biomass = st_read(here(paste0("data/model_outputs/spatial_mpa_10/CC_2100/spatial_biomass_all_scenarios_mpa10_", sp_name, ".gpkg")))

tot_biomass_nompa_future2 = tot_biomass_nompa_future[,83:1]
tot_biomass_nompa_future2_vect = as.vector(tot_biomass_nompa_future2)

Spatial_biomass$B_nompa_future = tot_biomass_nompa_future2_vect

tot_biomass_nompa_present2 = tot_biomass_nompa_present[,83:1]
tot_biomass_nompa_present2_vect = as.vector(tot_biomass_nompa_present2)

Spatial_biomass$B_nompa_present = tot_biomass_nompa_present2_vect

# Calculate MPA effect on basin-wide biomass for paper
Biomass_all_scenarios = Spatial_biomass %>%
  st_drop_geometry() %>%
  pivot_longer(10:33, names_to = "mpa_scenario", values_to = "B_mpa_future") %>%
  mutate(mpa_scenario_group = case_when(mpa_scenario %in% c(paste0("scen", 5:14)) ~ "EEZ-conservation",
                                        mpa_scenario %in% c(paste0("scen", 15:24)) ~ "Random",
                                        TRUE ~ mpa_scenario)) %>%
  
  group_by(mpa_scenario, mpa_scenario_group) %>%
  # basin-wide sum
  summarize(B_nompa_future = sum(B_nompa_future, na.rm = T),
            B_nompa_present = sum(B_nompa_present, na.rm = T),
            B_mpa_future = sum(B_mpa_future, na.rm = T)) %>%
  ungroup() %>%
  mutate(diff_B = B_nompa_future - B_nompa_present,
         delta_B = (B_nompa_future - B_nompa_present)/B_nompa_present * 100,
         diff_B_mpa = B_mpa_future - B_nompa_present,
         delta_B_mpa = (B_mpa_future - B_nompa_present)/B_nompa_present * 100) %>%
  
  mutate(delta_B = case_when(B_nompa_present == 0 & B_nompa_future == 0 ~ 0,
                             TRUE ~ delta_B)) %>%
  
  mutate(diff_delta = delta_B_mpa - delta_B) %>%
  
  group_by(mpa_scenario_group) %>%
  summarize(delta_B_6 = mean(delta_B, na.rm = T),
            delta_B_mpa_6 = mean(delta_B_mpa, na.rm = T),
            diff_delta_6 = mean(diff_delta, na.rm = T)) %>%
  
  ungroup() 

mean_diff_delta = mean(Biomass_all_scenarios$diff_delta_6, na.rm = T)

print(mean_diff_delta)

sd_diff_delta = sd(Biomass_all_scenarios$diff_delta_6, na.rm = T)

print(sd_diff_delta)

# For figures ----

Spatial_biomass_all_scenarios = Spatial_biomass %>%
  st_drop_geometry() %>%
  pivot_longer(10:33, names_to = "mpa_scenario", values_to = "B_mpa_future") %>%
  mutate(mpa_scenario_group = case_when(mpa_scenario %in% c(paste0("scen", 5:14)) ~ "EEZ-conservation",
                                        mpa_scenario %in% c(paste0("scen", 15:24)) ~ "Random",
                                        TRUE ~ mpa_scenario)) %>%
  mutate(diff_B = B_nompa_future - B_nompa_present,
         delta_B = (B_nompa_future - B_nompa_present)/B_nompa_present * 100,
         diff_B_mpa = B_mpa_future - B_nompa_present,
         delta_B_mpa = (B_mpa_future - B_nompa_present)/B_nompa_present * 100) %>%
  
  mutate(delta_B = case_when(B_nompa_present == 0 & B_nompa_future == 0 ~ 0,
                             TRUE ~ delta_B)) %>%
  
  mutate(diff_delta = delta_B_mpa - delta_B) %>%
  
  group_by(cell_ID, VALUE, GSA, mpa_scenario_group) %>%
  summarize(delta_B_6 = mean(delta_B, na.rm = T),
            delta_B_mpa_6 = mean(delta_B_mpa, na.rm = T),
            diff_delta_6 = mean(diff_delta, na.rm = T)) %>%
  
  ungroup() %>%
  group_by(cell_ID, VALUE, GSA) %>%
  summarize(delta_B = mean(delta_B_6, na.rm = T),
            delta_B_mpa = mean(delta_B_mpa_6, na.rm = T),
            diff_delta = mean(diff_delta_6, na.rm = T)) %>%
  ungroup()

Spatial_B_all_scenarios = Med_grid_GSA_EEZ_df %>%
  mutate(cell_ID = as.numeric(cell_ID)) %>%
  arrange(cell_ID) %>%
  left_join(Spatial_biomass_all_scenarios, by = c("cell_ID", "GSA", "VALUE")) %>%
  mutate(delta_B_mpa = case_when(VALUE == 0 & is.na(delta_B_mpa) ~ 0,
                                 TRUE ~ delta_B_mpa),
         delta_B = case_when(VALUE == -99 ~ NA,
                             TRUE ~ delta_B),
         diff_delta = case_when(VALUE == 0 & is.na(diff_delta) ~ 0,
                                TRUE ~ diff_delta)) # so no NANs in the Med and NAN in other seas


#### RELATIVE CHANGE - calculate average Y accross scenarios by GSA and then calculate delta Y on the average

all_vals <- c(Spatial_B_all_scenarios$delta_B,
              Spatial_B_all_scenarios$delta_B_mpa)

common_min <- min(all_vals, na.rm = TRUE)
common_max <- max(all_vals, na.rm = TRUE)


# Plot 1: MPA effect catch change
map_effect_biomass <- ggplot(Spatial_B_all_scenarios) +
  geom_sf(aes(fill = delta_B), color = NA) +  # remove border lines for a cleaner look
  geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +
  
  scale_fill_gradient2(
    low = "deepskyblue4",
    mid = "lightyellow",
    high = "darkred",
    midpoint = 0,
    limits = c(-50, 50),
    oob = scales::squish,
    name = expression(Delta~B~"(%)"),
    breaks = c(-50, -25, 0, 25, 50),
    labels = c("≤ -50%", -25, 0, 25, "≥ 50%")) +
  
  
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = "Relative change in total biomass between current and future periods",
    fill = expression(Delta~B~"(%)")
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
  here(paste0("figures/spatial_biomass/total_biomass/Delta_B_future_present.png")),
  plot = map_effect_biomass,
  dpi = 300,
  width = 10,
  height = 6
)

# Plot 2: catch change before MPAs
map_effect_biomass_with_mpas <- ggplot(Spatial_B_all_scenarios) +
  geom_sf(aes(fill = delta_B_mpa), color = NA) +  # remove border lines for a cleaner look
  geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +
  
  scale_fill_gradient2(
    low = "deepskyblue4",
    mid = "lightyellow",
    high = "darkred",
    midpoint = 0,
    limits = c(-50, 50),
    oob = scales::squish,
    name = expression(Delta~B~"(%)"),
    breaks = c(-50, -25, 0, 25, 50),
    labels = c("≤ -50%", -25, 0, 25, "≥ 50%")) +
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = paste0("Change in biomass with climate change and 10% MPAs - ", sp_name) ,
    fill = expression(Delta~B~"(%)")
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
  here(paste0("figures/spatial_biomass/total_biomass/Delta_B_future_present_MPAs_all_scens_mean_6_scens.png")),
  plot = map_effect_biomass_with_mpas,
  dpi = 300,
  width = 10,
  height = 6
)

# Plot 3: diff delta
common_min = min(Spatial_B_all_scenarios$diff_delta, na.rm = T)
common_max = max(Spatial_B_all_scenarios$diff_delta, na.rm = T)

map_mpa_effect_biomass <- ggplot(Spatial_B_all_scenarios) +
  
  geom_sf(aes(fill = diff_delta), color = NA) +  # remove border lines for a cleaner look
  geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +
  
  # scale_fill_gradient2(
  #   low = "darkgreen",     
  #   mid = "lightyellow",      
  #   high = "purple",         
  #   midpoint = 0,
  #   trans = "pseudo_log",
  #   limits = c(common_min, common_max),
  #   name = expression(Delta~B~"(%)")  # nicer label formatting
  # ) 
  scale_fill_gradient2(
    low = "darkgreen",
    mid = "lightyellow",
    high = "purple",
    midpoint = 0,
    limits = c(-10, 10),
    oob = scales::squish,
    name = expression(Delta~B~"(%)"),
    breaks = c(-10, -5, 0, 5, 10),
    labels = c("≤ -10%", -5, 0, 5, "≥ 10%")) +
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = "MPA impact of biomass change",
    fill = expression(Delta~B~"(%)")
  ) +
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = paste0("Diff delta - ", sp_name),
    fill = expression(Delta~B~"(%)")
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
  here(paste0("figures/spatial_biomass/total_biomass/MPA_effect_total_biomass_all_scens_mean_6_scens.png")),
  plot = map_mpa_effect_biomass,
  dpi = 300,
  width = 10,
  height = 6
)

