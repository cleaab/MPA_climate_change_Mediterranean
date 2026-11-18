# Load price dataset
price_kg = read.csv(here("data-mpa-CC-paper/Price_data_updated_without_outliers.csv"))

# LANDED VALUE - for all species by cell

spatial_catch_value_CC <- here(
  "data-mpa-CC-paper/model_outputs/spatial_catch_value/landed_catch_value_all_species_future_nompa.gpkg")

if (file.exists(spatial_catch_value_CC)) {
  message("File exists — skipping: ", spatial_catch_value_CC)
} else {
  # future -----
  nc = nc_open(here("data-mpa-CC-paper/model_outputs/spatial_nompa/Yield_nompa_scenario_CC.nc"))
  yield_nompa_future <- ncvar_get(nc, varid = "Catches")
  yield_nompa_future <- apply(yield_nompa_future, c(1,2,3), mean, na.rm = T) # mean over reps
  
  
  nspecies = 101
  landed_value_all = c()
  species_names = c()
  for (i in 1:nspecies){
    yield_sp = yield_nompa_future[1:190, 1:83, i]
    
    yield_sp2 = yield_sp[,83:1]
    yield_sp_vect = as.vector(yield_sp2)
    nrow(Med_grid_GSA) == length(yield_sp_vect) 
    
    price_kg_sp = price_kg %>%
      filter(species_id == i) %>%
      dplyr::select(gsa, price) %>%
      rename(GSA = gsa)
    
    mean_price_Medsea_sp = mean(price_kg_sp$price, na.rm = T)
    
    Med_grid_sp = Med_grid_GSA %>%
      st_drop_geometry() %>%
      mutate(cell_ID = as.numeric(cell_ID)) %>%
      arrange(cell_ID) %>%
      mutate(catch_tonnes = yield_sp_vect) %>%
      left_join(price_kg_sp, by = "GSA") %>%
      mutate(catch_kg = catch_tonnes * 10^6) %>%
      
      # if no price/kg for that gsa because landed value was not specified, then consider average Med price
      mutate(landed_value = case_when(!is.na(price) ~ catch_kg * price,
                                      TRUE ~ catch_kg * mean_price_Medsea_sp)) %>%
      mutate(sp = i)
    
    print(table(Med_grid_sp$landed_value[Med_grid_sp$VALUE == -99]))
    
    if (i == 1){
      Med_grid_sp_all = Med_grid_sp
    } else{
      Med_grid_sp_all = rbind(Med_grid_sp_all, Med_grid_sp)
      
    }
    
  }
  
  landed_value_all_nompa_future = Med_grid_sp_all %>%
    #st_drop_geometry() %>%
    group_by(cell_ID, VALUE, GSA_name, GSA) %>%
    summarize(landed_value = sum(landed_value, na.rm = T)) 
  
  sum(landed_value_all_nompa_future$landed_value[landed_value_all_nompa_future$VALUE == -99])
  sum(landed_value_all_nompa_future$landed_value[landed_value_all_nompa_future$VALUE == 0])
  
  
  st_write(landed_value_all_nompa_future, here("data-mpa-CC-paper/model_outputs/spatial_catch_value/landed_catch_value_all_species_future_nompa.gpkg"))
  
}

# PRESENT - NOMPAs

spatial_catch_value_noCC <- here(
  "data-mpa-CC-paper/model_outputs/spatial_catch_value/landed_catch_value_all_species_present_nompa.gpkg")

if (file.exists(spatial_catch_value_noCC)) {
  message("File exists — skipping: ", spatial_catch_value_noCC)
} else {
  # present -----
  nc = nc_open(here("data-mpa-CC-paper/model_outputs/spatial_nompa/Yield_nompa_scenario_noCC.nc"))
  yield_nompa_present <- ncvar_get(nc, varid = "Catches")
  yield_nompa_present <- apply(yield_nompa_present, c(1,2,3), mean, na.rm = T) # mean over reps
  
  
  
  nspecies = 101
  landed_value_all = c()
  species_names = c()
  for (i in 1:nspecies){
    yield_sp = yield_nompa_present[1:190, 1:83, i]
    
    yield_sp2 = yield_sp[,83:1]
    yield_sp_vect = as.vector(yield_sp2)
    nrow(Med_grid_GSA) == length(yield_sp_vect) 
    
    price_kg_sp = price_kg %>%
      filter(species_id == i) %>%
      dplyr::select(gsa, price) %>%
      rename(GSA = gsa)
    
    mean_price_Medsea_sp = mean(price_kg_sp$price, na.rm = T)
    
    Med_grid_sp = Med_grid_GSA %>%
      st_drop_geometry() %>%
      mutate(cell_ID = as.numeric(cell_ID)) %>%
      arrange(cell_ID) %>%
      mutate(catch_tonnes = yield_sp_vect) %>%
      left_join(price_kg_sp, by = "GSA") %>%
      mutate(catch_kg = catch_tonnes * 10^6) %>%
      
      # if no price/kg for that gsa because landed value was not specified, then consider average Med price
      mutate(landed_value = case_when(!is.na(price) ~ catch_kg * price,
                                      TRUE ~ catch_kg * mean_price_Medsea_sp)) %>%
      mutate(sp = i)
    
    print(table(Med_grid_sp$landed_value[Med_grid_sp$VALUE == -99]))
    
    
    if (i == 1){
      Med_grid_sp_all = Med_grid_sp
      
    } else{
      Med_grid_sp_all = rbind(Med_grid_sp_all, Med_grid_sp)
      
    }
    
  }
  
  landed_value_all_nompa_present = Med_grid_sp_all %>%
    #st_drop_geometry() %>%
    group_by(cell_ID, VALUE, GSA_name, GSA) %>%
    summarize(landed_value = sum(landed_value, na.rm = T)) 
  
  sum(landed_value_all_nompa_present$landed_value[landed_value_all_nompa_present$VALUE == -99])
  sum(landed_value_all_nompa_present$landed_value[landed_value_all_nompa_present$VALUE == 0])
  
  
  st_write(landed_value_all_nompa_present, here("data-mpa-CC-paper/model_outputs/spatial_catch_value/landed_catch_value_all_species_present_nompa.gpkg"))
  
}



# with mpas in the future (all scens) ------------------------------------------

spatial_catch_value_CC_mpa <- here(
  "data-mpa-CC-paper/model_outputs/spatial_catch_value/catch_value_sum_species_future_mpa_all_scens.gpkg")

if (file.exists(spatial_catch_value_CC_mpa)) {
  message("File exists — skipping: ", spatial_catch_value_CC_mpa)
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
    
    # future with mpa ------
    nspecies = 101
    landed_value_all = c()
    species_names = c()
    cell_IDs = c()
    for (i in 1:nspecies){
      yield_sp = yield_mpa_future[1:190, 1:83, i]
      
      yield_sp2 = yield_sp[,83:1]
      yield_sp_vect = as.vector(yield_sp2)
      nrow(Med_grid_GSA) == length(yield_sp_vect) 
      
      price_kg_sp = price_kg %>%
        filter(species_id == i) %>%
        dplyr::select(gsa, price) %>%
        rename(GSA = gsa)
      
      mean_price_Medsea_sp = mean(price_kg_sp$price, na.rm = T)
      
      Med_grid_sp = Med_grid_GSA %>%
        st_drop_geometry() %>%
        mutate(cell_ID = as.numeric(cell_ID)) %>%
        arrange(cell_ID) %>%
        mutate(catch_tonnes = yield_sp_vect) %>%
        left_join(price_kg_sp, by = "GSA") %>%
        mutate(catch_kg = catch_tonnes * 10^6) %>%
        
        # if no price/kg for that gsa because landed value was not specified, then consider average Med price
        mutate(landed_value = case_when(!is.na(price) ~ catch_kg * price,
                                        TRUE ~ catch_kg * mean_price_Medsea_sp)) %>%
        mutate(sp = i)
      
      print(table(Med_grid_sp$landed_value[Med_grid_sp$VALUE == -99]))
      
      if (i == 1){
        Med_grid_sp_all = Med_grid_sp
        
      } else{
        Med_grid_sp_all = rbind(Med_grid_sp_all, Med_grid_sp)
        
      }
      
    }
    
    landed_value_all = Med_grid_sp_all %>%
      #st_drop_geometry() %>%
      group_by(cell_ID, VALUE, GSA_name, GSA) %>%
      summarize(landed_value = sum(landed_value, na.rm = T)) %>%
      mutate(mpa_scenario = mpa_scenario)
    
    sum(landed_value_all$landed_value[landed_value_all$VALUE == -99])
    sum(landed_value_all$landed_value[landed_value_all$VALUE == 0])
    
    
    if (mpa_scenario == mpa_scenarios[1]){
      landed_value_all_scens = landed_value_all
      
    } else{
      landed_value_all_scens = rbind(landed_value_all_scens, landed_value_all) 
      
    }
  }
  
  sum(landed_value_all_scens$landed_value[landed_value_all_scens$VALUE == -99])
  st_write(landed_value_all_scens, here("data-mpa-CC-paper/model_outputs/spatial_catch_value/catch_value_sum_species_future_mpa_all_scens.gpkg")) # all mpa scenarios
  
}


# Fig8B- Plot - CATCH VALUE - all species ------------------------------------------------

# Load countries shapefile
worldbounds = st_read(here("data-mpa-CC-paper/shapefiles/world_map/world-bounds/world-bounds-a.shp"))
worldbounds_3035 = st_transform(worldbounds, 3035)

# Define bounding box around the Mediterranean region in EPSG:3035
med_bbox <- st_as_sfc(st_bbox(c(xmin = 2880851, xmax = 6680851, ymin = 2515444, ymax = 865443.7), crs = 3035))

# Clip worldbounds to this bbox
med_bounds <- st_intersection(worldbounds_3035, med_bbox)


# Sum over species  ------

species_classif = read.csv(here("data-mpa-CC-paper/classification_species.csv")) %>%
  filter(! species_id %in% c(0,1,2))

sp_name = "all_species_but_amphihaline"

species_id_list = species_classif$species_id
species_name_list = species_classif$species_name

length(species_id_list)
print(species_name_list)

# NO MPA - FUTURE
#landed_value_by_cell_future = st_read(here("data/model_outputs/landed_value-groups/landed_value_all_species_future_nompa_last.gpkg"))
landed_value_by_cell_future = st_read(here("data-mpa-CC-paper/model_outputs/spatial_catch_value/landed_catch_value_all_species_future_nompa.gpkg")) %>%
  rename(V_nompa_future = landed_value)


sum(landed_value_by_cell_future$Y_nompa_present[landed_value_by_cell_future$VALUE == -99])

# NO MPA - PRESENT
#landed_value_by_cell_present = st_read(here("data/model_outputs/landed_value-groups/landed_value_all_species_present_nompa_last.gpkg"))
landed_value_by_cell_present = st_read(here("data-mpa-CC-paper/model_outputs/spatial_catch_value/landed_catch_value_all_species_present_nompa.gpkg")) %>%
  rename(V_nompa_present = landed_value)


sum(landed_value_by_cell_present$Y_nompa_present[landed_value_by_cell_present$VALUE == -99])


# MPA - FUTURE
landed_value_by_cell_future_mpa = st_read(here("data-mpa-CC-paper/model_outputs/spatial_catch_value/catch_value_sum_species_future_mpa_all_scens.gpkg")) %>%
  rename(V_mpa_future = landed_value)

table(landed_value_by_cell_future_mpa$Y_mpa_future[landed_value_by_cell_future_mpa$VALUE == -99])

### -----
# Plot difference by GSA
Spatial_catch_value_by_GSA = landed_value_by_cell_future_mpa %>%
  left_join(landed_value_by_cell_present, by = c("cell_ID", "VALUE", "GSA", "GSA_name")) %>%
  left_join(landed_value_by_cell_future, by = c("cell_ID", "VALUE", "GSA", "GSA_name")) %>%
  mutate(diff_V = V_nompa_future - V_nompa_present,
         delta_V = (V_nompa_future - V_nompa_present)/V_nompa_present * 100,
         diff_V_mpa = V_mpa_future - V_nompa_present,
         delta_V_mpa = (V_mpa_future - V_nompa_present)/V_nompa_present * 100) %>%
  
  mutate(delta_V = case_when(V_nompa_present == 0 & V_nompa_future == 0 ~ 0,
                                     TRUE ~ delta_V)) %>%
  
  mutate(diff_delta = delta_V_mpa - delta_V)

# average all 24 scenarios
Spatial_catch_value_mean = Spatial_catch_value_by_GSA %>%
  group_by(cell_ID, VALUE, GSA) %>%
  summarize(delta_V = mean(delta_V, na.rm = T),
            delta_V_mpa = mean(delta_V_mpa, na.rm = T),
            diff_delta = mean(diff_delta, na.rm = T))

# average first S2 and S3 and then 6 MPA scenarios --------------

# basin-wide change --
Catch_value_mean = Spatial_catch_value_by_GSA %>%
  mutate(mpa_scenario_group = case_when(grepl("EEZ", mpa_scenario) ~ "EEZ-conservation",
                                        grepl("Random", mpa_scenario) ~ "Random",
                                        TRUE ~ mpa_scenario)) %>%  
  # basin-wide sum
  group_by(mpa_scenario, mpa_scenario_group) %>%
  summarize(V_nompa_future = sum(V_nompa_future, na.rm = T),
            V_nompa_present = sum(V_nompa_present, na.rm = T),
            V_mpa_future = sum(V_mpa_future, na.rm = T)) %>%
  ungroup() %>%
  mutate(diff_V = V_nompa_future - V_nompa_present,
         delta_V = (V_nompa_future - V_nompa_present)/V_nompa_present * 100,
         diff_V_mpa = V_mpa_future - V_nompa_present,
         delta_V_mpa = (V_mpa_future - V_nompa_present)/V_nompa_present * 100) %>%
  
  mutate(delta_V = case_when(V_nompa_present == 0 & V_nompa_future == 0 ~ 0,
                             TRUE ~ delta_V)) %>%
  
  mutate(diff_delta = delta_V_mpa - delta_V) %>%
  
  group_by(mpa_scenario_group) %>%
  summarize(delta_V_6 = mean(delta_V, na.rm = T),
            delta_V_mpa_6 = mean(delta_V_mpa, na.rm = T),
            diff_delta_6 = mean(diff_delta, na.rm = T)) 

mean_diff_delta = mean(Catch_value_mean$diff_delta_6, na.rm = T)

print(mean_diff_delta)

sd_diff_delta = sd(Catch_value_mean$diff_delta_6, na.rm = T)

print(sd_diff_delta)
# for figures ---

Spatial_catch_value_mean = Spatial_catch_value_by_GSA %>%
  mutate(mpa_scenario_group = case_when(grepl("EEZ", mpa_scenario) ~ "EEZ-conservation",
                                        grepl("Random", mpa_scenario) ~ "Random",
                                        TRUE ~ mpa_scenario)) %>%  
  group_by(cell_ID, VALUE, GSA, mpa_scenario_group) %>%
  summarize(delta_V_6 = mean(delta_V, na.rm = T),
            delta_V_mpa_6 = mean(delta_V_mpa, na.rm = T),
            diff_delta_6 = mean(diff_delta, na.rm = T)) %>%
  ungroup() %>%
  group_by(cell_ID, VALUE, GSA) %>%
  summarize(delta_V = mean(delta_V_6, na.rm = T),
            delta_V_mpa = mean(delta_V_mpa_6, na.rm = T),
            diff_delta = mean(diff_delta_6, na.rm = T)) %>%
  ungroup() 

sum(Spatial_catch_value_mean$delta_V_mpa[Spatial_catch_value_mean$VALUE == -99])

Spatial_catch_value_all_scenarios = Med_grid_GSA_EEZ_df %>%
  mutate(cell_ID = as.numeric(cell_ID)) %>%
  arrange(cell_ID) %>%
  left_join(Spatial_catch_value_mean, by = c("cell_ID", "GSA", "VALUE")) %>%
  mutate(delta_V_mpa = case_when(VALUE == 0 & is.na(delta_V_mpa) ~ 0,
                                 TRUE ~ delta_V_mpa),
         delta_V = case_when(VALUE == -99 ~ NA,
                             TRUE ~ delta_V),
         diff_delta = case_when(VALUE == 0 & is.na(diff_delta) ~ 0,
                                TRUE ~ diff_delta)) # so no NANs in the Med and NAN in other seas

sum(Spatial_catch_value_all_scenarios$delta_V_mpa[Spatial_catch_value_all_scenarios$VALUE == -99])

#### RELATIVE CHANGE - calculate average Y accross scenarios by GSA and then calculate delta Y on the average

all_vals <- c(Spatial_catch_value_all_scenarios$delta_V,
              Spatial_catch_value_all_scenarios$delta_V_mpa)

common_min <- min(all_vals, na.rm = TRUE)
common_max <- max(all_vals, na.rm = TRUE)

# Plot 1: MPA effect catch change
map_catch_value <- ggplot(Spatial_catch_value_all_scenarios) +
  geom_sf(aes(fill = delta_V), color = NA) +  # remove border lines for a cleaner look
  geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +
  
  scale_fill_gradient2(
    low = "darkred",
    mid = "lightyellow",
    high = "deepskyblue4",
    midpoint = 0,
    limits = c(-30, 30),
    oob = scales::squish,
    name = expression(Delta~V~"(%)"),
    breaks = c(-30, -15, 0, 15, 30),
    labels = c("≤ -30%", -15, 0, 15, "≥ 30%")) +
  
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = paste0("Mean change in landed value with climate change -", sp_name),
    fill = expression(Delta~V~"(%)")
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

print(map_catch_value)
# Save the plot
ggsave(
  here(paste0("figures/after_revision1/FINAL/Delta_V_without_MPAs_", sp_name, "_mean_6_scens.png")),
  plot = map_catch_value,
  dpi = 300,
  width = 10,
  height = 6
)

# Plot 2: catch change before MPAs
map_catch_value_mpa <- ggplot(Spatial_catch_value_all_scenarios) +
  geom_sf(aes(fill = delta_V_mpa), color = NA) +  # remove border lines for a cleaner look
  geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +
  
  scale_fill_gradient2(
    low = "darkred",
    mid = "lightyellow",
    high = "deepskyblue4",
    midpoint = 0,
    limits = c(-30, 30),
    oob = scales::squish,
    name = expression(Delta~V~"(%)"),
    breaks = c(-30, -15, 0, 15, 30),
    labels = c("≤ -30%", -15, 0, 15, "≥ 30%")) +
  
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = paste0("Change in landed value with climate change and 10% MPAs - ", sp_name) ,
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

print(map_catch_value_mpa)
# Save the plot
ggsave(
  here(paste0("figures/after_revision1/FINAL/Delta_V_with_MPAs_", sp_name, "_mean_6_scens.png")),
  plot = map_catch_value_mpa,
  dpi = 300,
  width = 10,
  height = 6
)

# Plot 3: diff delta
common_min = min(Spatial_catch_value_all_scenarios$diff_delta, na.rm = T)
common_max = max(Spatial_catch_value_all_scenarios$diff_delta, na.rm = T)
map_effect_mpas_catch_value <- ggplot(Spatial_catch_value_all_scenarios) +
  geom_sf(aes(fill = diff_delta), color = NA) +  # remove border lines for a cleaner look
  geom_sf(data = med_bounds, fill = "gray80", color = "grey40", size = 0.3) +
  

scale_fill_gradient2(
  low = "darkgreen",
  mid = "lightyellow",
  high = "purple",
  midpoint = 0,
  limits = c(-10, 10),
  oob = scales::squish,
  name = expression(Delta~V~"(%)"),
  breaks = c(-10, -5, 0, 5, 10),
  labels = c("≤ -10%", -5, 0, 5, "≥ 10%")) +
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = "MPA impact of yield change",
    fill = expression(Delta~V~"(%)")
  ) +
  coord_sf(expand = FALSE) +  # remove whitespace around the map
  labs(
    title = paste0("Diff delta - ", sp_name),
    fill = expression(Delta~V~"(%)")
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

print(map_effect_mpas_catch_value)
# Save the plot
ggsave(
  here(paste0("figures/after_revision1/FINAL/Diff_delta_V_with_MPAs_", sp_name, "_mean_6_scens.png")),
  plot = map_effect_mpas_catch_value,
  dpi = 300,
  width = 10,
  height = 6
)
