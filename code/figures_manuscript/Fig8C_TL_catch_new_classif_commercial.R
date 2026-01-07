# B2c. Trophic level of landings weighted by the proportion of catch of each specie - HC_C species
# All species (for manuscript text) ---
fig_name = "All_species"
species_classif = read.csv(here("data/classification_species_commercial_class_4_Jan_2026.csv")) %>%
  filter(! species_id %in% c(0,1,2))

species_name_selected = species_classif$species_name
species_id_selected = species_classif$species_id

TL_landings_current = read.csv(here("data/model_outputs/TL_landings/TL_landings_NoCC_GSA-prop_11-07-2023.csv")) %>%
  filter(species_name %in% species_name_selected)

Yield_prop_current = read.csv(here("data/model_outputs/Biomass_yield_by_species_NoCC_GSA-prop_11-07-2023.csv")) %>%
  dplyr::select(-biomass) %>%
  rename(species_id = sp_id,
         mpa_coverage = mpa.coverage) %>%
  filter(species_id %in% species_id_selected) %>%
  left_join(species_classif, by = "species_id")

TL_landings_weighted_current = Yield_prop_current %>%
  left_join(TL_landings_current, by = c("species_name", "mpa_coverage", "scenario"))

TL_landings_current_lit = TL_landings_weighted_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T))

TL_landings_baseline_NoMPA = unique(TL_landings_current_lit$TL_landings_all[TL_landings_current_lit$mpa_coverage == 0])

TL_landings_current_lit = TL_landings_weighted_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(TL_change = (TL_landings_all - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(climate_scenario = "2006-2013")


TL_landings_current_Random = TL_landings_weighted_current %>%
  filter(grepl('Random', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_landings_all),
         low = min(TL_landings_all),
         mean = mean(TL_landings_all)) %>%
  ungroup() %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(high_change = (high - TL_landings_baseline)/TL_landings_baseline * 100,
         low_change = (low - TL_landings_baseline)/TL_landings_baseline * 100,
         mean_change = (mean - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(scenario_group = "Random") %>%
  mutate(climate_scenario = "2006-2013")


TL_landings_current_EEZ = TL_landings_weighted_current %>%
  filter(grepl('EEZ', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_landings_all),
         low = min(TL_landings_all),
         mean = mean(TL_landings_all)) %>%
  ungroup() %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(high_change = (high - TL_landings_baseline)/TL_landings_baseline * 100,
         low_change = (low - TL_landings_baseline)/TL_landings_baseline * 100,
         mean_change = (mean - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(scenario_group = "EEZ-conservation") %>%
  mutate(climate_scenario = "2006-2013")

# Future scenario ----

TL_landings_future = read.csv(here("data/model_outputs/TL_landings/TL_landings_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  filter(species_name %in% species_name_selected)

Yield_prop_future = read.csv(here("data/model_outputs/Biomass_yield_by_species_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  dplyr::select(-biomass) %>%
  rename(species_id = sp_id,
         mpa_coverage = mpa.coverage) %>%
  filter(species_id %in% species_id_selected) %>%
  left_join(species_classif, by = "species_id")

TL_landings_weighted_future = Yield_prop_future %>%
  left_join(TL_landings_future, by = c("species_name", "mpa_coverage", "scenario"))


TL_landings_future_lit = TL_landings_weighted_future %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(TL_change = (TL_landings_all - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(climate_scenario = "2070-2100")

TL_landings_future_Random = TL_landings_weighted_future %>%
  filter(grepl('Random', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_landings_all),
         low = min(TL_landings_all),
         mean = mean(TL_landings_all)) %>%
  ungroup() %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(high_change = (high - TL_landings_baseline)/TL_landings_baseline * 100,
         low_change = (low - TL_landings_baseline)/TL_landings_baseline * 100,
         mean_change = (mean - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(scenario_group = "Random") %>%
  mutate(climate_scenario = "2070-2100")


TL_landings_future_EEZ = TL_landings_weighted_future %>%
  filter(grepl('EEZ', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_landings_all),
         low = min(TL_landings_all),
         mean = mean(TL_landings_all)) %>%
  ungroup() %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(high_change = (high - TL_landings_baseline)/TL_landings_baseline * 100,
         low_change = (low - TL_landings_baseline)/TL_landings_baseline * 100,
         mean_change = (mean - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(scenario_group = "EEZ-conservation") %>%
  mutate(climate_scenario = "2070-2100")





#   join by MPA scenario
TL_landings_lit = rbind(TL_landings_current_lit, TL_landings_future_lit) %>%
  filter(mpa_coverage < 11) %>%
  mutate(MPA_scenario = case_when(scenario == "Existing network" ~ "S1-Current",
                                  scenario == "Mazor scenario 8" ~ "S5-Mazor GFCM",
                                  scenario == "Mazor scenario 9" ~ "S6-Mazor SAUP",
                                  scenario == "Micheli" ~ "S4-Micheli",
                                  TRUE ~ scenario)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                      TRUE ~ "Baseline climate conditions"))

TL_landings_Random = rbind(TL_landings_current_Random, TL_landings_future_Random) %>%
  filter(mpa_coverage < 11) %>%
  mutate(MPA_scenario = case_when(scenario_group == "Random" ~ "S2-Random basin",
                                  TRUE ~ scenario_group)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                      TRUE ~ "Baseline climate conditions")) 


TL_landings_EEZ = rbind(TL_landings_current_EEZ, TL_landings_future_EEZ) %>%
  filter(mpa_coverage < 11) %>%
  mutate(MPA_scenario = case_when(scenario_group == "EEZ-conservation" ~ "S3-Random EEZ",
                                  TRUE ~ scenario_group)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                      TRUE ~ "Baseline climate conditions"))



### PLOT -
scenario <- c("solid", "dotted", "dotted", "dotdash", "twodash", "longdash")

colors <- c("cadetblue", "darkred")


# Entire Med Sea -------------------------------------

TL_landings_Med_future_present_all_species <- ggplot() +
  
  geom_ribbon(data = TL_landings_Random, aes(x = mpa_coverage, ymin = low_change, ymax = high_change, fill = climate_scenario), alpha = 0.3, show.legend = F) +
  
  geom_ribbon(data = TL_landings_EEZ, aes(x = mpa_coverage, ymin = low_change, ymax = high_change, fill = climate_scenario), alpha = 0.5, show.legend = F) +
  
  geom_line(data = TL_landings_EEZ, aes(x = mpa_coverage, y = mean_change, colour = climate_scenario, lty = MPA_scenario), lwd = 0.6, alpha=1) +
  
  geom_line(data = TL_landings_Random, aes(x = mpa_coverage, y = mean_change, colour = climate_scenario, lty = MPA_scenario), lwd = 0.4, alpha=1) +
  
  geom_line(data = TL_landings_lit, aes(x = mpa_coverage, y = TL_change, colour = climate_scenario, lty = MPA_scenario), lwd = 0.4, alpha=1) + 
  
  
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title = element_text(size = 14)) +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10)) + #change legend text font size 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_linetype_manual(values = scenario) +
  
  labs(x ="MPA coverage (%)", y = "Change in TL (%)", title = "C) TL catch", subtitle = paste0(fig_name)) +
  
  theme(legend.position = "right") +
  #theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = 16)) +
  ylim(c(-1.5,1.5))


print(TL_landings_Med_future_present_all_species)
# ggsave(here(paste0("figures/TL_landings/TL_landings_current_future_weighted_with_catches_", fig_name, ".png")), dpi = 600, height = 6, width = 6)
# ggsave(here(paste0("figures/TL_landings/TL_landings_current_future_weighted_with_catches_10pct_", fig_name, ".png")), dpi = 600, height = 6, width = 6)



# Commercial species ------
fig_name = "Species of high commercial interest"
species_classif = read.csv(here("data/classification_species_commercial_class_4_Jan_2026.csv")) %>%
  filter(commercial_class %in% c("HC", "C")) %>%
  filter(! species_id %in% c(0,1,2))

species_name_selected = species_classif$species_name
species_id_selected = species_classif$species_id

TL_landings_current = read.csv(here("data/model_outputs/TL_landings/TL_landings_NoCC_GSA-prop_11-07-2023.csv")) %>%
  filter(species_name %in% species_name_selected)

Yield_prop_current = read.csv(here("data/model_outputs/Biomass_yield_by_species_NoCC_GSA-prop_11-07-2023.csv")) %>%
  dplyr::select(-biomass) %>%
  rename(species_id = sp_id,
         mpa_coverage = mpa.coverage) %>%
  filter(species_id %in% species_id_selected) %>%
  left_join(species_classif, by = "species_id")

TL_landings_weighted_current = Yield_prop_current %>%
  left_join(TL_landings_current, by = c("species_name", "mpa_coverage", "scenario"))

TL_landings_current_lit = TL_landings_weighted_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T))

TL_landings_baseline_NoMPA = unique(TL_landings_current_lit$TL_landings_all[TL_landings_current_lit$mpa_coverage == 0])

TL_landings_current_lit = TL_landings_weighted_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(TL_change = (TL_landings_all - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(climate_scenario = "2006-2013")


TL_landings_current_Random = TL_landings_weighted_current %>%
  filter(grepl('Random', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_landings_all),
         low = min(TL_landings_all),
         mean = mean(TL_landings_all)) %>%
  ungroup() %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(high_change = (high - TL_landings_baseline)/TL_landings_baseline * 100,
         low_change = (low - TL_landings_baseline)/TL_landings_baseline * 100,
         mean_change = (mean - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(scenario_group = "Random") %>%
  mutate(climate_scenario = "2006-2013")


TL_landings_current_EEZ = TL_landings_weighted_current %>%
  filter(grepl('EEZ', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_landings_all),
         low = min(TL_landings_all),
         mean = mean(TL_landings_all)) %>%
  ungroup() %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(high_change = (high - TL_landings_baseline)/TL_landings_baseline * 100,
         low_change = (low - TL_landings_baseline)/TL_landings_baseline * 100,
         mean_change = (mean - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(scenario_group = "EEZ-conservation") %>%
  mutate(climate_scenario = "2006-2013")

# Future scenario ----

TL_landings_future = read.csv(here("data/model_outputs/TL_landings/TL_landings_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  filter(species_name %in% species_name_selected)

Yield_prop_future = read.csv(here("data/model_outputs/Biomass_yield_by_species_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  dplyr::select(-biomass) %>%
  rename(species_id = sp_id,
         mpa_coverage = mpa.coverage) %>%
  filter(species_id %in% species_id_selected) %>%
  left_join(species_classif, by = "species_id")

TL_landings_weighted_future = Yield_prop_future %>%
  left_join(TL_landings_future, by = c("species_name", "mpa_coverage", "scenario"))


TL_landings_future_lit = TL_landings_weighted_future %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(TL_change = (TL_landings_all - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(climate_scenario = "2070-2100")

TL_landings_future_Random = TL_landings_weighted_future %>%
  filter(grepl('Random', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_landings_all),
         low = min(TL_landings_all),
         mean = mean(TL_landings_all)) %>%
  ungroup() %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(high_change = (high - TL_landings_baseline)/TL_landings_baseline * 100,
         low_change = (low - TL_landings_baseline)/TL_landings_baseline * 100,
         mean_change = (mean - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(scenario_group = "Random") %>%
  mutate(climate_scenario = "2070-2100")


TL_landings_future_EEZ = TL_landings_weighted_future %>%
  filter(grepl('EEZ', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_landings_all),
         low = min(TL_landings_all),
         mean = mean(TL_landings_all)) %>%
  ungroup() %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(high_change = (high - TL_landings_baseline)/TL_landings_baseline * 100,
         low_change = (low - TL_landings_baseline)/TL_landings_baseline * 100,
         mean_change = (mean - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(scenario_group = "EEZ-conservation") %>%
  mutate(climate_scenario = "2070-2100")





#   join by MPA scenario
TL_landings_lit = rbind(TL_landings_current_lit, TL_landings_future_lit) %>%
  filter(mpa_coverage < 11) %>%
  mutate(MPA_scenario = case_when(scenario == "Existing network" ~ "S1-Current",
                                  scenario == "Mazor scenario 8" ~ "S5-Mazor GFCM",
                                  scenario == "Mazor scenario 9" ~ "S6-Mazor SAUP",
                                  scenario == "Micheli" ~ "S4-Micheli",
                                  TRUE ~ scenario)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                      TRUE ~ "Baseline climate conditions"))

TL_landings_Random = rbind(TL_landings_current_Random, TL_landings_future_Random) %>%
  filter(mpa_coverage < 11) %>%
  mutate(MPA_scenario = case_when(scenario_group == "Random" ~ "S2-Random basin",
                                  TRUE ~ scenario_group)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                      TRUE ~ "Baseline climate conditions")) 


TL_landings_EEZ = rbind(TL_landings_current_EEZ, TL_landings_future_EEZ) %>%
  filter(mpa_coverage < 11) %>%
  mutate(MPA_scenario = case_when(scenario_group == "EEZ-conservation" ~ "S3-Random EEZ",
                                  TRUE ~ scenario_group)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                      TRUE ~ "Baseline climate conditions"))

# basin-wide change across scenarios
TL_landings_lit_10 = TL_landings_lit %>%
  filter(mpa_coverage == 10 & climate_scenario == "Future climate conditions") %>%
  dplyr::select(MPA_scenario, TL_change)

TL_landings_random_10 = TL_landings_Random %>%
  filter(mpa_coverage == 10 & climate_scenario == "Future climate conditions") %>%
  group_by(MPA_scenario) %>%
  summarize(TL_change = mean(mean_change))

TL_landings_EEZ_10 = TL_landings_EEZ %>%
  filter(mpa_coverage == 10 & climate_scenario == "Future climate conditions") %>%
  group_by(MPA_scenario) %>%
  summarize(TL_change = mean(mean_change))

TL_landing_nompa = unique(TL_landings_lit$TL_change[TL_landings_lit$mpa_coverage == 0 & TL_landings_lit$climate_scenario == "Future climate conditions"])
print(TL_landing_nompa)

TL_landings_all_scenarios_10 = rbind(TL_landings_lit_10, TL_landings_random_10, TL_landings_EEZ_10) %>%
  rename(TL_change_mpa = TL_change) %>%
  mutate(TL_change_nompa = TL_landing_nompa) %>%
  mutate(diff_delta = TL_change_mpa - TL_change_nompa)

mean_MPA_effect = mean(TL_landings_all_scenarios_10$diff_delta)
print(mean_MPA_effect)
sd_MPA_effect = sd(TL_landings_all_scenarios_10$diff_delta)
print(sd_MPA_effect)

### PLOT -
scenario <- c("solid", "dotted", "dotted", "dotdash", "twodash", "longdash")

colors <- c("cadetblue", "darkred")


# Entire Med Sea -------------------------------------

TL_landings_Med_future_present_HC_C <- ggplot() +
  
  geom_ribbon(data = TL_landings_Random, aes(x = mpa_coverage, ymin = low_change, ymax = high_change, fill = climate_scenario), alpha = 0.3, show.legend = F) +
  
  geom_ribbon(data = TL_landings_EEZ, aes(x = mpa_coverage, ymin = low_change, ymax = high_change, fill = climate_scenario), alpha = 0.5, show.legend = F) +
  
  geom_line(data = TL_landings_EEZ, aes(x = mpa_coverage, y = mean_change, colour = climate_scenario, lty = MPA_scenario), lwd = 0.6, alpha=1) +
  
  geom_line(data = TL_landings_Random, aes(x = mpa_coverage, y = mean_change, colour = climate_scenario, lty = MPA_scenario), lwd = 0.4, alpha=1) +
  
  geom_line(data = TL_landings_lit, aes(x = mpa_coverage, y = TL_change, colour = climate_scenario, lty = MPA_scenario), lwd = 0.4, alpha=1) + 
  
  
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title = element_text(size = 14)) +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10)) + #change legend text font size 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_linetype_manual(values = scenario) +
  
  labs(x ="MPA coverage (%)", y = "Change in TL (%)", title = "C) TL catch", subtitle = paste0(fig_name)) +
  
  theme(legend.position = "right") +
  #theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = 16)) +
  ylim(c(-1.5,1.5))


print(TL_landings_Med_future_present_HC_C)
# ggsave(here(paste0("figures/TL_landings/TL_landings_current_future_weighted_with_catches_", fig_name, ".png")), dpi = 600, height = 6, width = 6)
# ggsave(here(paste0("figures/TL_landings/TL_landings_current_future_weighted_with_catches_10pct_", fig_name, ".png")), dpi = 600, height = 6, width = 6)


# Low commercial species ---------------------
fig_name = "Species of minor commercial interest"
species_classif = read.csv(here("data/classification_species_commercial_class_4_Jan_2026.csv")) %>%
  filter(commercial_class %in% c("LC", "NC")) %>%
  filter(! species_id %in% c(0,1,2))

species_name_selected = species_classif$species_name
species_id_selected = species_classif$species_id

TL_landings_current = read.csv(here("data/model_outputs/TL_landings/TL_landings_NoCC_GSA-prop_11-07-2023.csv")) %>%
  filter(species_name %in% species_name_selected)

Yield_prop_current = read.csv(here("data/model_outputs/Biomass_yield_by_species_NoCC_GSA-prop_11-07-2023.csv")) %>%
  dplyr::select(-biomass) %>%
  rename(species_id = sp_id,
         mpa_coverage = mpa.coverage) %>%
  filter(species_id %in% species_id_selected) %>%
  left_join(species_classif, by = "species_id")

TL_landings_weighted_current = Yield_prop_current %>%
  left_join(TL_landings_current, by = c("species_name", "mpa_coverage", "scenario"))

TL_landings_current_lit = TL_landings_weighted_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T))

TL_landings_baseline_NoMPA = unique(TL_landings_current_lit$TL_landings_all[TL_landings_current_lit$mpa_coverage == 0])

TL_landings_current_lit = TL_landings_weighted_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(TL_change = (TL_landings_all - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(climate_scenario = "2006-2013")


TL_landings_current_Random = TL_landings_weighted_current %>%
  filter(grepl('Random', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_landings_all),
         low = min(TL_landings_all),
         mean = mean(TL_landings_all)) %>%
  ungroup() %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(high_change = (high - TL_landings_baseline)/TL_landings_baseline * 100,
         low_change = (low - TL_landings_baseline)/TL_landings_baseline * 100,
         mean_change = (mean - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(scenario_group = "Random") %>%
  mutate(climate_scenario = "2006-2013")


TL_landings_current_EEZ = TL_landings_weighted_current %>%
  filter(grepl('EEZ', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_landings_all),
         low = min(TL_landings_all),
         mean = mean(TL_landings_all)) %>%
  ungroup() %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(high_change = (high - TL_landings_baseline)/TL_landings_baseline * 100,
         low_change = (low - TL_landings_baseline)/TL_landings_baseline * 100,
         mean_change = (mean - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(scenario_group = "EEZ-conservation") %>%
  mutate(climate_scenario = "2006-2013")

# Future scenario ----

TL_landings_future = read.csv(here("data/model_outputs/TL_landings/TL_landings_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  filter(species_name %in% species_name_selected)

Yield_prop_future = read.csv(here("data/model_outputs/Biomass_yield_by_species_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  dplyr::select(-biomass) %>%
  rename(species_id = sp_id,
         mpa_coverage = mpa.coverage) %>%
  filter(species_id %in% species_id_selected) %>%
  left_join(species_classif, by = "species_id")

TL_landings_weighted_future = Yield_prop_future %>%
  left_join(TL_landings_future, by = c("species_name", "mpa_coverage", "scenario"))



TL_landings_future_lit = TL_landings_weighted_future %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(TL_change = (TL_landings_all - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(climate_scenario = "2070-2100")

TL_landings_future_Random = TL_landings_weighted_future %>%
  filter(grepl('Random', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_landings_all),
         low = min(TL_landings_all),
         mean = mean(TL_landings_all)) %>%
  ungroup() %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(high_change = (high - TL_landings_baseline)/TL_landings_baseline * 100,
         low_change = (low - TL_landings_baseline)/TL_landings_baseline * 100,
         mean_change = (mean - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(scenario_group = "Random") %>%
  mutate(climate_scenario = "2070-2100")


TL_landings_future_EEZ = TL_landings_weighted_future %>%
  filter(grepl('EEZ', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_yield = sum(yield, na.rm = T),
         prop_yield = yield/total_yield,
         TL_landings_weighted = TL_landings * prop_yield) %>%
  summarise(TL_landings_all = sum(TL_landings_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_landings_all),
         low = min(TL_landings_all),
         mean = mean(TL_landings_all)) %>%
  ungroup() %>%
  mutate(TL_landings_baseline = TL_landings_baseline_NoMPA) %>%
  mutate(high_change = (high - TL_landings_baseline)/TL_landings_baseline * 100,
         low_change = (low - TL_landings_baseline)/TL_landings_baseline * 100,
         mean_change = (mean - TL_landings_baseline)/TL_landings_baseline * 100) %>%
  mutate(scenario_group = "EEZ-conservation") %>%
  mutate(climate_scenario = "2070-2100")

#   join by MPA scenario
TL_landings_lit = rbind(TL_landings_current_lit, TL_landings_future_lit) %>%
  filter(mpa_coverage < 11) %>%
  mutate(MPA_scenario = case_when(scenario == "Existing network" ~ "S1-Current",
                                  scenario == "Mazor scenario 8" ~ "S5-Mazor GFCM",
                                  scenario == "Mazor scenario 9" ~ "S6-Mazor SAUP",
                                  scenario == "Micheli" ~ "S4-Micheli",
                                  TRUE ~ scenario)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                      TRUE ~ "Baseline climate conditions"))
  
TL_landings_Random = rbind(TL_landings_current_Random, TL_landings_future_Random) %>%
  filter(mpa_coverage < 11) %>%
  mutate(MPA_scenario = case_when(scenario_group == "Random" ~ "S2-Random basin",
                                  TRUE ~ scenario_group)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                      TRUE ~ "Baseline climate conditions")) 

TL_landings_EEZ = rbind(TL_landings_current_EEZ, TL_landings_future_EEZ) %>%
  filter(mpa_coverage < 11) %>%
  mutate(MPA_scenario = case_when(scenario_group == "EEZ-conservation" ~ "S3-Random EEZ",
                                  TRUE ~ scenario_group)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                      TRUE ~ "Baseline climate conditions"))



### PLOT -
colors <- c("cadetblue", "darkred")


# Entire Med Sea -------------------------------------

TL_landings_Med_future_present_LC <- ggplot() +
  
  geom_ribbon(data = TL_landings_Random, aes(x = mpa_coverage, ymin = low_change, ymax = high_change, fill = climate_scenario), alpha = 0.3, show.legend = F) +
  
  geom_ribbon(data = TL_landings_EEZ, aes(x = mpa_coverage, ymin = low_change, ymax = high_change, fill = climate_scenario), alpha = 0.5, show.legend = F) +
  
  geom_line(data = TL_landings_EEZ, aes(x = mpa_coverage, y = mean_change, colour = climate_scenario, lty = MPA_scenario), lwd = 0.6, alpha=1) +
  
  geom_line(data = TL_landings_Random, aes(x = mpa_coverage, y = mean_change, colour = climate_scenario, lty = MPA_scenario), lwd = 0.4, alpha=1) +
  
  geom_line(data = TL_landings_lit, aes(x = mpa_coverage, y = TL_change, colour = climate_scenario, lty = MPA_scenario), lwd = 0.4, alpha=1) + 
  
  
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title = element_text(size = 14)) +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10)) + #change legend text font size 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_linetype_manual(values = scenario) +
  
  labs(x ="MPA coverage (%)", y = "Change in TL (%)", title = "", subtitle = paste0(fig_name)) +
  
  theme(legend.position = "right") +
  #theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = 16)) +
  ylim(c(-1.5,1.5))

print(TL_landings_Med_future_present_LC)
# Both plots together:
library(ggpubr)
TL_catch <- ggarrange(TL_landings_Med_future_present_HC_C, TL_landings_Med_future_present_LC,
                           ncol=2,
                           nrow=1,
                           common.legend = T,
                           legend = "right",
                           align = "hv") 

print(TL_catch)
#ggsave(here("figures/TL_landings_HC_C_LC_relative_change_for_paper.png"), dpi = 600, height = 6, width = 10)
ggsave(here("figures/TL_landings_HC_C_LC_relative_change_for_paper_v3_updated_classif_4_Jan_2026.png"), dpi = 600, height = 6, width = 10)

