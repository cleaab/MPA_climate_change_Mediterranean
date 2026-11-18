# B2d. Trophic level of the community weighted by the proportion of biomass of each specie

# import classification of species by groups
vertical_distrib = read.csv(here("data-mpa-CC-paper/classification_species.csv")) %>%
  dplyr::select(species_name, group) %>%
  rownames_to_column("species_id") %>%
  mutate(species_id = as.numeric(species_id) - 1)

vertical_distrib_noamphihaline = vertical_distrib %>%
  filter(! species_id %in% c(0,1,2))

TL_community_current = read.csv(here("data-mpa-CC-paper/model_outputs/TL_community/TL_community_NoCC_GSA-prop_11-07-2023.csv")) %>%
  filter(! species_name %in% c("Alosaalosa", "Alosafallax", "Anguillaanguilla"))

Biomass_prop_current = read.csv(here("data-mpa-CC-paper/model_outputs/Biomass_yield_by_species_NoCC_GSA-prop_11-07-2023.csv")) %>%
  dplyr::select(-yield) %>%
  rename(species_id = sp_id,
         mpa_coverage = mpa.coverage) %>%
  filter(! species_id %in% c(0, 1, 2)) %>%
  left_join(vertical_distrib_noamphihaline, by = "species_id")

TL_community_weighted_current = Biomass_prop_current %>%
  left_join(TL_community_current, by = c("species_name", "mpa_coverage", "scenario"))

TL_community_current_lit = TL_community_weighted_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_biomass = sum(biomass, na.rm = T),
         prop_biomass = biomass/total_biomass,
         TL_community_weighted = TL_community * prop_biomass) %>%
  summarise(TL_community_all = sum(TL_community_weighted, na.rm = T))

TL_baseline_nompa = unique(TL_community_current_lit$TL_community_all[TL_community_current_lit$mpa_coverage == 0])

TL_community_current_lit = TL_community_weighted_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_biomass = sum(biomass, na.rm = T),
         prop_biomass = biomass/total_biomass,
         TL_community_weighted = TL_community * prop_biomass) %>%
  summarise(TL_community_all = sum(TL_community_weighted, na.rm = T)) %>%
  mutate(TL_baseline = TL_baseline_nompa) %>%
  mutate(TL_change = (TL_community_all - TL_baseline)/TL_baseline * 100) %>%
  mutate(climate_scenario = "2006-2013")


TL_community_current_Random = TL_community_weighted_current %>%
  filter(grepl('Random', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_biomass = sum(biomass, na.rm = T),
         prop_biomass = biomass/total_biomass,
         TL_community_weighted = TL_community * prop_biomass) %>%
  summarise(TL_community_all = sum(TL_community_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_community_all),
         low = min(TL_community_all),
         mean = mean(TL_community_all)) %>%
  ungroup() %>%
  mutate(TL_baseline = TL_baseline_nompa) %>%
  mutate(high_change = (high - TL_baseline)/TL_baseline * 100,
         low_change = (low - TL_baseline)/TL_baseline * 100,
         mean_change = (mean - TL_baseline)/TL_baseline * 100) %>%
  mutate(scenario_group = "Random") %>%
  mutate(climate_scenario = "2006-2013")


TL_community_current_EEZ = TL_community_weighted_current %>%
  filter(grepl('EEZ', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_biomass = sum(biomass, na.rm = T),
         prop_biomass = biomass/total_biomass,
         TL_community_weighted = TL_community * prop_biomass) %>%
  summarise(TL_community_all = sum(TL_community_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_community_all),
         low = min(TL_community_all),
         mean = mean(TL_community_all)) %>%
  ungroup() %>%
  mutate(TL_baseline = TL_baseline_nompa) %>%
  mutate(high_change = (high - TL_baseline)/TL_baseline * 100,
         low_change = (low - TL_baseline)/TL_baseline * 100,
         mean_change = (mean - TL_baseline)/TL_baseline * 100) %>%
  mutate(scenario_group = "EEZ-conservation") %>%
  mutate(climate_scenario = "2006-2013")

# Future scenario ----

TL_community_future = read.csv(here("data-mpa-CC-paper/model_outputs/TL_community/TL_community_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  filter(! species_name %in% c("Alosaalosa", "Alosafallax", "Anguillaanguilla"))

Biomass_prop_future = read.csv(here("data-mpa-CC-paper/model_outputs/Biomass_yield_by_species_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  dplyr::select(-yield) %>%
  rename(species_id = sp_id,
         mpa_coverage = mpa.coverage) %>%
  filter(! species_id %in% c(0, 1, 2)) %>%
  left_join(vertical_distrib_noamphihaline, by = "species_id")

TL_community_weighted_future = Biomass_prop_future %>%
  left_join(TL_community_future, by = c("species_name", "mpa_coverage", "scenario"))



TL_community_future_lit = TL_community_weighted_future %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_biomass = sum(biomass, na.rm = T),
         prop_biomass = biomass/total_biomass,
         TL_community_weighted = TL_community * prop_biomass) %>%
  summarise(TL_community_all = sum(TL_community_weighted, na.rm = T)) %>%
  mutate(TL_baseline = TL_baseline_nompa) %>%
  mutate(TL_change = (TL_community_all - TL_baseline)/TL_baseline * 100) %>%
  mutate(climate_scenario = "2070-2100")

TL_community_future_Random = TL_community_weighted_future %>%
  filter(grepl('Random', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_biomass = sum(biomass, na.rm = T),
         prop_biomass = biomass/total_biomass,
         TL_community_weighted = TL_community * prop_biomass) %>%
  summarise(TL_community_all = sum(TL_community_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_community_all),
         low = min(TL_community_all),
         mean = mean(TL_community_all)) %>%
  ungroup() %>%
  mutate(TL_baseline = TL_baseline_nompa) %>%
  mutate(high_change = (high - TL_baseline)/TL_baseline * 100,
         low_change = (low - TL_baseline)/TL_baseline * 100,
         mean_change = (mean - TL_baseline)/TL_baseline * 100) %>%
  mutate(scenario_group = "Random") %>%
  mutate(climate_scenario = "2070-2100")


TL_community_future_EEZ = TL_community_weighted_future %>%
  filter(grepl('EEZ', scenario)) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_biomass = sum(biomass, na.rm = T),
         prop_biomass = biomass/total_biomass,
         TL_community_weighted = TL_community * prop_biomass) %>%
  summarise(TL_community_all = sum(TL_community_weighted, na.rm = T)) %>%
  ungroup() %>%
  group_by(mpa_coverage) %>%
  mutate(high = max(TL_community_all),
         low = min(TL_community_all),
         mean = mean(TL_community_all)) %>%
  ungroup() %>%
  mutate(TL_baseline = TL_baseline_nompa) %>%
  mutate(high_change = (high - TL_baseline)/TL_baseline * 100,
         low_change = (low - TL_baseline)/TL_baseline * 100,
         mean_change = (mean - TL_baseline)/TL_baseline * 100) %>%
  mutate(scenario_group = "EEZ-conservation") %>%
  mutate(climate_scenario = "2070-2100")

#   join by MPA scenario
TL_community_lit = rbind(TL_community_current_lit, TL_community_future_lit) %>%
  filter(mpa_coverage < 11) %>%
  mutate(MPA_scenario = case_when(scenario == "Existing network" ~ "S1-Current",
                                  scenario == "Mazor scenario 8" ~ "S5-Mazor GFCM",
                                  scenario == "Mazor scenario 9" ~ "S6-Mazor SAUP",
                                  scenario == "Micheli" ~ "S4-Micheli",
                                  TRUE ~ scenario)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                      TRUE ~ "Baseline climate conditions"))

TL_community_Random = rbind(TL_community_current_Random, TL_community_future_Random) %>%
  filter(mpa_coverage < 11) %>%
  mutate(MPA_scenario = case_when(scenario_group == "Random" ~ "S2-Random basin",
                                  TRUE ~ scenario_group)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                      TRUE ~ "Baseline climate conditions")) 

TL_community_EEZ = rbind(TL_community_current_EEZ, TL_community_future_EEZ) %>%
  filter(mpa_coverage < 11) %>%
  mutate(MPA_scenario = case_when(scenario_group == "EEZ-conservation" ~ "S3-Random EEZ",
                                  TRUE ~ scenario_group)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                      TRUE ~ "Baseline climate conditions"))


### PLOT -
scenario <- c("solid", "dotted", "dotted", "dotdash", "twodash", "longdash")

colors <- c("cadetblue", "darkred")


# Entire Med Sea -------------------------------------

print(TL_community_Med_future_present)

draw_key_shaded <- function(alpha_val) {
  function(data, params, size) {
    grobTree(
      rectGrob(gp = gpar(fill = alpha(data$colour, alpha_val), col = NA)),
      draw_key_path(data, params, size)
    )
  }
}
TL_community_Med_future_present <- ggplot() +
  
  geom_ribbon(data = TL_community_Random, aes(x = mpa_coverage, ymin = low_change, ymax = high_change, fill = climate_scenario), alpha = 0.3, show.legend = F) +
  
  geom_ribbon(data = TL_community_EEZ, aes(x = mpa_coverage, ymin = low_change, ymax = high_change, fill = climate_scenario), alpha = 0.5, show.legend = F) +
  
  geom_line(data = TL_community_EEZ, aes(x = mpa_coverage, y = mean_change, colour = climate_scenario, lty = MPA_scenario),
            lwd = 0.6, alpha = 1, key_glyph = draw_key_shaded(0.5)) +
  
  geom_line(data = TL_community_Random, aes(x = mpa_coverage, y = mean_change, colour = climate_scenario, lty = MPA_scenario),
            lwd = 0.4, alpha = 1, key_glyph = draw_key_shaded(0.3)) +
  
  geom_line(data = TL_community_lit, aes(x = mpa_coverage, y = TL_change, colour = climate_scenario, lty = MPA_scenario),
            lwd = 0.4, alpha = 1) +
  
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black", linewidth = 1) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14)) +
  theme(legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_linetype_manual(values = scenario) +
  labs(x ="MPA coverage (%)", y = "Change in TL (%)", title = "Trophic level of the community") +
  theme(legend.position = "right") +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = 16)) 
  #ylim(-7.2, 8.8)

print(TL_community_Med_future_present)
ggsave(here("figures/TL_community/TL_community_current_future_weighted_with_biomass_10pct.png"), dpi = 600, height = 6, width = 6)



TL_mean = TL_community_weighted_future %>% 
  filter(mpa_coverage == 10) %>%
  group_by(species_name) %>%
  summarize(TL_sp = mean(TL_community))

