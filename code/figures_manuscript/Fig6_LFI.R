# B1. LFI
pas = 62

LFI_current = read.csv(here("data-mpa-CC-paper/model_outputs/LFI_with_sd_among_reps_NoCC_GSA-prop_11-07-2023.csv")) 

LFI_future = read.csv(here("data-mpa-CC-paper/model_outputs/LFI_with_sd_among_reps_CC_2100_GSA-prop_11-07-2023.csv"))

LFI_current_lit = LFI_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
  group_by(mpa_coverage, mpa, scenario) %>%
  summarise(mean_LFI_reps = mean(mean_LFI_60_years))

LFI_baseline_nompa = unique(LFI_current_lit$mean_LFI_reps[LFI_current_lit$mpa_coverage == 0])

LFI_current_lit = LFI_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
  group_by(mpa_coverage, mpa, scenario) %>%
  summarise(mean_LFI_reps = mean(mean_LFI_60_years)) %>%
  mutate(LFI_baseline = LFI_baseline_nompa) %>%
  mutate(LFI_change = (mean_LFI_reps - LFI_baseline)/LFI_baseline * 100) %>%
  mutate(climate_scenario = "2006-2013")


LFI_current_Random = LFI_current %>%
  filter(grepl('Random', scenario)) %>%
  mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
  group_by(mpa_coverage, mpa, scenario) %>%
  summarise(mean_LFI_reps = mean(mean_LFI_60_years)) %>%
  ungroup() %>%
  group_by(mpa_coverage, mpa) %>%
  mutate(high = max(mean_LFI_reps),
         low = min(mean_LFI_reps),
         mean = mean(mean_LFI_reps)) %>%
  ungroup() %>%
  mutate(LFI_baseline = LFI_baseline_nompa) %>%
  mutate(high_change = (high - LFI_baseline)/LFI_baseline * 100,
         low_change = (low - LFI_baseline)/LFI_baseline * 100,
         mean_change = (mean - LFI_baseline)/LFI_baseline * 100) %>%
  mutate(scenario_group = "Random") %>%
  mutate(climate_scenario = "2006-2013")


LFI_current_EEZ = LFI_current %>%
  filter(grepl('EEZ', scenario)) %>%
  mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
  group_by(mpa_coverage, mpa, scenario) %>%
  summarise(mean_LFI_reps = mean(mean_LFI_60_years)) %>%
  ungroup() %>%
  group_by(mpa_coverage, mpa) %>%
  mutate(high = max(mean_LFI_reps),
         low = min(mean_LFI_reps),
         mean = mean(mean_LFI_reps)) %>%
  ungroup() %>%
  mutate(LFI_baseline = LFI_baseline_nompa) %>%
  mutate(high_change = (high - LFI_baseline)/LFI_baseline * 100,
         low_change = (low - LFI_baseline)/LFI_baseline * 100,
         mean_change = (mean - LFI_baseline)/LFI_baseline * 100) %>%
  mutate(scenario_group = "EEZ-conservation") %>%
  mutate(climate_scenario = "2006-2013")

LFI_future_lit = LFI_future %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
  group_by(mpa_coverage, mpa, scenario) %>%
  summarise(mean_LFI_reps = mean(mean_LFI_60_years)) %>%
  mutate(LFI_baseline = LFI_baseline_nompa) %>%
  mutate(LFI_change = (mean_LFI_reps - LFI_baseline)/LFI_baseline * 100) %>%
  mutate(climate_scenario = "2070-2100")

LFI_future_Random = LFI_future %>%
  filter(grepl('Random', scenario)) %>%
  mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
  group_by(mpa_coverage, mpa, scenario) %>%
  summarise(mean_LFI_reps = mean(mean_LFI_60_years)) %>%
  ungroup() %>%
  group_by(mpa_coverage, mpa) %>%
  mutate(high = max(mean_LFI_reps),
         low = min(mean_LFI_reps),
         mean = mean(mean_LFI_reps)) %>%
  ungroup() %>%
  mutate(LFI_baseline = LFI_baseline_nompa) %>%
  mutate(high_change = (high - LFI_baseline)/LFI_baseline * 100,
         low_change = (low - LFI_baseline)/LFI_baseline * 100,
         mean_change = (mean - LFI_baseline)/LFI_baseline * 100) %>%
  mutate(scenario_group = "EEZ-conservation") %>%
  mutate(scenario_group = "Random") %>%
  mutate(climate_scenario = "2070-2100")


LFI_future_EEZ = LFI_future %>%
  filter(grepl('EEZ', scenario)) %>%
  mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
  group_by(mpa_coverage, mpa, scenario) %>%
  summarise(mean_LFI_reps = mean(mean_LFI_60_years)) %>%
  ungroup() %>%
  group_by(mpa_coverage, mpa) %>%
  mutate(high = max(mean_LFI_reps),
         low = min(mean_LFI_reps),
         mean = mean(mean_LFI_reps)) %>%
  ungroup() %>%
  mutate(LFI_baseline = LFI_baseline_nompa) %>%
  mutate(high_change = (high - LFI_baseline)/LFI_baseline * 100,
         low_change = (low - LFI_baseline)/LFI_baseline * 100,
         mean_change = (mean - LFI_baseline)/LFI_baseline * 100) %>%
  mutate(scenario_group = "EEZ-conservation") %>%
  mutate(scenario_group = "EEZ-conservation") %>%
  mutate(climate_scenario = "2070-2100")

#   join by MPA scenario
LFI_lit = rbind(LFI_current_lit, LFI_future_lit) %>%
  mutate(MPA_scenario = case_when(scenario == "Existing network" ~ "S1-Current",
                                  scenario == "Mazor scenario 8" ~ "S5-Mazor GFCM",
                                  scenario == "Mazor scenario 9" ~ "S6-Mazor SAUP",
                                  scenario == "Micheli" ~ "S4-Micheli",
                                  TRUE ~ scenario)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                       TRUE ~ "Baseline climate conditions"))

LFI_Random = rbind(LFI_current_Random, LFI_future_Random) %>%
  mutate(MPA_scenario = case_when(scenario_group == "Random" ~ "S2-Random basin",
                                  TRUE ~ scenario_group)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                       TRUE ~ "Baseline climate conditions")) 

LFI_EEZ = rbind(LFI_current_EEZ, LFI_future_EEZ) %>%
  mutate(MPA_scenario = case_when(scenario_group == "EEZ-conservation" ~ "S3-Random EEZ",
                                  TRUE ~ scenario_group)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                       TRUE ~ "Baseline climate conditions"))

### PLOT -
scenario <- c("solid", "dotted", "dotted", "dotdash", "twodash", "longdash")

colors <- c("cadetblue", "darkred")


# Entire Med Sea -------------------------------------

LFI_lit_Med = LFI_lit %>%
  filter(mpa == "Entire Mediterranean Sea") %>%
  filter(mpa_coverage < 11)

LFI_Random_Med = LFI_Random %>%
  filter(mpa == "Entire Mediterranean Sea") %>%
  filter(mpa_coverage < 11)


LFI_EEZ_Med = LFI_EEZ %>%
  filter(mpa == "Entire Mediterranean Sea") %>%
  filter(mpa_coverage < 11)





LFI_Med_future_present <- ggplot() +
  
  geom_ribbon(data = LFI_Random_Med, aes(x = mpa_coverage, ymin = low_change, ymax = high_change, fill = climate_scenario), alpha = 0.3, show.legend = F) +
  
  geom_ribbon(data = LFI_EEZ_Med, aes(x = mpa_coverage, ymin = low_change, ymax = high_change, fill = climate_scenario), alpha = 0.5, show.legend = F) +
  
  geom_line(data = LFI_EEZ_Med, aes(x = mpa_coverage, y = mean_change, colour = climate_scenario, lty = MPA_scenario), lwd = 0.6, alpha=1) +
  
  geom_line(data = LFI_Random_Med, aes(x = mpa_coverage, y = mean_change, colour = climate_scenario, lty = MPA_scenario), lwd = 0.4, alpha=1) +
  
  geom_line(data = LFI_lit_Med, aes(x = mpa_coverage, y = LFI_change, colour = climate_scenario, lty = MPA_scenario), lwd = 0.4, alpha=1) + 
  
  
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
  
  labs(x ="MPA coverage (%)", y = "Change in LFI (%)", title = "Large Fish Indicator") +
  
  theme(legend.position = "right") +
  #theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) 
# ylim(0.25, 0.43)

print(LFI_Med_future_present)
ggsave(here("figures/LFI/LFI_current_future_Med.png"), dpi = 600, height = 6, width = 6)


# --------------------------
# Inside MPAs
LFI_lit_Med = LFI_lit %>%
  filter(mpa == "Inside MPAs") %>%
  filter(mpa_coverage < 11)


LFI_Random_Med = LFI_Random %>%
  filter(mpa == "Inside MPAs") %>%
  filter(mpa_coverage < 11)


LFI_EEZ_Med = LFI_EEZ %>%
  filter(mpa == "Inside MPAs") %>% 
  filter(mpa_coverage < 11)





LFI_inside_future_present <- ggplot() +
  
  geom_ribbon(data = LFI_Random_Med, aes(x = mpa_coverage, ymin = low, ymax = high, fill = climate_scenario), alpha = 0.3) +
  
  geom_ribbon(data = LFI_EEZ_Med, aes(x = mpa_coverage, ymin = low, ymax = high, fill = climate_scenario), alpha = 0.5) +
  
  geom_line(data = LFI_EEZ_Med, aes(x = mpa_coverage, y = mean, colour = climate_scenario, lty = scenario_group), lwd = 0.8, alpha=1) +
  
  geom_line(data = LFI_Random_Med, aes(x = mpa_coverage, y = mean, colour = climate_scenario, lty = scenario_group), lwd = 0.8, alpha=1) +
  
  geom_line(data = LFI_lit_Med, aes(x = mpa_coverage, y = mean_LFI_reps, colour = climate_scenario, lty = scenario), lwd = 0.8, alpha=1) + 
  
  
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
  
  labs(x ="MPA coverage (%)", y = "LFI (> 20 cm)", title = "Inside MPAs") +
  
  theme(legend.position = "right") +
  #theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  ylim(0.25, 0.43)

print(LFI_inside_future_present)
ggsave(here("figures/LFI/LFI_current_future_inside_MPAs.png"), dpi = 600, height = 6, width = 6)


# --------------------------
# Outside MPAs
LFI_lit_Med = LFI_lit %>%
  filter(mpa == "Outside MPAs") %>%
  filter(mpa_coverage < 11)


LFI_Random_Med = LFI_Random %>%
  filter(mpa == "Outside MPAs") %>%
  filter(mpa_coverage < 11)


LFI_EEZ_Med = LFI_EEZ %>%
  filter(mpa == "Outside MPAs") %>%
  filter(mpa_coverage < 11)





LFI_outside_future_present <- ggplot() +
  
  geom_ribbon(data = LFI_Random_Med, aes(x = mpa_coverage, ymin = low, ymax = high, fill = climate_scenario), alpha = 0.3) +
  
  geom_ribbon(data = LFI_EEZ_Med, aes(x = mpa_coverage, ymin = low, ymax = high, fill = climate_scenario), alpha = 0.5) +
  
  geom_line(data = LFI_EEZ_Med, aes(x = mpa_coverage, y = mean, colour = climate_scenario, lty = scenario_group), lwd = 0.8, alpha=1) +
  
  geom_line(data = LFI_Random_Med, aes(x = mpa_coverage, y = mean, colour = climate_scenario, lty = scenario_group), lwd = 0.8, alpha=1) +
  
  geom_line(data = LFI_lit_Med, aes(x = mpa_coverage, y = mean_LFI_reps, colour = climate_scenario, lty = scenario), lwd = 0.8, alpha=1) + 
  
  
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
  
  labs(x ="MPA coverage (%)", y = "LFI (> 20 cm)", title = "Outside MPAs") +
  
  theme(legend.position = "right") +
  #theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  ylim(0.25, 0.43)

print(LFI_outside_future_present)
ggsave(here("figures/LFI/LFI_current_future_outside_MPAs.png"), dpi = 600, height = 6, width = 6)
