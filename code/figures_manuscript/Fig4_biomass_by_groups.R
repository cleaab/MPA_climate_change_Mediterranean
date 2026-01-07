rm(list=ls())
library(tidyverse)
library(sf)
library(ncdf4)
library(here)
library(osmose)

# A3b. Biomass by groups - Barplots - all species
lit.scenarios.name <- c("Mazor scenario 8",
                        "Mazor scenario 9",
                        "Micheli",
                        "Existing network")

random.scenarios.name <- c("Random 11",
                           "Random 12",
                           "Random 13",
                           "Random 14",
                           "Random 15",
                           "Random 16",
                           "Random 17",
                           "Random 18",
                           "Random 19",
                           "Random 20",
                           "EEZ-conservation 1",
                           "EEZ-conservation 2",
                           "EEZ-conservation 3",
                           "EEZ-conservation 4",
                           "EEZ-conservation 5",
                           "EEZ-conservation 6",
                           "EEZ-conservation 7",
                           "EEZ-conservation 8",
                           "EEZ-conservation 9",
                           "EEZ-conservation 10")

MPA_scenarios_name <- c("S5-Mazor GFCM",
                        "S6-Mazor SAUP",
                        "S4-Micheli",
                        "S1-Current")
# BIOMASS by groups ----------------------------------------------------------

sp_group_levels <- c("Large pelagic", "Medium pelagic", "Large demersal", "Medium demersal", "Large benthic", "Medium benthic", "Small benthic", "Small pelagic", "Macroinvertebrates", "Small demersal")

legend_scenarios <- c("Existing network", "Random 11", "EEZ-conservation 1", "Micheli", "Mazor scenario 8", "Mazor scenario 9")

group_levels <- c("Existing network", "Random", "EEZ-scale conservation", "Micheli", "Mazor scenario 8", "Mazor scenario 9")

legend_labels <- c("S1-Current", "S2-Random basin", "S3-Random EEZ", "S4-Micheli", "S5-Mazor GFCM", "S6-Mazor SAUP")


####
Biomass_current = read.csv(here("data/model_outputs/Biomass_bygroups_NoCC_GSA-prop_11-07-2023.csv")) %>%
  filter(mpa.coverage < 11)

Biomass_future = read.csv(here("data/model_outputs/Biomass_bygroups_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  filter(mpa.coverage < 11)

pas = 62
Biomass_current_lit = Biomass_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  mutate(x_scale = mpa.coverage * pas/6229 * 100) %>%
  group_by(mpa.coverage, mpa, scenario, group) %>%
  summarise(B_mpa = mean(tot.biomass),
            B_nompa = mean(tot.biomass.ref)) %>%
  mutate(B_change = (B_mpa - B_nompa)/B_nompa * 100,
         B_diff = (B_mpa - B_nompa)) %>%
  mutate(climate_scenario = "2006-2013") 


Biomass_current_Random = Biomass_current %>%
  filter(grepl('Random', scenario)) %>%
  mutate(x_scale = mpa.coverage * pas/6229 * 100) %>%
  group_by(mpa.coverage, mpa, scenario, group) %>%
  summarise(B_mpa = mean(tot.biomass),
            B_nompa = mean(tot.biomass.ref)) %>%
  mutate(B_change = (B_mpa - B_nompa)/B_nompa * 100,
         B_diff = (B_mpa - B_nompa)) %>%
  ungroup() %>%
  group_by(mpa.coverage, mpa, group) %>%
  mutate(high = max(B_change),
         low = min(B_change),
         mean = mean(B_change)) %>%
  mutate(scenario_group = "Random") %>%
  mutate(climate_scenario = "2006-2013")

Biomass_current_EEZ = Biomass_current %>%
  filter(grepl('EEZ', scenario)) %>%
  mutate(x_scale = mpa.coverage * pas/6229 * 100) %>%
  group_by(mpa.coverage, mpa, scenario, group) %>%
  summarise(B_mpa = mean(tot.biomass),
            B_nompa = mean(tot.biomass.ref)) %>%
  mutate(B_change = (B_mpa - B_nompa)/B_nompa * 100,
         B_diff = (B_mpa - B_nompa)) %>%
  ungroup() %>%
  group_by(mpa.coverage, mpa, group) %>%
  mutate(high = max(B_change),
         low = min(B_change),
         mean = mean(B_change)) %>%
  mutate(scenario_group = "EEZ-conservation") %>%
  mutate(climate_scenario = "2006-2013")



Biomass_future_lit = Biomass_future %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  mutate(x_scale = mpa.coverage * pas/6229 * 100) %>%
  group_by(mpa.coverage, mpa, scenario, group) %>%
  summarise(B_mpa = mean(tot.biomass),
            B_nompa = mean(tot.biomass.ref)) %>%
  mutate(B_change = (B_mpa - B_nompa)/B_nompa * 100,
         B_diff = (B_mpa - B_nompa)) %>%
  mutate(climate_scenario = "2070-2100") 


Biomass_future_Random = Biomass_future %>%
  filter(grepl('Random', scenario)) %>%
  mutate(x_scale = mpa.coverage * pas/6229 * 100) %>%
  group_by(mpa.coverage, mpa, scenario, group) %>%
  summarise(B_mpa = mean(tot.biomass),
            B_nompa = mean(tot.biomass.ref)) %>%
  mutate(B_change = (B_mpa - B_nompa)/B_nompa * 100,
         B_diff = (B_mpa - B_nompa)) %>%
  ungroup() %>%
  group_by(mpa.coverage, mpa, group) %>%
  mutate(high = max(B_change),
         low = min(B_change),
         mean = mean(B_change)) %>%
  mutate(scenario_group = "Random") %>%
  mutate(climate_scenario = "2070-2100")

Biomass_future_EEZ = Biomass_future %>%
  filter(grepl('EEZ', scenario)) %>%
  mutate(x_scale = mpa.coverage * pas/6229 * 100) %>%
  group_by(mpa.coverage, mpa, scenario, group) %>%
  summarise(B_mpa = mean(tot.biomass),
            B_nompa = mean(tot.biomass.ref)) %>%
  mutate(B_change = (B_mpa - B_nompa)/B_nompa * 100,
         B_diff = (B_mpa - B_nompa)) %>%
  ungroup() %>%
  group_by(mpa.coverage, mpa, group) %>%
  mutate(high = max(B_change),
         low = min(B_change),
         mean = mean(B_change)) %>%
  mutate(scenario_group = "EEZ-conservation") %>%
  mutate(climate_scenario = "2070-2100")  


#   join by MPA scenario
Biomass_lit = rbind(Biomass_current_lit, Biomass_future_lit) %>%
  mutate(group = case_when(group == "Cephalopod" ~ "Cephalopods",
                           group == "Crustacean" ~ "Crustaceans",
                           TRUE ~ group)) %>%
  mutate(MPA_scenario = case_when(scenario == "Existing network" ~ "S1-Current",
                              scenario == "Mazor scenario 8" ~ "S5-Mazor GFCM",
                              scenario == "Mazor scenario 9" ~ "S6-Mazor SAUP",
                              scenario == "Micheli" ~ "S4-Micheli",
                              TRUE ~ scenario)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                       TRUE ~ "Baseline climate conditions"))

Biomass_Random = rbind(Biomass_current_Random, Biomass_future_Random) %>%
  mutate(group = case_when(group == "Cephalopod" ~ "Cephalopods",
                           group == "Crustacean" ~ "Crustaceans",
                           TRUE ~ group)) %>%
  mutate(MPA_scenario = case_when(scenario_group == "Random" ~ "S2-Random basin",
                                    TRUE ~ scenario_group)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                       TRUE ~ "Baseline climate conditions")) 

Biomass_EEZ = rbind(Biomass_current_EEZ, Biomass_future_EEZ) %>%
  mutate(group = case_when(group == "Cephalopod" ~ "Cephalopods",
                           group == "Crustacean" ~ "Crustaceans",
                           TRUE ~ group)) %>%
  
  mutate(MPA_scenario = case_when(scenario_group == "EEZ-conservation" ~ "S3-Random EEZ",
                                    TRUE ~ scenario_group)) %>%
    mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                         TRUE ~ "Baseline climate conditions"))

### PLOT -
scenario <- c("solid", "dotted", "dotted", "dotdash", "twodash", "longdash")

colors <- c("cadetblue", "darkred")


B_NoMPA = Biomass_lit %>%
  filter(mpa.coverage == 0) %>%
  filter(MPA_scenario == "S1-Current") %>%
  filter(mpa == "Entire Mediterranean Sea")
# Entire Med Sea -------------------------------------


library(ggplot2)
library(dplyr)
library(cowplot)
library(here)

plot_list <- list()
groups <- unique(Biomass_lit$group)

groups <- c(groups[3:5], "Cephalopods", groups[6:8], "Crustaceans", groups[9:11])
# ---- Create dummy plot for extracting the legend ----
dummy_group <- groups[1]

Biomass_lit_Med_dummy <- Biomass_lit %>%
  filter(mpa == "Entire Mediterranean Sea", group == dummy_group)

Biomass_Random_Med_dummy <- Biomass_Random %>%
  filter(mpa == "Entire Mediterranean Sea", group == dummy_group)

Biomass_EEZ_Med_dummy <- Biomass_EEZ %>%
  filter(mpa == "Entire Mediterranean Sea", group == dummy_group)


legend_plot <- ggplot() +
  geom_ribbon(data = Biomass_Random_Med_dummy, aes(x = mpa.coverage, ymin = low, ymax = high, fill = climate_scenario), alpha = 0.3, show.legend = F) +
  geom_ribbon(data = Biomass_EEZ_Med_dummy, aes(x = mpa.coverage, ymin = low, ymax = high, fill = climate_scenario), alpha = 0.5, show.legend = F) +
  geom_line(data = Biomass_EEZ_Med_dummy, aes(x = mpa.coverage, y = mean, colour = climate_scenario, lty = MPA_scenario), linewidth = 0.6) +
  geom_line(data = Biomass_Random_Med_dummy, aes(x = mpa.coverage, y = mean, colour = climate_scenario, lty = MPA_scenario), linewidth = 0.4) +
  geom_line(data = Biomass_lit_Med_dummy, aes(x = mpa.coverage, y = B_change, colour = climate_scenario, lty = MPA_scenario), linewidth = 0.4) +
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_linetype_manual(values = scenario) +
  guides(fill = "none") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold")) 


shared_legend <- cowplot::get_legend(legend_plot)

# ---- Generate plots without legends ----
for (g in groups) {

  Biomass_lit_Med <- Biomass_lit %>%
    filter(mpa == "Entire Mediterranean Sea", group == g)
  
  Biomass_Random_Med <- Biomass_Random %>%
    filter(mpa == "Entire Mediterranean Sea", group == g)
  
  Biomass_EEZ_Med <- Biomass_EEZ %>%
    filter(mpa == "Entire Mediterranean Sea", group == g)
  
  p <- ggplot() +
    geom_ribbon(data = Biomass_Random_Med, aes(x = mpa.coverage, ymin = low, ymax = high, fill = climate_scenario), alpha = 0.3) +
    geom_ribbon(data = Biomass_EEZ_Med, aes(x = mpa.coverage, ymin = low, ymax = high, fill = climate_scenario), alpha = 0.5) +
    geom_line(data = Biomass_EEZ_Med, aes(x = mpa.coverage, y = mean, colour = climate_scenario, lty = MPA_scenario), linewidth = 0.6) +
    geom_line(data = Biomass_Random_Med, aes(x = mpa.coverage, y = mean, colour = climate_scenario, lty = MPA_scenario), linewidth = 0.4) +
    geom_line(data = Biomass_lit_Med, aes(x = mpa.coverage, y = B_change, colour = climate_scenario, lty = MPA_scenario), linewidth = 0.4) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "none"
    ) +
    scale_colour_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_linetype_manual(values = scenario) +
    labs(x = "MPA coverage (%)", y = "Biomass change (%)", title = g)
  
  plot_list[[g]] <- p
}

# ---- Add the legend as the 12th item ----
plot_list[["legend"]] <- cowplot::plot_grid(shared_legend)

# ---- Combine into a 3 x 4 grid ----
final_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 4)

# ---- Save the plot ----
ggsave(here::here(paste0("figures/Biomass_by_groups_Med/Biomass_combined_legend_bottom_right_all_groups_for_paper_v2.png")),
       plot = final_plot, dpi = 600, height = 14, width = 14)
