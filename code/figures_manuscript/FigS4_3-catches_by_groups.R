rm(list=ls())
library(ggplot2)
library(dplyr)
library(cowplot)
library(here)

pas = 62

Catches_current = read.csv(here("data/model_outputs/Catches_bygroups_NoCC_GSA-prop_11-07-2023.csv")) %>%
  filter(mpa.coverage < 11)

Catches_future = read.csv(here("data/model_outputs/Catches_bygroups_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  filter(mpa.coverage < 11)


Catches_current_lit = Catches_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  mutate(x_scale = mpa.coverage * pas/6229 * 100) %>%
  group_by(mpa.coverage, mpa, scenario, group) %>%
  summarise(B_mpa = mean(tot.catches),
            B_nompa = mean(tot.catches.ref)) %>%
  mutate(B_change = (B_mpa - B_nompa)/B_nompa * 100,
         B_diff = (B_mpa - B_nompa)) %>%
  mutate(climate_scenario = "2006-2013") 


Catches_current_Random = Catches_current %>%
  filter(grepl('Random', scenario)) %>%
  mutate(x_scale = mpa.coverage * pas/6229 * 100) %>%
  group_by(mpa.coverage, mpa, scenario, group) %>%
  summarise(B_mpa = mean(tot.catches),
            B_nompa = mean(tot.catches.ref)) %>%
  mutate(B_change = (B_mpa - B_nompa)/B_nompa * 100,
         B_diff = (B_mpa - B_nompa)) %>%
  ungroup() %>%
  group_by(mpa.coverage, mpa, group) %>%
  mutate(high = max(B_change),
         low = min(B_change),
         mean = mean(B_change)) %>%
  mutate(scenario_group = "Random") %>%
  mutate(climate_scenario = "2006-2013")

Catches_current_EEZ = Catches_current %>%
  filter(grepl('EEZ', scenario)) %>%
  mutate(x_scale = mpa.coverage * pas/6229 * 100) %>%
  group_by(mpa.coverage, mpa, scenario, group) %>%
  summarise(B_mpa = mean(tot.catches),
            B_nompa = mean(tot.catches.ref)) %>%
  mutate(B_change = (B_mpa - B_nompa)/B_nompa * 100,
         B_diff = (B_mpa - B_nompa)) %>%
  ungroup() %>%
  group_by(mpa.coverage, mpa, group) %>%
  mutate(high = max(B_change),
         low = min(B_change),
         mean = mean(B_change)) %>%
  mutate(scenario_group = "EEZ-conservation") %>%
  mutate(climate_scenario = "2006-2013")




Catches_future_lit = Catches_future %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  mutate(x_scale = mpa.coverage * pas/6229 * 100) %>%
  group_by(mpa.coverage, mpa, scenario, group) %>%
  summarise(B_mpa = mean(tot.catches),
            B_nompa = mean(tot.catches.ref)) %>%
  mutate(B_change = (B_mpa - B_nompa)/B_nompa * 100,
         B_diff = (B_mpa - B_nompa)) %>%
  mutate(climate_scenario = "2070-2100") 


Catches_future_Random = Catches_future %>%
  filter(grepl('Random', scenario)) %>%
  mutate(x_scale = mpa.coverage * pas/6229 * 100) %>%
  group_by(mpa.coverage, mpa, scenario, group) %>%
  summarise(B_mpa = mean(tot.catches),
            B_nompa = mean(tot.catches.ref)) %>%
  mutate(B_change = (B_mpa - B_nompa)/B_nompa * 100,
         B_diff = (B_mpa - B_nompa)) %>%
  ungroup() %>%
  group_by(mpa.coverage, mpa, group) %>%
  mutate(high = max(B_change),
         low = min(B_change),
         mean = mean(B_change)) %>%
  mutate(scenario_group = "Random") %>%
  mutate(climate_scenario = "2070-2100")

Catches_future_EEZ = Catches_future %>%
  filter(grepl('EEZ', scenario)) %>%
  mutate(x_scale = mpa.coverage * pas/6229 * 100) %>%
  group_by(mpa.coverage, mpa, scenario, group) %>%
  summarise(B_mpa = mean(tot.catches),
            B_nompa = mean(tot.catches.ref)) %>%
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
Catches_lit = rbind(Catches_current_lit, Catches_future_lit) %>%
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

Catches_Random = rbind(Catches_current_Random, Catches_future_Random) %>%
  mutate(group = case_when(group == "Cephalopod" ~ "Cephalopods",
                           group == "Crustacean" ~ "Crustaceans",
                           TRUE ~ group)) %>%
  mutate(MPA_scenario = case_when(scenario_group == "Random" ~ "S2-Random basin",
                                  TRUE ~ scenario_group)) %>%
  mutate(climate_scenario = case_when(climate_scenario == "2070-2100" ~ "Future climate conditions",
                                      TRUE ~ "Baseline climate conditions")) 

Catches_EEZ = rbind(Catches_current_EEZ, Catches_future_EEZ) %>%
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


plot_list <- list()
groups <- unique(Catches_lit$group)

groups <- c(groups[3:5], "Cephalopods", groups[6:8], "Crustaceans", groups[9:11])
# ---- Create dummy plot for extracting the legend ----
dummy_group <- groups[1]

Catches_lit_Med_dummy <- Catches_lit %>%
  filter(mpa == "Entire Mediterranean Sea", group == dummy_group)

Catches_Random_Med_dummy <- Catches_Random %>%
  filter(mpa == "Entire Mediterranean Sea", group == dummy_group)

Catches_EEZ_Med_dummy <- Catches_EEZ %>%
  filter(mpa == "Entire Mediterranean Sea", group == dummy_group)

legend_plot <- ggplot() +
  geom_ribbon(data = Catches_Random_Med_dummy, aes(x = mpa.coverage, ymin = low, ymax = high, fill = climate_scenario), alpha = 0.3, show.legend = F) +
  geom_ribbon(data = Catches_EEZ_Med_dummy, aes(x = mpa.coverage, ymin = low, ymax = high, fill = climate_scenario), alpha = 0.5, show.legend = F) +
  geom_line(data = Catches_EEZ_Med_dummy, aes(x = mpa.coverage, y = mean, colour = climate_scenario, lty = MPA_scenario), linewidth = 0.6) +
  geom_line(data = Catches_Random_Med_dummy, aes(x = mpa.coverage, y = mean, colour = climate_scenario, lty = MPA_scenario), linewidth = 0.4) +
  geom_line(data = Catches_lit_Med_dummy, aes(x = mpa.coverage, y = B_change, colour = climate_scenario, lty = MPA_scenario), linewidth = 0.4) +
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_linetype_manual(values = scenario) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold"))

shared_legend <- cowplot::get_legend(legend_plot)

# ---- Generate plots without legends ----
for (g in groups) {
  Catches_lit_Med <- Catches_lit %>%
    filter(mpa == "Entire Mediterranean Sea", group == g)
  
  Catches_Random_Med <- Catches_Random %>%
    filter(mpa == "Entire Mediterranean Sea", group == g)
  
  Catches_EEZ_Med <- Catches_EEZ %>%
    filter(mpa == "Entire Mediterranean Sea", group == g)
  
  p <- ggplot() +
    geom_ribbon(data = Catches_Random_Med, aes(x = mpa.coverage, ymin = low, ymax = high, fill = climate_scenario), alpha = 0.3, show.legend = F) +
    geom_ribbon(data = Catches_EEZ_Med, aes(x = mpa.coverage, ymin = low, ymax = high, fill = climate_scenario), alpha = 0.5, show.legend = F) +
    geom_line(data = Catches_EEZ_Med, aes(x = mpa.coverage, y = mean, colour = climate_scenario, lty = MPA_scenario), linewidth = 0.6) +
    geom_line(data = Catches_Random_Med, aes(x = mpa.coverage, y = mean, colour = climate_scenario, lty = MPA_scenario), linewidth = 0.4) +
    geom_line(data = Catches_lit_Med, aes(x = mpa.coverage, y = B_change, colour = climate_scenario, lty = MPA_scenario), linewidth = 0.4) +
    
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
    labs(x = "MPA coverage (%)", y = "Catches change (%)", title = g)
  
  plot_list[[g]] <- p
}

# ---- Add the legend as the 12th item ----
plot_list[["legend"]] <- cowplot::plot_grid(shared_legend)

# ---- Combine into a 3 x 4 grid ----
final_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 4)

# ---- Save the plot ----
#ggsave(here::here(paste0("figures/Catches_by_groups_Med/Catches_combined_legend_bottom_right_all_groups_for_paper.png")),
#       plot = final_plot, dpi = 600, height = 14, width = 14)
ggsave(here::here(paste0("figures/Catches_by_groups_Med/Catches_combined_legend_bottom_right_all_groups_for_paper_v2.png")),
       plot = final_plot, dpi = 600, height = 14, width = 14)
