#Total biomass increase in the current and future periods up to 10% (regrouping random scenarios for visual):

Biom_present <- read.csv(here("data/model_outputs/Total_biomass_NoCC_GSA-prop_11-07-2023.csv")) %>%
  rename(tot.biomass.present = tot.biomass,
         sd.tot.biomass.present = sd.tot.biomass) %>%
  dplyr::select(-relative_biom, -relative_biom_sd, -min, -max) %>%
  filter(mpa == "Entire Mediterranean Sea") 

Biom_present$biomass_before_MPAs = Biom_present$tot.biomass.present[Biom_present$mpa.coverage == 0]
Biom_present$biomass_change_with_MPAs_present = (Biom_present$tot.biomass.present - Biom_present$biomass_before_MPAs)/Biom_present$biomass_before_MPAs * 100
Biom_present$biomass_diff_with_MPAs_present = (Biom_present$tot.biomass.present - Biom_present$biomass_before_MPAs)

Biom_future <- read.csv(here("data/model_outputs/Total_biomass_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  rename(tot.biomass.future = tot.biomass,
         sd.tot.biomass.future = sd.tot.biomass) %>%
  dplyr::select(-relative_biom, -relative_biom_sd, -min, -max) %>%
  filter(mpa == "Entire Mediterranean Sea") 

Biom_future$biomass_before_MPAs = Biom_future$tot.biomass.future[Biom_future$mpa.coverage == 0]
Biom_future$biomass_change_with_MPAs_future = (Biom_future$tot.biomass.future - Biom_future$biomass_before_MPAs)/Biom_future$biomass_before_MPAs * 100
Biom_future$biomass_diff_with_MPAs_future = (Biom_future$tot.biomass.future - Biom_future$biomass_before_MPAs)

# Compare deltas in a same plot
B_delta_comparison = Biom_present %>%
  left_join(Biom_future, by = c("scenario", "mpa", "mpa.coverage")) %>%
  filter(scenario %in% c("Existing network", "Mazor scenario 8", "Mazor scenario 9", "Micheli")) %>%
  dplyr::select("scenario", "mpa", "mpa.coverage", "biomass_change_with_MPAs_future", "biomass_change_with_MPAs_present") %>%
  mutate(delta_change = biomass_change_with_MPAs_future - biomass_change_with_MPAs_present)
  #filter(mpa.coverage < 11)

B_delta_comparison_random = Biom_present %>%
  left_join(Biom_future, by = c("scenario", "mpa", "mpa.coverage")) %>%
  filter(grepl("Random", scenario)) %>%
  dplyr::select("scenario", "mpa", "mpa.coverage", "biomass_change_with_MPAs_future", "biomass_change_with_MPAs_present") %>%
  mutate(delta_change = biomass_change_with_MPAs_future - biomass_change_with_MPAs_present) %>%
  #filter(mpa.coverage < 11) %>%
  group_by(mpa.coverage) %>%
  mutate(max = max(delta_change),
         mean = mean(delta_change),
         min = min(delta_change))

B_delta_comparison_random_EEZ = Biom_present %>%
  left_join(Biom_future, by = c("scenario", "mpa", "mpa.coverage")) %>%
  filter(grepl("EEZ", scenario)) %>%
  dplyr::select("scenario", "mpa", "mpa.coverage", "biomass_change_with_MPAs_future", "biomass_change_with_MPAs_present") %>%
  mutate(delta_change = biomass_change_with_MPAs_future - biomass_change_with_MPAs_present) %>%
  #filter(mpa.coverage < 11) %>%
  group_by(mpa.coverage) %>%
  mutate(max = max(delta_change),
         mean = mean(delta_change),
         min = min(delta_change))


mpa_colors <- c("Existing network" = "gold", 
                "Random 11" = "gray60", 
                "Random 12" = "gray60",
                "Random 13" = "gray60",
                "Random 14" = "gray60",
                "Random 15" = "gray60",
                "Random 16" = "gray60",
                "Random 17" = "gray60",
                "Random 18" = "gray60",
                "Random 19" = "gray60",
                "Random 20" = "gray60",
                
                "EEZ-conservation 1" = "black", 
                "EEZ-conservation 2" = "black", 
                "EEZ-conservation 3" = "black", 
                "EEZ-conservation 4" = "black", 
                "EEZ-conservation 5" = "black", 
                "EEZ-conservation 6" = "black", 
                "EEZ-conservation 7" = "black", 
                "EEZ-conservation 8" = "black", 
                "EEZ-conservation 9" = "black", 
                "EEZ-conservation 10" = "black", 
                
                "Micheli" = "darkorange", 
                "Mazor scenario 8" = "darkslateblue", 
                "Mazor scenario 9" = "darkorchid")

legend_scenarios <- c("Existing network", "Micheli", "Random 11", "Mazor scenario 8", "EEZ-conservation 1", "Mazor scenario 9")

group_levels <- c("Existing network", "Micheli", "Random", "Mazor scenario 8", "EEZ-scale conservation", "Mazor scenario 9")

legend_labels <- c("S1-Current", "S4-Micheli", "S2-Random basin", "S5-Mazor GFCM", "S3-Random EEZ", "S6-Mazor SAUP")

Delta_comparison_plot <- ggplot() +
  

  geom_ribbon(data = B_delta_comparison_random_EEZ, aes(x = mpa.coverage, ymin = min, ymax = max), fill = "gray40", alpha = 0.5) +
  geom_ribbon(data = B_delta_comparison_random, aes(x = mpa.coverage, ymin = min, ymax = max), fill = "gray80", alpha = 0.5) +
  
  geom_line(data = B_delta_comparison_random, aes(x = mpa.coverage, y = delta_change, col = scenario), lwd = 0.3, alpha=1) +
  geom_line(data = B_delta_comparison_random_EEZ, aes(x = mpa.coverage, y = delta_change, col = scenario), lwd = 0.3, alpha=1) +
  
  geom_line(data = B_delta_comparison, aes(x = mpa.coverage, y = delta_change, col = scenario), lwd = 0.8, alpha=1) +
  geom_line(data = B_delta_comparison, aes(x = mpa.coverage, y = 0), col = "black", lty = "dashed", lwd = 0.8, alpha=1) +
  
  scale_linetype_manual(values = scenario) +  
  scale_colour_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
  scale_fill_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) + 
 
  labs(title= "Total biomass",
       subtitle = "A) Entire Mediterranean Sea",
       x ="MPA coverage (%)", y = "Change in MPA effectiveness (%)") +

  theme_bw() +
  theme(plot.title = element_text(face = "bold"))

print(Delta_comparison_plot)

ggsave(here("figures/impact_mpa_future_ecoregions/Efficiency_loss_MPA_future_30pct.png"), dpi = 300, height = 6, width = 6)

# Are differences significative?  ------ yes for all coverage levels considering each MPA scenario as a pseudo-replicate

for (cov in 1:30) {
  present_vals <- subset(Biom_present, mpa.coverage == cov)$biomass_change_with_MPAs_present
  
  future_vals <- subset(Biom_future, mpa.coverage == cov)$biomass_change_with_MPAs_future
  
  print(present_vals)
  print(future_vals)
  
  # check if normal
  normal_test_present = shapiro.test(present_vals)$p.value
  normal_test_future = shapiro.test(future_vals)$p.value
  
  print(normal_test_present)
  print(normal_test_future)
  
  if (normal_test_future > 0.05 & normal_test_present > 0.05){
    
    p_value = t.test(present_vals, future_vals)$p.value
    
  } else{
    p_value = wilcox.test(present_vals, future_vals)$p.value
  }
  
  if (cov == 1){
    p_values = p_value
  } else{
    p_values = c(p_values, p_value)
  }
  
  
}

compare_means = data.frame(seq(1,30,1))
colnames(compare_means) = "mpa.coverage"
compare_means$p_value = p_values
compare_means = compare_means %>%
  mutate(significative = case_when(p_value < 0.05 ~ "yes",
                                   TRUE ~ "no"))
