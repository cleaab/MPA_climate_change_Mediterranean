#Plot maps - Figure 9B (average across 10 replicate S3 (EEZ) MPA scenarios)

mpa_coverage = 10
B_ecoregions_all_groups_all_reps = st_read(here(paste0("data/model_outputs/spatial_comparison_ecoregions_mpa_", mpa_coverage, "_with_geom.gpkg")))

Impact_MPA = B_ecoregions_all_groups_all_reps %>%
  st_drop_geometry() %>%
  # calculate mean B accross EEZ scenarios
  group_by(ecoregions, scenario) %>%
  summarize(B_present_mpa = mean(B_present_mpa, na.rm = T),
            B_future_mpa = mean(B_future_mpa, na.rm = T),
            B_present_nompa = mean(B_present_nompa, na.rm = T),
            B_future_nompa = mean(B_future_nompa, na.rm = T)) %>%
  
  mutate(impact_mpa_future_diff = B_future_mpa - B_future_nompa,
         impact_mpa_future_delta = (B_future_mpa - B_future_nompa)/B_future_nompa * 100,
         impact_mpa_present_diff = B_present_mpa - B_present_nompa,
         impact_mpa_present_delta = (B_present_mpa - B_present_nompa)/B_present_nompa * 100) 


# change in impact present future - delta

Impact_MPA_mean = Impact_MPA %>%
  dplyr::select(ecoregions, scenario, impact_mpa_future_delta, impact_mpa_present_delta) %>%
  rename(future = impact_mpa_future_delta, present = impact_mpa_present_delta) %>%
  mutate(delta_diff = future-present) %>%

  group_by(ecoregions) %>%
  summarize(delta_diff_mean = mean(delta_diff, na.rm = T),
         delta_diff_sd = sd(delta_diff, na.rm = T))
  

  Impact_MPA_mean$ecoregions <- factor(
    Impact_MPA_mean$ecoregions,
  levels = Impact_MPA_mean %>%
    group_by(ecoregions) %>%
    summarise(mean_delta = mean(delta_diff_mean)) %>%
    arrange(mean_delta) %>%
    pull(ecoregions)
)


# Plot with reordered areas
plot_impact_diff_by_ecoregions <- ggplot() +
  geom_bar(
    data = Impact_MPA_mean,
    aes(y = ecoregions, x = delta_diff_mean),
    stat = "identity",
  ) +
  geom_errorbar(
    data = Impact_MPA_mean,
    aes(
      y = ecoregions,
      xmin = delta_diff_mean - delta_diff_sd,
      xmax = delta_diff_mean + delta_diff_sd
    ),
    width = 0.2
  ) +
  labs(title = "Impact of MPA by ecoregion", x = "MPA efficiency (%)", y = "") +
  
  labs(title= "",
       subtitle = "B) By ecoregion for a 10% MPA network",
       x = "Change in MPA effectiveness (%)") +
  
  theme_minimal()

print(plot_impact_diff_by_ecoregions)

ggsave(here("figures/Figure9B_Impact_MPA_future_by_ecoregions.png"), dpi = 300, height = 6, width = 6)


