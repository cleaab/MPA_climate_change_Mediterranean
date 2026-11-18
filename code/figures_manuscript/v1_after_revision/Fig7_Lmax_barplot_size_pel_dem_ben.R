rm(list=ls())
library(tidyverse)
library(ncdf4)
library(here)


# Wilcox test (need to keep all the replicates):
Lmax_present_nompa = read.csv(here("data-mpa-CC-paper/model_outputs/Lmax/Lmax_nompa_present.csv"))
Lmax_future_nompa = read.csv(here("data-mpa-CC-paper/model_outputs/Lmax/Lmax_nompa_future.csv")) 
  
species_index = unique(Lmax_future_nompa$species_index)

for (i in species_index) {
  present_vals <- subset(Lmax_present_nompa, species_index == i)$L95
  
  future_vals <- subset(Lmax_future_nompa, species_index == i)$L95
  
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
  
  if (i == species_index[1]){
    p_values = p_value
  } else{
    p_values = c(p_values, p_value)
  }
  
  
}

compare_means = data.frame(species_index)
compare_means$p_value = p_values
compare_means = compare_means %>%
  filter(!species_index %in% c(0,1,2)) %>%
  mutate(significative = case_when(p_value < 0.05 ~ "yes",
                                   TRUE ~ "no"))
table(compare_means$significative)
length(unique(compare_means$species_index))

# Plot barplot changes No MPAs (Supplementary Figure 6) ------------------------
species_classif = read.csv(here("data-mpa-CC-paper/classification_species.csv")) %>%
  dplyr::select(species_id, species_name, group_article1, updated_commercial_interest, size_class, vertical_position, vulnerability_cat) %>%
  filter(!species_id %in% c(0,1,2,4, 10, 33, 54, 55, 64, 88, 93, 100)) %>% #removed anadromous/catadromous species
  
  mutate(
    size_habitat_group = case_when(
      is.na(size_class) | is.na(vertical_position) ~ NA_character_,
      TRUE ~ paste(size_class, tolower(vertical_position))
    ))

Lmax_present_nompa = read.csv(here("data-mpa-CC-paper/model_outputs/Lmax/Lmax_nompa_present.csv")) %>%
  dplyr::select(-X) %>%
  group_by(species_index) %>%
  summarize(L95_mean = mean(L95),
            L95_sd = sd(L95),
            L99_mean = mean(L99),
            L99_sd = sd(L99)) 


Lmax_future_nompa = read.csv(here("data-mpa-CC-paper/model_outputs/Lmax/Lmax_nompa_future.csv")) %>%
  dplyr::select(-X) %>%
  group_by(species_index) %>%
  summarize(L95_future_mean = mean(L95),
            L95_future_sd = sd(L95),
            L99_future_mean = mean(L99),
            L99_future_sd = sd(L99)) 

Lmax = Lmax_present_nompa %>%
  left_join(Lmax_future_nompa, by = "species_index") %>%
  left_join(compare_means, by = "species_index") %>%
  mutate(diff_L95 = L95_future_mean - L95_mean) %>%
  mutate(delta_L95 = (L95_future_mean - L95_mean)/L95_mean * 100) %>%
  mutate(change_category = case_when(diff_L95 < 0 ~ "decrease",
                                     diff_L95 > 0 ~ "increase",
                                     TRUE ~ "stable")) %>%
  rename(species_id = species_index) %>%
  left_join(species_classif, by = "species_id") %>%
  filter(significative == "yes") %>%
  filter(! species_id %in% c(0,1,2)) %>%
  mutate(species_name = case_when(species_name == "Etrumeusteres" ~ "Etrumeusgolanii",
                                  species_name == "Sauridaundosquamis" ~ "Sauridalessepsianus",
                                  TRUE ~ species_name)) %>%
  mutate(species_name = fct_reorder(species_name, diff_L95))   # <-- this line
  
table(Lmax$significative)
table(Lmax$change_category)

table(Lmax$change_category[Lmax$significative == "yes"])

Supp_Fig_6_Lmax <- ggplot(Lmax) +
  geom_bar(aes(x = diff_L95, y = factor(species_name), fill = change_category), stat="identity") +
  labs(x = "Difference in Lmax (cm)", y = " ")

print(Supp_Fig_6_Lmax)

# To save figure
ggsave(here("figures/after_revision1/FINAL/SuppFig6_Change_Lmax_future_present.png"), dpi =300, width = 10, height = 10)


# Fig 7 - Lmax: ----------------------------------------------------------------
Lmax_present_nompa = read.csv(here("data-mpa-CC-paper/model_outputs/Lmax/Lmax_nompa_present.csv")) %>%
  dplyr::select(-X) %>%
  group_by(species_index) %>%
  summarize(L95_mean = mean(L95),
            L95_sd = sd(L95),
            L99_mean = mean(L99),
            L99_sd = sd(L99)) 
  

Lmax_future_nompa = read.csv(here("data-mpa-CC-paper/model_outputs/Lmax/Lmax_nompa_future.csv")) %>%
  dplyr::select(-X) %>%
  group_by(species_index) %>%
  summarize(L95_future_mean = mean(L95),
            L95_future_sd = sd(L95),
            L99_future_mean = mean(L99),
            L99_future_sd = sd(L99)) 

# all scens - mean accross spatial cells and across scenarios - first mean across S2 and S3 and then mean across 6 MPA scenarios for 10% coverage
Lmax_future_mpa = read.csv(here("data-mpa-CC-paper/model_outputs/Lmax/Lmax_mpa_future_all_scens.csv")) %>%
  dplyr::select(-X) %>%
  mutate(mpa_scenario_group = case_when(grepl("EEZ", mpa_scenario) ~ "EEZ-conservation",
                                        grepl("Random", mpa_scenario) ~ "Random",
                                        TRUE ~ mpa_scenario)) %>%
  group_by(species_index, mpa_scenario_group) %>%
  summarize(L95_future_mean_6 = mean(L95),
            L95_future_sd_6 = sd(L95),
            L99_future_mean_6 = mean(L99),
            L99_future_sd_6 = sd(L99)) %>%
  ungroup() %>%
  group_by(species_index) %>%
  summarize(L95_future_mean = mean(L95_future_mean_6),
            L95_future_sd = sd(L95_future_sd_6),
            L99_future_mean = mean(L99_future_mean_6),
            L99_future_sd = sd(L99_future_sd_6))

Lmax_nompa = Lmax_present_nompa %>%
  left_join(Lmax_future_nompa, by = "species_index") %>%
  left_join(compare_means, by = "species_index") %>%
  mutate(diff_L95 = L95_future_mean - L95_mean) %>%
  mutate(delta_L95 = (L95_future_mean - L95_mean)/L95_mean * 100) %>%
  mutate(change_category = case_when(diff_L95 < 0 ~ "decrease",
                                     diff_L95 > 0 ~ "increase",
                                     TRUE ~ "stable")) %>%
  rename(species_id = species_index) %>%
  left_join(species_classif, by = "species_id") %>%
  filter(significative == "yes") %>%
  filter(! species_id %in% c(0,1,2)) %>%
  mutate(scenario = "without_MPAs")

Lmax_mpa = Lmax_present_nompa %>%
  left_join(Lmax_future_mpa, by = "species_index") %>%
  left_join(compare_means, by = "species_index") %>%
  mutate(diff_L95 = L95_future_mean - L95_mean) %>%
  mutate(delta_L95 = (L95_future_mean - L95_mean)/L95_mean * 100) %>%
  mutate(change_category = case_when(diff_L95 < 0 ~ "decrease",
                                     diff_L95 > 0 ~ "increase",
                                     TRUE ~ "stable")) %>%
  rename(species_id = species_index) %>%
  left_join(species_classif, by = "species_id") %>%
  filter(significative == "yes") %>%
  filter(! species_id %in% c(0,1,2)) %>%
  mutate(scenario = "with_MPAs")

Lmax_mitigation = rbind(Lmax_nompa, Lmax_mpa) %>%
  mutate(species_name = fct_reorder(species_name, diff_L95))  

change_cats = Lmax_mitigation %>%
  filter(scenario == "without_MPAs") %>%
  dplyr::select(species_name, change_category)

Lmax_mitigation2 = Lmax_mitigation %>%
  dplyr::select(species_name, diff_L95, significative, scenario, vulnerability_cat, group_article1, size_class, updated_commercial_interest, vertical_position, size_habitat_group) %>%
  pivot_wider(names_from = scenario, values_from = diff_L95) %>%
  mutate(MPA_impact = with_MPAs - without_MPAs) %>%
  left_join(change_cats, by = "species_name") %>%
  mutate(MPA_effect = case_when(MPA_impact > 0 ~ "mitigation",
                                MPA_impact < 0 ~ "amplification",
                                TRUE ~ "stable")) %>%
  mutate(species_name = fct_reorder(species_name, MPA_impact)) 

# Plot Figure 7
Species_declining <- Lmax_mitigation2 %>%
  filter(
    change_category == "decrease",
    significative == "yes"
  ) %>%
  group_by(size_habitat_group) %>%
  summarise(
    sp_declining = n(),
    amplification = sum(MPA_effect == "amplification", na.rm = TRUE),
    mitigation = sum(MPA_effect == "mitigation", na.rm = TRUE),
    Prop_amplification = amplification / sp_declining * 100,
    Prop_mitigation = mitigation / sp_declining * 100,
    .groups = "drop"
  ) %>%
  ungroup()

tot_sp_per_group = species_classif %>%
  group_by(size_habitat_group) %>%
  summarise(tot_species_per_group = n())

Species_declining <- Species_declining %>%
  left_join(tot_sp_per_group, by = "size_habitat_group") %>%
  mutate(Prop_declining = sp_declining/tot_species_per_group*100) %>%
  pivot_longer(5:6, names_to = "MPA_effect", values_to = "MPA_impact_prop")

  
Species_declining <- Species_declining %>%
  mutate(
    size_habitat_group = reorder(size_habitat_group, Prop_declining)
  )

Fig7_barplot_change_Lmax_future_mpas <- ggplot(
  Species_declining,
  aes(
    x = size_habitat_group,
    y = Prop_declining * MPA_impact_prop / 100,
    fill = MPA_effect
  )
) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "Prop_amplification" = "darkgreen",
      "Prop_mitigation" = "purple"
    ),
    labels = c(
      "Prop_amplification" = "Amplification",
      "Prop_mitigation" = "Mitigation"
    )
  ) +
  labs(
    x = NULL,
    y = "Species with significant Lmax declines (%)",
    fill = "MPA effect"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.background = element_rect(
      fill = "transparent",
      colour = NA
    ),
    plot.background = element_rect(
      fill = "transparent",
      colour = NA
    )
  ) 

print(Fig7_barplot_change_Lmax_future_mpas)

# save Figure 7
ggsave(here::here(paste0("figures/after_revision1/FINAL/Fig7_Lmax_barplot_sp_classif_pel_dem_ben_89sp_reordered.png")),
       plot = barplot_change_Lmax_future_mpas_plot, dpi = 600, height = 6, width = 7)


# Identify species that benefit from MPA --------------------------
species_benefitting_from_MPA = Lmax_mitigation2 %>%
  filter(change_category == "decrease" & MPA_effect == "mitigation")


