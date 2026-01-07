clusters = read.csv(here("data/clusters_6_groups_Linf_TL_vd_rf_lifepsan_no_amphihaline_with_traits.csv")) %>%
  dplyr::select(-X) %>%
  rename(fe = cluster) %>%
  mutate(species_id = as.character(species_id))

commercial_importance = read.csv(here("data/classification_species_commercial_class_4_Jan_2026.csv")) %>%
  dplyr::select(species_name, commercial_class)

Table_S2_1 = clusters %>%
  left_join(commercial_importance, by = "species_name") %>%
  mutate(commercial_cat = case_when(commercial_class %in% c("HC", "C") ~ "High",
                                    commercial_class == "LC" ~ "Minor",
                                    TRUE ~ "None"))

write.csv(Table_S2_1, here("data/Table_S2_1_species_list_updated.csv"))
