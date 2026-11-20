library(tidyverse)

species_classif = read.csv(here("data-mpa-CC-paper/species_classif_size_habitat.csv"))

price_data = read.csv(here("data-mpa-CC-paper/fisheries_data/Price_data_updated_without_outliers.csv")) %>%
  mutate(species_name = case_when(species_name == "Diplodussargus" ~ "Diplodussargussargus",
                                  TRUE ~ species_name)) %>%
  
  mutate(fishing_entity = case_when(fishing_entity %in% c("North Cyprus", "South Cyprus") ~ "Cyprus",
                                    fishing_entity == "Syrian Arab Republic" ~ "Syria",
                                    is.na(fishing_entity) ~ "Unknown",
                                    #fishing_entity %in% c("Mauritania", "Iceland", "Taiwan", "Korea (South)", "Unknown Fishing Country") ~ "Unknown or other",
                                    TRUE ~ fishing_entity)) %>%
  
  group_by(species_name, gsa) %>%
  summarize(price = mean(price, na.rm = T)) %>%
  
  # left_join
  left_join(species_classif, by = "species_name")


price_data_mean_species = price_data %>%
  filter(!is.na(species_id)) %>%
  group_by(species_name, size_habitat_group) %>%
  summarize(price = mean(price)) %>%
  ungroup() %>%
  mutate(stand_price = price/max(price)) %>%
  mutate(size_habitat_group = case_when(size_habitat_group %in% c("Large pelagic", "Medium pelagic") ~ "Medium & large pelagic",
                                    size_habitat_group %in% c("Large demersal", "Medium demersal") ~ "Medium & large demersal",
                                    size_habitat_group %in% c("Large benthic", "Medium benthic") ~ "Medium & large benthic",
                                    TRUE ~ size_habitat_group)) %>%
  mutate(species_name = case_when(species_name == "Etrumeusteres" ~ "Etrumeusgolanii",
                                  species_name == "Sauridaundosquamis" ~ "Sauridalessepsianus",
                                  TRUE ~ species_name)) # correct name for species


group_colors <- c(
  "Small benthic" = "#A6CEE3",
  "Medium & large benthic" = "#0072B2",
  
  "Small demersal" = "#FDD0A2",
  "Medium & large demersal" = "#D55E00",
  
  "Small pelagic" = "#A8DCCF",
  "Medium & large pelagic" = "#009E73"
)
price_rank_size <- ggplot(price_data_mean_species) +
  geom_bar(
    aes(
      y = reorder(species_name, stand_price),
      x = stand_price,
      fill = size_habitat_group
    ),
    stat = "identity"
  ) +
  scale_fill_manual(
    values = group_colors,
    name = "Group"
  ) +
  
  labs(
    x = "Standardized Price",
    y = "Species Name",
    title = ""
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(price_rank_size)

ggsave(here("figures/after_revision1/FINAL/SuppFig3_rank_species_by_landing_value_kg_groups.png"), dpi = 300, width = 8, height = 10)


