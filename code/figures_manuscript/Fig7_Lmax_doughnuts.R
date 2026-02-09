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
  mutate(significative = case_when(p_value < 0.05 ~ "yes",
                                   TRUE ~ "no"))
table(compare_means$significative)
length(unique(compare_means$species_index))

# Plot barplot changes Nompas:
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

species_classif = read.csv(here("data-mpa-CC-paper/classification_species.csv")) %>%
  dplyr::select(species_id, species_name, group_article1, updated_commercial_interest, size_class, vertical_position, vulnerability_cat)

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

FigS4_2 <- ggplot(Lmax) +
  geom_bar(aes(x = diff_L95, y = factor(species_name), fill = change_category), stat="identity") +
  labs(x = "Difference in Lmax (cm)", y = " ")

print(FigS4_2)
ggsave(here("figures/Lmax_updated/FigS4_2_Change_Lmax_future_present.png"), dpi =300, width = 10, height = 10)


# Fig 7- Lmax: -----------------------------------------------
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

Lmax_future_mpa = read.csv(here("data-mpa-CC-paper/model_outputs/Lmax/Lmax_mpa_future.csv")) %>%
  dplyr::select(-X) %>%
  group_by(species_index) %>%
  summarize(L95_future_mean = mean(L95),
            L95_future_sd = sd(L95),
            L99_future_mean = mean(L99),
            L99_future_sd = sd(L99))

# all scens - mean accross spatial cells and across scenarios - first mean across S2 and S3 and then mean across 6 MPA scenarios
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


species_classif = read.csv(here("data-mpa-CC-paper/classification_species.csv")) %>%
  dplyr::select(species_id, species_name, group_article1, updated_commercial_interest, size_class, vertical_position, vulnerability_cat) %>%
  mutate(group_article1 = case_when(group_article1 == "Cephalopod" ~ "Cephalopods",
                                    group_article1 == "Crustacean" ~ "Crustaceans",
                                    TRUE ~ group_article1))

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

#Plot change and MPA impact: --------------------------------------------------
change_cats = Lmax_mitigation %>%
  filter(scenario == "without_MPAs") %>%
  dplyr::select(species_name, change_category)

Lmax_mitigation2 = Lmax_mitigation %>%
  dplyr::select(species_name, diff_L95, significative, scenario, vulnerability_cat, group_article1, size_class, updated_commercial_interest, vertical_position) %>%
  pivot_wider(names_from = scenario, values_from = diff_L95) %>%
  mutate(MPA_impact = with_MPAs - without_MPAs) %>%
  left_join(change_cats, by = "species_name") %>%
  mutate(MPA_effect = case_when(MPA_impact > 0 ~ "mitigation",
                                MPA_impact < 0 ~ "amplification",
                                TRUE ~ "stable")) %>%
  mutate(species_name = fct_reorder(species_name, MPA_impact)) %>%
  # <-- this line
  mutate(group_article1 = case_when(group_article1 == "Cephalopod" ~ "Cephalopods",
                                    group_article1 == "Crustacean" ~ "Crustaceans",
                                    TRUE ~ group_article1))

# Doughnut plots - by groups (fig 7 main text) ---------------------------------
groups = unique(Lmax_mitigation2$group_article1)
groups = c("Large-sized benthic fish", "Large-sized demersal fish", "Large-sized pelagic fish", "Cephalopods", "Medium-sized benthic fish", "Medium-sized demersal fish", "Medium-sized pelagic fish", "Crustaceans", "Small-sized benthic fish", "Small-sized demersal fish", "Small-sized pelagic fish")


# dummy plot
dummy_group <- groups[2]

Species_declining = Lmax_mitigation2 %>%
  filter(change_category == "decrease" & group_article1 == dummy_group)

Species_group = length(species_classif$species_name[species_classif$group_article1 == dummy_group])
Prop_declining = round(nrow(Species_declining)/Species_group *100, 0)

category_counts <- table(Species_declining$MPA_effect)
category_props <- prop.table(category_counts)


# Plot the proportions
df_plot <- as.data.frame(category_props)
colnames(df_plot) <- c("Change", "Proportion")

# Compute labels
df_plot$label <- paste0(round(100 * df_plot$Proportion, 0), "%")

fig_name <- gsub("-sized", "", i)

# Plot: Donut Chart
legend_plot <- ggplot(df_plot, aes(x = 2, y = Proportion, fill = Change)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  xlim(0.5, 2.5) +  # Makes the hole in the middle
  theme_void() +
  #geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 6) +
  scale_fill_manual(values = c("mitigation" = "purple", "amplification" = "darkgreen", "no effect" = "lightyellow")) +
  ggtitle(paste0(fig_name)) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  labs(fill = "MPA effect on \nLmax declines") +
  theme_bw(base_size = 16) +
  annotate("text", x = 0.5, y = 0, label = paste0(Prop_declining, "%"), size = 10, fontface = "bold") 

shared_legend <- cowplot::get_legend(legend_plot)

#######
plot_list <- list()
for (i in groups){
  Species_declining = Lmax_mitigation2 %>%
    filter(change_category == "decrease" & group_article1 == i)
  
  Species_group = length(species_classif$species_name[species_classif$group_article1 == i])
  Prop_declining = round(nrow(Species_declining)/Species_group *100, 0)
  
  # Species_increasing = Lmax_join %>%
  #   filter(change_category == "increase")
  ###PLOT - with mpas in the future
  # Count number of species in each category
  category_counts <- table(Species_declining$MPA_effect)
  category_props <- prop.table(category_counts)
  
  
  # Plot the proportions
  df_plot <- as.data.frame(category_props)
  colnames(df_plot) <- c("MPA_effect", "Proportion")
  print(i)
  print(df_plot)
  # Compute labels
  df_plot$label <- paste0(round(100 * df_plot$Proportion, 0), "%")
  
  fig_name <- gsub("-sized", "", i)
  
  
  # Plot: Donut Chart
  change_Lmax_future_mpas_plot <- ggplot(df_plot, aes(x = 2, y = Proportion, fill = MPA_effect)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    xlim(0.5, 2.5) +  # Makes the hole in the middle
    theme_void() +
    #geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 6) +
    scale_fill_manual(values = c("mitigation" = "purple", "amplification" = "darkgreen", "no effect" = "lightyellow")) +
    ggtitle(paste0(fig_name)) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
    annotate("text", x = 0.5, y = 0, label = paste0(Prop_declining, "%"), size = 7, fontface = "bold") 
    
  plot_list[[i]] <- change_Lmax_future_mpas_plot
}


# ---- Combine into a 3 x 4 grid ----
plot_list[["legend"]] <- cowplot::plot_grid(shared_legend)

# ---- Combine into a 3 x 4 grid ----
final_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 4)
print(final_plot)

# ---- Save the plot ----
ggsave(here::here(paste0("figures/Lmax_updated/doughnughts/groups/Lmax_combined_figure_corrected_for_paper_v2_all_6_scens.png")),
       plot = final_plot, dpi = 600, height = 6, width = 7)
### --------------------------------
# Identify species that benefit from MPA --------------------------
species_benefitting_from_MPA = Lmax_mitigation2 %>%
  filter(change_category == "decrease" & MPA_effect == "mitigation")

species_benefitting_from_MPA_mean_24_scens = Lmax_mitigation2 %>%
  filter(change_category == "decrease" & MPA_effect == "mitigation")

species_benefitting_from_MPA_S5 = Lmax_mitigation2 %>%
  filter(change_category == "decrease" & MPA_effect == "mitigation")


