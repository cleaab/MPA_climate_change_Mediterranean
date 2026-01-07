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

Fishing_scenario_name = c("Fishing-the-line", "Proportional by GSA", "Uniform by GSA")
Fishing_scenario = c("Fishing-the-line", "GSA-prop", "GSA-uniform")

MPA_scenarios_name <- c("S5-Mazor GFCM",
                        "S6-Mazor SAUP",
                        "S4-Micheli",
                        "S1-Current")

lit.mpa.mask.dir <- c(here("data/MPA_scenarios/Mazor_scenario_8/csv_gpkg_files"),
                      here("data/MPA_scenarios/Mazor_scenario_9/csv_gpkg_files"),
                      here("data/MPA_scenarios/Micheli/csv_gpkg_files"),
                      here("data/MPA_scenarios/Existing_network/csv_gpkg_files"))


random.mpa.mask.dir <- c(here("data/MPA_scenarios/Random/rep11/csv_gpkg_files"),
                         here("data/MPA_scenarios/Random/rep12/csv_gpkg_files"),
                         here("data/MPA_scenarios/Random/rep13/csv_gpkg_files"),
                         here("data/MPA_scenarios/Random/rep14/csv_gpkg_files"),
                         here("data/MPA_scenarios/Random/rep15/csv_gpkg_files"),
                         here("data/MPA_scenarios/Random/rep16/csv_gpkg_files"),
                         here("data/MPA_scenarios/Random/rep17/csv_gpkg_files"),
                         here("data/MPA_scenarios/Random/rep18/csv_gpkg_files"),
                         here("data/MPA_scenarios/Random/rep19/csv_gpkg_files"),
                         here("data/MPA_scenarios/Random/rep20/csv_gpkg_files"),
                         here("data/MPA_scenarios/EEZ-conservation/rep1/csv_gpkg_files"),
                         here("data/MPA_scenarios/EEZ-conservation/rep2/csv_gpkg_files"),
                         here("data/MPA_scenarios/EEZ-conservation/rep3/csv_gpkg_files"),
                         here("data/MPA_scenarios/EEZ-conservation/rep4/csv_gpkg_files"),
                         here("data/MPA_scenarios/EEZ-conservation/rep5/csv_gpkg_files"),
                         here("data/MPA_scenarios/EEZ-conservation/rep6/csv_gpkg_files"),
                         here("data/MPA_scenarios/EEZ-conservation/rep7/csv_gpkg_files"),
                         here("data/MPA_scenarios/EEZ-conservation/rep8/csv_gpkg_files"),
                         here("data/MPA_scenarios/EEZ-conservation/rep9/csv_gpkg_files"),
                         here("data/MPA_scenarios/EEZ-conservation/rep10/csv_gpkg_files"))

# Calculate ANOVA - % of variance explained by CC and MPAs
biomass_NoCC_GSAprop = read.csv(here("data/model_outputs/Biomass_bygroups_NoCC_GSA-prop_11-07-2023.csv")) %>%
  filter(mpa == "Entire Mediterranean Sea") %>%
  mutate(climate = "Baseline")

biomass_CC_GSAprop = read.csv(here("data/model_outputs/Biomass_bygroups_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  filter(mpa == "Entire Mediterranean Sea") %>%
  mutate(climate = "Future")


data_10pct = rbind(biomass_NoCC_GSAprop, biomass_CC_GSAprop) %>%
  rename(MPA = scenario, species_group = group) %>%
  filter(mpa.coverage == 10) %>%
  mutate(biomass_change = (tot.biomass - tot.biomass.ref)/tot.biomass.ref * 100) %>%
  dplyr::select(-mpa, -x_scale, -mpa.coverage, -tot.biomass, -tot.biomass.ref) %>%
  mutate(MPA = case_when(grepl("EEZ", MPA) ~ "EEZ-conservation",
                         grepl("Random", MPA) ~ "Random",
                         TRUE ~ MPA)) %>%
  mutate(MPA = as.factor(MPA),
         climate = as.factor(climate)) 

summary(data_10pct)

# Compute anova - climate, MPA scenarios at 10% coverage
#species_groups = unique(data_10pct$species_group)
species_groups <- c("Large-sized benthic fish", 
                    "Large-sized demersal fish", 
                    "Large-sized pelagic fish",
                    "Cephalopod",
                    "Medium-sized benthic fish",
                    "Medium-sized demersal fish",
                    "Medium-sized pelagic fish",
                    "Crustacean",
                    "Small-sized benthic fish",
                    "Small-sized demersal fish",
                    "Small-sized pelagic fish")

for (i in species_groups){
  data_10pct_sp_group = data_10pct %>%
    filter(species_group == i)
  mod = lm(biomass_change ~ climate * MPA, data=data_10pct_sp_group)
  anova_result = anova(mod)
  print(anova_result)
  summary(mod)
  
  SS_term <- anova_result[,"Sum Sq"]
  SS_total <- sum(SS_term)
  pct_total <- 100 * SS_term / SS_total
  print(pct_total)
  
  vars = c("climate", "MPA", "climate:MPA", "stochasticity")
  
  rel_var = data.frame(vars, pct_total)
  rel_var$species_group = i
  
  if (i == species_groups[1]){
    rel_var_all_groups = rel_var
  } else{
    rel_var_all_groups = rbind(rel_var_all_groups, rel_var)
   
  }
}

# Plot - relative effects

my_order <- c("Large-sized pelagic fish", "Medium-sized pelagic fish", "Small-sized pelagic fish",
              "Large-sized demersal fish", "Medium-sized demersal fish", "Small-sized demersal fish",
              "Large-sized benthic fish", "Medium-sized benthic fish", "Small-sized benthic fish",
              "Crustacean", "Cephalopod")

rel_var_all_groups <- rel_var_all_groups %>%
  mutate(species_group = factor(species_group, levels = my_order))

# my_order <- c("stochasticity", "climate", "climate:MPA", "MPA")
# 
# rel_var_all_groups <- rel_var_all_groups %>%
#   mutate(vars = factor(vars, levels = my_order))


fill_cols <- c(
  "stochasticity" = "grey80",
  "MPA"           = "#009E73",  # bluish green
  "climate"       = "#0072B2",  # blue
  "climate:MPA"   = "#CC79A7"   # reddish purple (reads like a "mix")
)

ggplot(rel_var_all_groups,
       aes(x = species_group, y = pct_total, fill = vars)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Species group", y = "% of total variance", fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("figures/anova/pct_total_variance_by_species_groups.png"), dpi = 300, height = 5, width = 7)

ggplot(rel_var_all_groups,
       aes(x = pct_total, y = species_group, fill = vars)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(y = "", x = "% of total variance", fill = "Source") +
  theme_bw() +
  scale_fill_manual(values = fill_cols, breaks = names(fill_cols)) 
  

ggsave(here("figures/anova/pct_total_variance_by_species_groups_flipped.png"), dpi = 300, height = 5, width = 7)


# Make plot where MPA coverage increases between 0 and 30% for each species group
data = rbind(biomass_NoCC_GSAprop, biomass_CC_GSAprop)

plot_list <- list()
assump_checks <- data.frame()

for (g in species_groups){
  
  data_species_group = data %>%
    rename(MPA = scenario, species_group = group) %>%
    filter(species_group == g) %>%
    mutate(biomass_change = (tot.biomass - tot.biomass.ref)/tot.biomass.ref * 100) %>%
    dplyr::select(-mpa, -x_scale, -tot.biomass, -tot.biomass.ref) %>%
    mutate(MPA = case_when(grepl("EEZ", MPA) ~ "EEZ-conservation",
                           grepl("Random", MPA) ~ "Random",
                           TRUE ~ MPA)) %>%
    mutate(MPA = as.factor(MPA),
           climate = as.factor(climate)) 
  
  summary(data_species_group)
  str(data_species_group)
  
  # Compute anova - climate, MPA scenarios at 10% coverage
  mpa_coverages = unique(data_species_group$mpa.coverage)
  
  for (i in mpa_coverages){
    data_species_group_cov = data_species_group %>%
      filter(mpa.coverage == i)
    mod = lm(biomass_change ~ climate * MPA, data=data_species_group_cov)
    #anova_result = anova(mod)
    anova_result = car::Anova(mod, type = 2)
    
    print(anova_result)
    summary(mod)
    
    
    # -----------------------
    # Assumption checks
    # -----------------------
    
    # (A) Normality of residuals (Shapiro-Wilk; best for small/moderate n)
    shap_p <- tryCatch(shapiro.test(residuals(mod))$p.value, error = function(e) NA_real_)
    
    # (B) Homoscedasticity: (1) Breusch-Pagan on residuals vs fitted, and
    # (2) Levene across climate×MPA groups (often more interpretable for ANOVA)
    bp_p   <- tryCatch(lmtest::bptest(mod)$p.value, error = function(e) NA_real_)
    
    # Create an interaction group for Levene’s test
    data_species_group_cov <- data_species_group_cov %>%
      mutate(cell = interaction(climate, MPA, drop = TRUE))
    
    lev_p <- tryCatch(car::leveneTest(biomass_change ~ cell, data = data_species_group_cov)[1, "Pr(>F)"],
                      error = function(e) NA_real_)
    
    cat("\n", "Group:", g, "| coverage:", i,
        "| Shapiro p:", signif(shap_p, 3),
        "| BP p:", signif(bp_p, 3),
        "| Levene p:", signif(lev_p, 3), "\n")
    
    # Optional: quick diagnostic plots (uncomment to inspect)
    # par(mfrow=c(1,2))
    # plot(mod, which = 1)  # Residuals vs fitted
    # plot(mod, which = 2)  # Normal Q-Q
    # par(mfrow=c(1,1))
    
    assump_checks <- rbind(assump_checks,
                           data.frame(species_group = g,
                                      mpa_coverage = i,
                                      n = nrow(data_species_group_cov),
                                      shapiro_p = shap_p,
                                      bptest_p = bp_p,
                                      levene_p = lev_p))
    
    SS_term <- anova_result[,"Sum Sq"]
    SS_total <- sum(SS_term)
    pct_total <- 100 * SS_term / SS_total
    print(pct_total)
    
    vars = c("climate", "MPA", "climate:MPA", "non-identified")
    
    rel_var = data.frame(vars, pct_total)
    rel_var$mpa_coverage = i
    
    if (i == mpa_coverages[1]){
      rel_var_all_coverage_levels = rel_var
      
      fill_cols <- c(
        "non-identified" = "grey80",
        "MPA"           = "#009E73",  # bluish green
        "climate"       = "#0072B2",  # blue
        "climate:MPA"   = "#CC79A7"   # reddish purple (reads like a "mix")
      )
      
      legend_plot <- ggplot(rel_var_all_coverage_levels,
                            aes(x = mpa_coverage, y = pct_total, fill = vars)) +
        geom_bar(stat = "identity", position = "stack") +
        labs(x = "MPA coverage (%)", y = "% of total variance", fill = "Source", title = paste0(g)) +
        scale_fill_manual(values = fill_cols, breaks = names(fill_cols)) +
        theme_bw() +
        theme(legend.position = "right",
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 16, face = "bold"))
      
      shared_legend <- cowplot::get_legend(legend_plot)
      
      
    } else{
      rel_var_all_coverage_levels = rbind(rel_var_all_coverage_levels, rel_var)
      
    }
  }
  
  my_order <- c("non-identified", "climate", "climate:MPA", "MPA")
  
  rel_var_all_coverage_levels <- rel_var_all_coverage_levels %>%
    mutate(vars = factor(vars, levels = my_order))
  
  p <- ggplot(rel_var_all_coverage_levels,
         aes(x = mpa_coverage, y = pct_total, fill = vars)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = "MPA coverage (%)", y = "% of total variance", fill = "Source", title = paste0(g)) +
    scale_fill_manual(values = fill_cols, breaks = names(fill_cols)) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "none")
  
  plot_list[[g]] <- p
  
  # to save each plot individually
  #ggsave(here(paste0("figures/anova/pct_total_variance_by_mpa_coverage_", g, ".png")), dpi = 300, height = 5, width = 7)
  
}

plot_list[["legend"]] <- cowplot::plot_grid(shared_legend)

final_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 4)

ggsave(here("figures/anova/pct_total_variance_by_mpa_coverage_all_groups.png"), dpi = 300, height = 14, width = 14)

