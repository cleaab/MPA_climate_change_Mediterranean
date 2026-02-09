rm(list=ls())

# Calculate ANOVA - % of variance explained by CC and MPAs
biomass_NoCC_GSAprop = read.csv(here("data-mpa-CC-paper/model_outputs/Biomass_bygroups_NoCC_GSA-prop_11-07-2023.csv")) %>%
  filter(mpa == "Entire Mediterranean Sea") %>%
  mutate(climate = "Baseline")

biomass_CC_GSAprop = read.csv(here("data-mpa-CC-paper/model_outputs/Biomass_bygroups_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  filter(mpa == "Entire Mediterranean Sea") %>%
  mutate(climate = "Future")


# Figure 5 - MPA coverage increases between 0 and 30% for each species group
data = rbind(biomass_NoCC_GSAprop, biomass_CC_GSAprop)

plot_list <- list()
assump_checks <- data.frame()

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
print(final_plot)

ggsave(here("figures/anova/pct_total_variance_by_mpa_coverage_all_groups.png"), dpi = 300, height = 14, width = 14)

