
# 24 MPA network configurations are kept separate so well balanced and can use Anova type II. 

# Calculate ANOVA - % of variance explained by CC and MPAs
biomass_NoCC_GSAprop = read.csv(here("data-mpa-CC-paper/model_outputs/Biomass_bygroups_habitatNoCC_GSA-prop_11-07-2023.csv")) %>%
  filter(mpa == "Entire Mediterranean Sea") %>%
  mutate(climate = "Baseline")

biomass_CC_GSAprop = read.csv(here("data-mpa-CC-paper/model_outputs/Biomass_bygroups_habitatCC_2100_GSA-prop_11-07-2023.csv")) %>%
  filter(mpa == "Entire Mediterranean Sea") %>%
  mutate(climate = "Future")


# Figure 5 - MPA coverage increases between 0 and 30% for each species group
data = rbind(biomass_NoCC_GSAprop, biomass_CC_GSAprop)

plot_list <- list()
assump_checks <- data.frame()

species_groups <- c("Large benthic", 
                    "Large demersal", 
                    "Large pelagic",
                    "Medium benthic",
                    "Medium demersal",
                    "Medium pelagic",
                    "Small benthic",
                    "Small demersal",
                    "Small pelagic")

for (g in species_groups){
  data_species_group = data %>%
    rename(MPA = scenario, species_group = group) %>%
    filter(species_group == g) %>%
    mutate(biomass_change = (tot.biomass - tot.biomass.ref)/tot.biomass.ref * 100) %>%
    dplyr::select(-mpa, -x_scale, -tot.biomass, -tot.biomass.ref) %>%
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
    
    rel_var <- data.frame(
      vars = rownames(anova_result),
      pct_total = pct_total,
      mpa_coverage = i
    )
    
    rel_var <- rel_var %>%
      mutate(
        vars = recode(
          vars,
          "Residuals" = "Residual stochastic variation"
        )
      )
    
    if (i == mpa_coverages[1]){
      rel_var_all_coverage_levels = rel_var
      
      fill_cols <- c(
        "Residual stochastic variation" = "grey80",
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
  
  my_order <- c("Residual stochastic variation", "climate", "climate:MPA", "MPA")
  
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
    
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.key.size = unit(1.2, "cm")
  )
  
  plot_list[[g]] <- p
  
  # to save each plot individually
  #ggsave(here(paste0("figures/anova/Fig5_pct_total_variance_by_mpa_coverage_", g, ".png")), dpi = 300, height = 5, width = 7)
  
}

with(data_species_group_cov, table(climate, MPA))

plot_grid_3x3 <- ggpubr::ggarrange(
  plotlist = plot_list,
  ncol = 3,
  nrow = 3,
  common.legend = TRUE,
  legend = "bottom"
)



print(plot_grid_3x3)


ggsave(here("figures/after_revision1/FINAL/Fig5_pct_total_variance_by_mpa_coverage_habitat_groups.png"), dpi = 600, height = 10, width = 10, plot = plot_grid_3x3)

