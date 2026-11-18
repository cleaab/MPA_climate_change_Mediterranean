# Fig 6B - Trophic level of the community weighted by the proportion of biomass of each specie

# import classification of species by groups
vertical_distrib = read.csv(here("data-mpa-CC-paper/classification_species.csv")) %>%
  dplyr::select(species_name, group) %>%
  rownames_to_column("species_id") %>%
  mutate(species_id = as.numeric(species_id) - 1)

vertical_distrib_noamphihaline = vertical_distrib %>%
  filter(! species_id %in% c(0,1,2))

TL_community_current = read.csv(here("data-mpa-CC-paper/model_outputs/TL_community/TL_community_NoCC_GSA-prop_11-07-2023.csv")) %>%
  filter(! species_name %in% c("Alosaalosa", "Alosafallax", "Anguillaanguilla"))

Biomass_prop_current = read.csv(here("data-mpa-CC-paper/model_outputs/Biomass_yield_by_species_NoCC_GSA-prop_11-07-2023.csv")) %>%
  dplyr::select(-yield) %>%
  rename(species_id = sp_id,
         mpa_coverage = mpa.coverage) %>%
  filter(! species_id %in% c(0, 1, 2)) %>%
  left_join(vertical_distrib_noamphihaline, by = "species_id")

TL_community_weighted_current = Biomass_prop_current %>%
  left_join(TL_community_current, by = c("species_name", "mpa_coverage", "scenario"))

TL_community_current_lit = TL_community_weighted_current %>%
  filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
  group_by(mpa_coverage, scenario) %>%
  mutate(total_biomass = sum(biomass, na.rm = T),
         prop_biomass = biomass/total_biomass,
         TL_community_weighted = TL_community * prop_biomass) %>%
  summarise(TL_community_all = sum(TL_community_weighted, na.rm = T))

TL_baseline_nompa = unique(TL_community_current_lit$TL_community_all[TL_community_current_lit$mpa_coverage == 0])


# Future scenario ----

TL_community_future = read.csv(here("data-mpa-CC-paper/model_outputs/TL_community/TL_community_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  filter(! species_name %in% c("Alosaalosa", "Alosafallax", "Anguillaanguilla"))

Biomass_prop_future = read.csv(here("data-mpa-CC-paper/model_outputs/Biomass_yield_by_species_CC_2100_GSA-prop_11-07-2023.csv")) %>%
  dplyr::select(-yield) %>%
  rename(species_id = sp_id,
         mpa_coverage = mpa.coverage) %>%
  filter(! species_id %in% c(0, 1, 2)) %>%
  left_join(vertical_distrib_noamphihaline, by = "species_id")

TL_community_weighted_future = Biomass_prop_future %>%
  left_join(TL_community_future, by = c("species_name", "mpa_coverage", "scenario"))

TLc_Med = TL_community_weighted_future %>%
  filter(mpa_coverage %in% c(0,10)) %>%
  rename(mpa_scenario = scenario) %>%
  group_by(mpa_coverage, mpa_scenario) %>%
  mutate(total_biomass = sum(biomass, na.rm = T),
         prop_biomass = biomass/total_biomass,
         TL_community_weighted = TL_community * prop_biomass) %>%
  summarise(TL_community_all = sum(TL_community_weighted, na.rm = T)) %>%
  mutate(TL_baseline = TL_baseline_nompa) %>%
  mutate(TL_change = (TL_community_all - TL_baseline)/TL_baseline * 100) %>%
  mutate(mpa_scenario_group = case_when(grepl("EEZ", mpa_scenario) ~ "S3-Random EEZ",
                                        grepl("Random", mpa_scenario) ~ "S2-Random basin",
                                        mpa_scenario %in% c("Existing network") ~ "S1-Current",
                                        mpa_scenario %in% c("Micheli") ~ "S4-Micheli",
                                        mpa_scenario %in% c("Mazor scenario 8") ~ "S5-Mazor GFCM",
                                        mpa_scenario %in% c("Mazor scenario 9") ~ "S6-Mazor SAUP",
                                        TRUE ~ mpa_scenario)) %>%
  group_by(mpa_coverage, mpa_scenario_group) %>%
  summarise(TLc_change_mean = mean(TL_change, na.rm = T),
            TLc_change_sd = sd(TL_change, na.rm = T)) %>%
  ungroup() %>%
  mutate(mpa_coverage = case_when(mpa_coverage == 0 ~ "0%",
                                  TRUE ~ "10%")) %>%
  mutate(mpa_scenario_group = case_when(mpa_coverage == "0%" ~ NA,
                                        TRUE ~ mpa_scenario_group)) %>%
  distinct(TLc_change_mean, .keep_all = T) 

# PLOT barplot
mpa_colors <- c("S1-Current" = "gold", 
                "S2-Random basin" = "gray60", 
                "S3-Random EEZ" = "black",
                "S4-Micheli" = "darkorange", 
                "S5-Mazor GFCM" = "darkslateblue", 
                "S6-Mazor SAUP" = "darkorchid")

TLc_Med <- TLc_Med %>%
  mutate(xpos = if_else(mpa_coverage == "0%", 1, 1.6))


# ---- Hatch lines for the 0% bar (drawn in data coordinates) ----
bar0  <- filter(TLc_Med, mpa_coverage == "0%")
hatch <- make_hatch_ref(bar0, value_col = "TLc_change_mean", ylim_range = c(-7, 0))

TL_Med <- ggplot() +
  # 0% bar: light fill + outline, own legend entry via linetype
  geom_col(
    data = bar0,
    aes(x = xpos, y = TLc_change_mean, linetype = "No MPA, RCP8.5"),
    fill = "grey95",
    colour = "grey30",
    linewidth = 0.4,
    width = bar_w,
    key_glyph = draw_key_hatch
  ) +
  # stripes on top of the 0% bar
  geom_segment(
    data = hatch,
    aes(x = x, xend = xend, y = y, yend = yend),
    colour = "grey30",
    linewidth = 0.4,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  # 10% bars: unchanged
  geom_col(
    data = filter(TLc_Med, mpa_coverage == "10%"),
    aes(x = xpos, y = TLc_change_mean, fill = mpa_scenario_group),
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  geom_errorbar(
    data = TLc_Med %>%
      filter(mpa_coverage == "10%") %>%
      mutate(
        ymin = if_else(mpa_scenario_group %in% c("S2-Random basin", "S3-Random EEZ"),
                       TLc_change_mean - TLc_change_sd, NA_real_),
        ymax = if_else(mpa_scenario_group %in% c("S2-Random basin", "S3-Random EEZ"),
                       TLc_change_mean + TLc_change_sd, NA_real_)
      ),
    aes(x = xpos, ymin = ymin, ymax = ymax, group = mpa_scenario_group),
    position = position_dodge(width = 0.7),
    width = 0.2
  ) +
  scale_x_continuous(breaks = c(1, 1.6), labels = c("0%", "10%")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = mpa_colors, name = "MPA scenarios") +
  scale_linetype_manual(values = c("No MPA, RCP8.5" = "solid"), name = NULL) +
  guides(linetype = guide_legend(order = 1),   # No MPA on top
         fill     = guide_legend(order = 2)) + # your scenario legend below
  theme_bw() +
  theme(legend.box = "vertical",
        legend.box.just = "left") +
  labs(y = "Change in TL (%)",
       x = "MPA coverage",
       title = "Trophic level of the community") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0)) +
  ylim(-7, 0)


print(TL_Med)
ggsave(here("figures/after_revision1/FINAL/TLc_barplot_Med.png"), dpi = 600, height = 6, width = 6)
